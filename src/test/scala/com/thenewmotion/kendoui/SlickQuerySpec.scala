package com.thenewmotion.kendoui

import java.sql.Timestamp

import org.joda.time.DateTime
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import slick.driver.H2Driver

import SlickQuery._

class SlickQuerySpec extends SpecificationWithJUnit {
  val driver = H2Driver
  import driver.simple._

  implicit val dtMapper = MappedColumnType.base[DateTime, Timestamp](
    dt => new Timestamp(dt.getMillis),
    ts => new DateTime(ts.getTime)
  )

  case class Tst(
    id: Long, kind: String, name: String,
    last: Option[String], prev: Option[String], at: DateTime
  )

  class Test(tag: Tag) extends Table[Tst](tag, "test") {
    val id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    val kind = column[String]("kind")
    val name = column[String]("name")
    val lastRes = column[Option[String]]("last_res")
    val prevRes = column[Option[String]]("prev_res")
    val at    = column[DateTime]("at")
    def * = (id, kind, name, lastRes, prevRes, at) <> (Tst.tupled, Tst.unapply)
  }

  val tests = TableQuery[Test]
  val db = Database.forURL(
    "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1;DATABASE_TO_UPPER=FALSE;MVCC=TRUE",
    driver = "org.h2.Driver")
  object dt {
    val now = DateTime.now
    val `-5min` = now.minusMinutes(5)
    val `-10min` = now.minusMinutes(10)
    val `-15min` = now.minusMinutes(15)
    val `-20min` = now.minusMinutes(20)
  }
  val original = Seq(
    Tst(1, "Spec", "aaa", Some("ok"), None, dt.now),
    Tst(2, "Spec", "abc", Some("ko"), Some("ok"), dt.`-5min`),
    Tst(3, "Rest", "bcc", Some("ko"), Some("ko"), dt.`-10min`),
    Tst(4, "Rest", "ccd", Some("ok"), Some("ok"), dt.`-15min`),
    Tst(5, "Rest", "def", Some("ko"), None, dt.`-20min`)
  )
  db withTransaction { implicit s =>
    tests.ddl.create

    tests ++= original
  }

  trait Case extends Scope {
    def withTransaction[T](f: Session => T) = {
      db withTransaction { implicit s =>
        val res = f(s)
        s.conn.rollback()
        res
      }
    }
  }

  "paged" >> new Case { withTransaction { implicit s =>
    val page = Page(2, 1)
    val kq = KendoQuery(page = Some(page))

    val (data, count) = tests.sortBy(_.id).page(kq, _.id)
    data mustEqual original.drop(page.skip).take(page.take)
    count mustEqual original.length
  }}

  "defauls to sort by 'countBy' " >> new Case {
    db withSession { implicit s =>
      val (data, count) = tests.page(KendoQuery(), _.id)
      data mustEqual original
      count mustEqual original.length
    }
  }

  "sort by last pass status" >> new Case {
    db withSession { implicit s =>
      val kq = KendoQuery(sorter = Some(Sorter("lastRes", Direction.Asc)))

      val (data, count) = tests.page(kq, _.id)
      data.map(_.last) mustEqual original.sortBy { t =>
        t.last.fold("")(identity)
      }.map(_.last)
      count mustEqual original.length
    }
  }

  trait OpCase extends Scope {
    def fixture: (KendoQuery, Tst => Boolean)

    db withSession { implicit s =>
      val (kendoQuery, memQuery) = fixture
      val actual = tests.sortBy(_.id).page(kendoQuery, _.id)
      val expected = {
        val exp = original.filter(memQuery)
        (exp, exp.length)
      }
      (actual._1 aka "values") mustEqual expected._1
      (actual._2 aka "total") mustEqual expected._2
      s.conn.rollback()
      ok
    }
  }

  "all" >> new OpCase {
    def fixture = (
      KendoQuery(),
      (_: Tst) => true
    )
  }

  "specs only" >> new OpCase {
    def fixture = (
      KendoQuery(
        filters = List(Filter("kind", "Spec", Operator.EqualTo))
      ),
      _.kind == "Spec"
    )
  }
  "not specs" >> new OpCase {
    def fixture = (
      KendoQuery(
        filters = List(Filter("kind", "Spec", Operator.NotEqualTo))
      ),
      _.kind != "Spec"
    )
  }

  "after a moment strictly" >> new OpCase {
    def fixture = (
      KendoQuery(
        filters = List(Filter("at", dt.`-15min`.toString, Operator.GreaterThen))
      ),
      _.at isAfter dt.`-15min`
    )
  }

  "after or at a moment" >> new OpCase { def fixture = (
    KendoQuery(
      filters = List(Filter("at", dt.`-15min`.toString, Operator.GreaterThenOrEqualTo))
    ),
    t => !(t.at isBefore dt.`-15min`)
  )}

  "before a moment" >> new OpCase { def fixture = (
    KendoQuery(
      filters = List(Filter("at", dt.`-15min`.toString, Operator.LessThen))),
    _.at isBefore dt.`-15min`
  )}

  "before or at a moment" >> new OpCase { def fixture = (
    KendoQuery(
      filters = List(Filter("at", dt.`-15min`.toString, Operator.LessThenOrEqualTo))),
    t => !(t.at isAfter dt.`-15min`)
  )}

  "name contains 'b'">> new OpCase { def fixture = (
    KendoQuery(
      filters = List(Filter("name", "b", Operator.Contains))),
    _.name contains "b"
  )}

  "name doesn't contain 'c'" >> new OpCase { def fixture = (
    KendoQuery(
      filters = List(Filter("name", "c", Operator.DoesNotContain))),
    _.name.indexOf("c") < 0
  )}

  "name starts with 'a'" >> new OpCase { def fixture = (
    KendoQuery(
      filters = List(Filter("name", "a", Operator.StartsWith))),
    _.name startsWith "a"
  )}

  "name ends with 'c'" >> new OpCase { def fixture = (
    KendoQuery(
      filters = List(Filter("name", "c", Operator.EndsWith))),
    _.name endsWith "c"
  )}

  "last passed specs" >> new OpCase { def fixture = (
    KendoQuery(
      filters = List(
        Filter("kind", "Spec", Operator.EqualTo),
        Filter("lastRes", "ok", Operator.EqualTo))),
    (t: Tst) => t.kind == "Spec" && t.last.exists(_ == "ok")
  )}

  "with 'a' in name and passed within 10 minutes" >> new OpCase { def fixture = (
    KendoQuery(
      filters = List(
        Filter("name", "a", Operator.Contains),
        Filter("lastRes", "ok", Operator.EqualTo),
        Filter("at", dt.`-10min`.toString, Operator.GreaterThen))),
    (t: Tst) =>
      t.name.contains("a") && t.last.exists(_ == "ok") && t.at.isAfter(dt.`-10min`)
  )}

}
