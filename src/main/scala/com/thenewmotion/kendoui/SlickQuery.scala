package com.thenewmotion.kendoui

import scala.slick.ast._
import scala.slick.driver.JdbcProfile
import scala.slick.lifted._
import Operator._

object SlickQuery {
  def apply[T, E <: AbstractTable[T]](q: Query[E, T, Seq])(implicit p: JdbcProfile) =
    new SlickQuery(q)(p)
  private val defaultPage = Page(0, 1000)
}

class SlickQuery[T, E <: AbstractTable[T]] private(q: Query[E, T, Seq])(p: JdbcProfile) {
  import SlickQuery._
  import p.simple._

  private val cache = collection.mutable.Map[String, Node]()

  private def colNode(e: E, field: String) = cache.getOrElseUpdate(field,
    e.getClass.getDeclaredMethod(field).invoke(e)
    .asInstanceOf[Column[_]].toNode
  )

  private def predicate(e: E, f: Filter) = {
    val (c, v) = (colNode(e, f.field), LiteralNode(f.value))
    val L = Library
    def \(fs: FunctionSymbol, nodes: Node*) = fs.typed[Boolean](nodes: _*)
    Column.forNode[Boolean](f.operator match {
      case EqualTo => \(L.==, c, v)
      case NotEqualTo => \(L.Not, \(L.==, c, v))
      case GreaterThen => \(L.>, c, v)
      case GreaterThenOrEqualTo => \(L.>=, c, v)
      case LessThen => \(L.<, c, v)
      case LessThenOrEqualTo => \(L.<=, c, v)
      case StartsWith => \(L.StartsWith, c, v)
      case EndsWith => \(L.EndsWith, c, v)
      case Contains => \(L.Like, c, LiteralNode(s"%${f.value}%"))
      case DoesNotContain => \(L.Not, \(L.Like, c, LiteralNode(s"%${f.value}%")))
    })
  }

  private def ordering(e: E, s: Sorter) = {
    val ordering = s.dir match {
      case Direction.Asc => Ordering(Ordering.Asc)
      case Direction.Desc => Ordering(Ordering.Desc)
    }
    new Ordered(Seq(colNode(e, s.field) -> ordering))
  }

  def page[F, G, T](kq: KendoQuery, countBy: E => F)(implicit shape: Shape[ColumnsShapeLevel, F, T, G], session: Session) = {

    val filtered = kq.filters.foldLeft(q)((acc, f) => acc.filter(predicate(_, f)))
    val sorted = kq.sorter.fold(filtered)(s => filtered.sortBy(ordering(_, s)))

    val count = Query(filtered.map(countBy)(shape).length)
    val data = {
      val p = kq.page getOrElse defaultPage
      sorted.drop(p.skip).take(p.take)
    }
    data.list -> count.first
  }
}
