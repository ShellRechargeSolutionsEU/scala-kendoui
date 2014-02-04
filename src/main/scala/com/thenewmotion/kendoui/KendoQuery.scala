package com.thenewmotion.kendoui

import net.liftweb.http.Req
import scala.util.Try
import scalax.StringOption

/**
 * @author Yaroslav Klymko
 */
object KendoQuery {
  def apply(req: Req): KendoQuery = apply(req.params.collect {
    case (k, List(v)) => k -> v
  })

  def apply(params: Map[String, String]): KendoQuery = {
    def param(x: String): Option[String] = params.get(x).flatMap(StringOption(_))

    def filters(i: Int): List[Filter] = {
      val filter = for {
        f <- param(s"filter[filters][$i][field]")
        v <- param(s"filter[filters][$i][value]")
        o <- param(s"filter[filters][$i][operator]")
        oe <- Operator.parse(o)
      } yield Filter(f, v, oe)

      filter match {
        case Some(x) => x :: filters(i + 1)
        case None => Nil
      }
    }

    def sorters(i: Int): List[Sorter] = {
      val sorter = for {
        f <- param(s"sort[$i][field]")
        d <- param(s"sort[$i][dir]")
        dir <- Direction.values.find(_.toString equalsIgnoreCase d)
      } yield Sorter(f, dir)

      sorter match {
        case Some(x) => x :: sorters(i + 1)
        case None => Nil
      }
    }

    def page = {
      def intParam(x: String) = param(x).flatMap(x => Try(x.toInt).toOption)
      for {
        skip <- intParam("skip")
        take <- intParam("take")
      } yield Page(skip, take)
    }

    KendoQuery(filters(0), sorters(0).headOption, page)
  }
}

case class KendoQuery(filters: List[Filter] = Nil, sorter: Option[Sorter] = None, page: Option[Page] = None)

case class Filter(field: String, value: String, operator: Operator.Value)

case class Sorter(field: String, dir: Direction.Value)

case class Page(skip: Int, take: Int)

object Operator extends Enumeration {
  val EqualTo, NotEqualTo, LessThen, LessThenOrEqualTo, GreaterThen, GreaterThenOrEqualTo, StartsWith, EndsWith, Contains, DoesNotContain = Value

  lazy val aliases = Map(
    EqualTo -> List("eq", "==", "isequalto", "equals", "equalto", "equal"),
    NotEqualTo -> List("neq", "!=", "isnotequalto", "notequals", "notequalto", "notequal", "ne"),
    LessThen -> List("lt", "<", "islessthan", "lessthan", "less"),
    LessThenOrEqualTo -> List("lte", "<=", "islessthanorequalto", "lessthanequal", "le"),
    GreaterThen -> List("gt", ">", "isgreaterthan", "greaterthan", "greater"),
    GreaterThenOrEqualTo -> List("gte", ">=", "isgreaterthanorequalto", "greaterthanequal", "ge"),
    StartsWith -> List("startswith"),
    EndsWith -> List("endswith"),
    Contains -> List("contains", "substringof"),
    DoesNotContain -> List("doesnotcontain"))

  def parse(s: String): Option[Value] = aliases.collectFirst {
    case (o, xs) if xs.contains(s.toLowerCase) => o
  }
}

object Direction extends Enumeration {
  val Asc = Value("asc")
  val Desc = Value("desc")
}


