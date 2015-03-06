package com.thenewmotion.kendoui

import net.liftweb.http.Req
import scala.util.Try
import scalax.StringOption

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
        oe <- Operator(o)
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
  val
    EqualTo, NotEqualTo,
    LessThen, LessThenOrEqualTo,
    GreaterThen, GreaterThenOrEqualTo,
    StartsWith, EndsWith,
    Contains, DoesNotContain = Value

  lazy val aliases = (
    Seq("eq", "==", "isequalto", "equals", "equalto", "equal").map(_ -> EqualTo) ++
    Seq("neq", "!=", "isnotequalto", "notequals", "notequalto", "notequal", "ne").map(_ -> NotEqualTo) ++
    Seq("lt", "<", "islessthan", "lessthan", "less").map(_ -> LessThen) ++
    Seq("lte", "<=", "islessthanorequalto", "lessthanequal", "le").map(_ -> LessThenOrEqualTo) ++
    Seq("gt", ">", "isgreaterthan", "greaterthan", "greater").map(_ -> GreaterThen) ++
    Seq("gte", ">=", "isgreaterthanorequalto", "greaterthanequal", "ge").map(_ -> GreaterThenOrEqualTo) ++
    Seq("startswith").map(_ -> StartsWith) ++
    Seq("endswith").map(_ -> EndsWith) ++
    Seq("contains", "substringof").map(_ -> Contains) ++
    Seq("doesnotcontain").map(_ -> DoesNotContain)
  ).toMap

  def apply(s: String): Option[Value] = aliases.get(s.toLowerCase)
}

object Direction extends Enumeration {
  val Asc = Value("asc")
  val Desc = Value("desc")
}


