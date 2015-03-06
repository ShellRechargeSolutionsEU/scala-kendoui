package com.thenewmotion.kendoui

import com.typesafe.scalalogging.slf4j.LazyLogging

import scala.language.implicitConversions
import org.squeryl._
import org.squeryl.dsl.ast._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl._
import org.squeryl.internals.FieldMetaData
import com.thenewmotion.time.Imports._
import org.joda.time.format.ISODateTimeFormat
import com.thenewmotion.kendoui.AnyRefMeta._
import com.thenewmotion.kendoui.Operator._

class QueryFromRequest[T <: AnyRef](view: View[T], kq: KendoQuery) extends LazyLogging {

  object MetaData {
    def unapply(field: String): Option[FieldMetaData] =
      view.posoMetaData.findFieldMetaDataForProperty(field)
  }

  def fieldMetaData(field: String): Option[FieldMetaData] =
    view.posoMetaData.findFieldMetaDataForProperty(field)

  lazy val sortContext: Option[(FieldMetaData, Direction.Value)] = kq.sorter.map {
    case Sorter(MetaData(meta), dir) => meta -> dir
  }

  lazy val filterContext: List[(FieldMetaData, Filter)] = kq.filters.collect {
    case filter@Filter(MetaData(meta), _, _) => meta -> filter
  }

  def logicalBoolean(t: TypedExpressionNode[_], value: String, operator: Operator.Value): LogicalBoolean = {

    def dateOrNumerical = {

      def createTimestampFromString(dt: String) = {
        // ISO 8601 UTC+Offset: [YYYY]-[MM]-[DD]T[hh]:[mm]:[ss]±[hh]:[mm]
        val formatter = ISODateTimeFormat.dateTimeNoMillis
        formatter.parseDateTime(dt).toTimestamp
      }

      val valueExpression: Option[TypedExpressionNode[_]] = PartialFunction.condOpt(t) {
        case _: DateExpression[_] => createTimestampFromString(value): DateExpression[_]
        case _: NumericalExpression[_] => value.toInt: NumericalExpression[_]
        case _: BooleanExpression[_] => value.toBoolean: BooleanExpression[_]
      }

      def notImplemented = {
        logger.warn("Filtering is not implemented for %s and %s".format(t, operator))
        0 === 0
      }

      valueExpression.map(x => operator match {
        case GreaterThen => new BinaryOperatorNodeLogicalBoolean(t, x, ">")
        case GreaterThenOrEqualTo => new BinaryOperatorNodeLogicalBoolean(t, x, ">=")
        case LessThen => new BinaryOperatorNodeLogicalBoolean(t, x, "<")
        case LessThenOrEqualTo => new BinaryOperatorNodeLogicalBoolean(t, x, "<=")
        case EqualTo => new EqualityExpression(t, x)
        case NotEqualTo => not(new EqualityExpression(t, x))
        case _ => notImplemented
      }) getOrElse notImplemented
    }

    (t -> operator) match {
      case (str: StringExpression[_], Contains) => str like s"%$value%"
      case (str: StringExpression[_], DoesNotContain) => not(str like s"%$value%")
      case (str: StringExpression[_], StartsWith) => str like s"$value%"
      case (str: StringExpression[_], EndsWith) => str like s"%$value"
      case (bool: BooleanExpression[_], EqualTo) => new EqualityExpression(bool, value.toBoolean)
      case (bool: BooleanExpression[_], NotEqualTo) => not(new EqualityExpression(bool, value.toBoolean))
      case (str: NonNumericalExpression[_], EqualTo) => new EqualityExpression(str, value)
      case (str: NonNumericalExpression[_], NotEqualTo) => not(new EqualityExpression(str, value))
      case _ => dateOrNumerical
    }
  }

  def filter(t: T): LogicalBoolean = ((0 === 0: LogicalBoolean) /: filterContext) {
    case (expr, (fmd, filter)) => expr.and(logicalBoolean(t.field(fmd), filter.value, filter.operator))
  }

  def sort(t: T): Option[ExpressionNode] = sortContext.map {
    case (meta, dir) =>
      def field = t.field(meta)
      dir match {
        case Direction.Asc => field.asc
        case _ => field.desc
      }
  }

  implicit def richQuery[A](query: Query[A]): RichQuery[A] = new RichQuery(query)

  class RichQuery[A](query: Query[A]) {
    def paginate(): Query[A] = kq.page match {
      case Some(Page(skip, take)) => query.page(skip, take)
      case None => query
    }
  }

}