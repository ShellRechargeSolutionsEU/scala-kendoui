package com.thenewmotion.kendoui

import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.ast.LogicalBoolean
import com.thenewmotion.kendoui.AnyRefMeta._
import com.thenewmotion.kendoui.Operator._

/**
 * An extension of QueryFromRequest that offers some support for string filters against comma-separated lists.
 *
 * If a field is marked as a comma-separated field by passing it in the appropriate constructor argument, any filter
 * with the Contains operator on that field will only return rows where the filter value actually occurs as one of the
 * elements of the elements of the comma-separated list.
 *
 * As an example: when using plain QueryFromRequest, filtering all rows whose "tags" field contains "aa" will return
 * a row where the "tags" field contains ",schaap,geit,". When using QueryFromRequestWithCommaSeparatedFields
 * and passing Set("tags") to its constructor, a filter of rows whose "tags" field contains "aa" will NOT return
 * this row. If you filter for "schaap" however, the row with ",schaap,geit," as it tags will be returned.
 */
class QueryFromRequestWithCommaSeparatedFields[T <: AnyRef](view: View[T], kq: KendoQuery,
                                                            commaSeparatedFields: Set[String])
  extends QueryFromRequest[T](view, kq) {

  // We need to surround value of permissions filter with commas.
  // So we override this filter function.
  override def filter(t: T): LogicalBoolean = ((0 === 0: LogicalBoolean) /: filterContext) {
    case (expr, (fmd, filter)) =>
      val newValue = modifyFilterValueIfCommaSeparatedField(filter)
      expr.and(logicalBoolean(t.field(fmd), newValue, filter.operator))
  }

  private[kendoui] def modifyFilterValueIfCommaSeparatedField(filter: Filter): String =
    if (commaSeparatedFields.contains(filter.field) && filter.operator == Contains) modifyFilterValue(filter.value)
    else filter.value

  private def modifyFilterValue(filterValue: String) = "," + filterValue + ","
}
