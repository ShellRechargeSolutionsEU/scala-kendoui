package com.thenewmotion.kendoui

import scala.slick.ast.{Node, Library, Ordering}
import scala.slick.lifted.{NothingContainer, Shape, ColumnOrdered, AbstractTable}
import scala.slick.lifted.TypeMapper._
import scala.slick.driver.ExtendedDriver
import com.typesafe.scalalogging.slf4j.Logging
import net.liftweb.http.Req
import scala.reflect.ClassTag

/**
 * @author Yaroslav Klymko
 */
object SlickQuery {
  def apply[AT <: AbstractTable[_]](table: AT, req: Req)(implicit driver: ExtendedDriver): SlickQuery[AT] =
    new SlickQuery(table, KendoQuery(req))
}

class SlickQuery[AT <: AbstractTable[_]](table: AT, kq: KendoQuery)(implicit driver: ExtendedDriver) extends Logging {

  import driver.simple._
  import ConstColumn.TRUE

  private def tableColumn(field: String, table: AT): Option[Column[_]] =
    try Some(table.getClass.getMethod(field).invoke(table).asInstanceOf[Column[_]]) catch {
      case e: Exception =>
        logger.warn(s"no column ${table.tableName}.$field")
        None
    }

  def query(defaultSort: AT => ColumnOrdered[_]) = table.where(filter).sortBy(sort(defaultSort))

  def filter(table: AT) = {
    def filter(f: Filter): Option[Column[Boolean]] = tableColumn(f.field, table).map {
      column =>
        val n = Node(column)

        val value = if (column.tpe == BooleanTypeMapper) {
          if (f.value == "true") "1"
          else "0"
        } else {
          f.value
        }

        def v = ConstColumn(value)

        import Operator._
        f.operator match {
          case EqualTo => Library.==.column(n, Node(v))
          case NotEqualTo => Library.Not.column(Library.==.typed[Boolean](n, Node(v)))
          case LessThen => Library.<.column(n, Node(v))
          case LessThenOrEqualTo => Library.<=.column(n, Node(v))
          case GreaterThen => Library.>.column(n, Node(v))
          case GreaterThenOrEqualTo => Library.>=.column(n, Node(v))
          case StartsWith => Library.Like.column(n, Node(ConstColumn(s"$value%")))
          case EndsWith => Library.Like.column(n, Node(ConstColumn(s"%$value")))
          case Contains => Library.Like.column(n, Node(ConstColumn(s"%$value%")))
          case DoesNotContain => Library.Not.column(Library.Like.typed[Boolean](n, Node(ConstColumn(s"%$value%"))))
        }
    }
    kq.filters.flatMap(filter).foldLeft(TRUE === TRUE)((x, y) => Library.And.column(Node(x), Node(y)))
  }

  def sort(default: AT => ColumnOrdered[_])(table: AT): ColumnOrdered[_] = {
    def sortDir(x: Direction.Value) = x match {
      case Direction.Asc => Ordering.Asc
      case Direction.Desc => Ordering.Desc
    }
    val sorter = for {
      Sorter(field, dir) <- kq.sorter
      column <- tableColumn(field, table)
    } yield ColumnOrdered(column, Ordering(direction = sortDir(dir)))
    sorter getOrElse default(table)
  }

  def dataAndCount[F, G, T](orderBy: AT => ColumnOrdered[_], countBy: AT => F)(implicit shape: Shape[F, T, G]) =
    dataAndCountSorted(orderBy, countBy, table.where(filter))

  def dataAndCount[F, G, T](orderBy: AT => ColumnOrdered[_],
                            countBy: AT => F,
                            where: AT => Column[Option[Boolean]])(implicit shape: Shape[F, T, G]) =
    dataAndCountSorted(orderBy, countBy, table.where(x => where(x) && filter(x)))

  private def dataAndCountSorted[F, G, T](orderBy: AT => ColumnOrdered[_],
                                          countBy: AT => F,
                                          filtered: Query[AT, NothingContainer#TableNothing])(implicit shape: Shape[F, T, G]) = {
    val sorted = filtered.sortBy(sort(orderBy))
    val countQuery = Query(filtered.map(countBy).length)
    val dataQuery = kq.page.fold(sorted.take(1000)) {
      case Page(s, t) => sorted.drop(s).take(t)
    }
    dataQuery -> countQuery
  }
}