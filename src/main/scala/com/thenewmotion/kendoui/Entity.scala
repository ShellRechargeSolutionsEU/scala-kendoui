package com.thenewmotion.kendoui

import java.lang.reflect.Field
import org.squeryl.annotations.Transient

/**
 * @author Yaroslav Klymko
 */
trait HasValues {
  protected def values: Seq[Any]
}

trait EquateValues extends Equals with HasValues {

  override def hashCode() = values.hashCode()

  override def equals(a: Any) = a match {
    case that: EquateValues if this.canEqual(that) && that.canEqual(this) =>
      this.values == that.values
    case _ => false
  }

  def canEqual(that: Any): Boolean = that.getClass.isAssignableFrom(getClass)
}

trait ValuesToString {
  self: HasValues =>

  override def toString = getClass.getSimpleName + "(" + valuesToString + ")"

  protected def valuesToString: String = {
    val fs: Seq[(String, Any)] = _fields.map(field => field.getName -> field.get(this))
    val unique: Seq[(String, Any)] = fs.groupBy(_._2).collect {
      case (_, Seq(x)) => x
    }.toList

    fs.filter(unique.contains(_)).toSeq.map {
      case (name, Some(value)) => name + "=" + value
      case (name, value) => name + "=" + value
    }.mkString(", ")
  }

  @Transient
  private lazy val _fields: Seq[Field] = {
    def get(clazz: Class[_]): List[Field] = if (clazz != classOf[Object]) {
      get(clazz.getSuperclass) ::: clazz.getDeclaredFields.toList
    } else Nil

    val fs: List[(Field, Any)] = get(getClass).map {
      f =>
        f.setAccessible(true)
        f -> f.get(this)
    }
    values flatMap (value => fs.find(_._2 == value).map(_._1))
  }
}

trait Entity extends HasValues with EquateValues with ValuesToString

//works faster - evaluates once
//for case classes only
trait CaseEntity extends Entity {
  private lazy val _valuesToString = super.valuesToString

  override protected def valuesToString = _valuesToString

  private lazy val _hashCode = super.hashCode()

  override def hashCode() = _hashCode

  protected def caseValues: Seq[Any]

  private lazy val _values = caseValues

  protected def values = _values
}