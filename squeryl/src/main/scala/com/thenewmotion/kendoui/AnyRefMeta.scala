package com.thenewmotion.kendoui

import scala.language.implicitConversions
import org.squeryl.{DummyEnum, PrimitiveTypeMode}
import org.squeryl.internals.FieldMetaData
import org.squeryl.dsl.ast.TypedExpressionNode

class AnyRefMeta(ref: AnyRef) extends PrimitiveTypeMode {

  def field(fmd: FieldMetaData): TypedExpressionNode[_] = {
    val ft = fmd.fieldType

    fmd.get(ref)

    if (isInt(ft)) sampleInt
    else if (isLong(ft)) sampleLong
    else if (isString(ft)) sampleString
    else if (isBoolean(ft)) sampleBoolean
    else if (isDouble(ft)) sampleDouble
    else if (isFloat(ft)) sampleFloat
    else if (isDate(ft)) sampleDate
    else if (isBigDecimal(ft)) sampleBigDecimal
    else if (isTimestamp(ft)) sampleTimestamp
    else if (isUuid(ft)) sampleUuid
    else if (isBinary(ft)) sampleBinary
    else if (isEnumerationValueType(ft)) DummyEnum.DummyEnumerationValue
    else throw new UnsupportedOperationException
  }

  def isInt(t: Class[_]) = t.isAssignableFrom(classOf[Int]) || t.isAssignableFrom(classOf[java.lang.Integer])

  def isLong(t: Class[_]) = t.isAssignableFrom(classOf[Long]) || t.isAssignableFrom(classOf[java.lang.Long])

  def isString(t: Class[_]) = t.isAssignableFrom(classOf[java.lang.String])

  def isBoolean(t: Class[_]) = t.isAssignableFrom(classOf[Boolean]) || t.isAssignableFrom(classOf[java.lang.Boolean])

  def isDouble(t: Class[_]) = t.isAssignableFrom(classOf[Double]) || t.isAssignableFrom(classOf[java.lang.Double])

  def isFloat(t: Class[_]) = t.isAssignableFrom(classOf[Float]) || t.isAssignableFrom(classOf[java.lang.Float])

  def isDate(t: Class[_]) = classOf[java.util.Date].isAssignableFrom(t)

  def isBigDecimal(t: Class[_]) = t.isAssignableFrom(classOf[scala.math.BigDecimal]) || t.isAssignableFrom(classOf[java.math.BigDecimal])

  def isTimestamp(t: Class[_]) = classOf[java.sql.Timestamp].isAssignableFrom(t)

  def isBinary(t: Class[_]) = t.isAssignableFrom(classOf[Array[Byte]])

  def isEnumerationValueType(t: Class[_]) = classOf[Enumeration#Value].isAssignableFrom(t)

  def isUuid(t: Class[_]) = t.isAssignableFrom(classOf[java.util.UUID])
}

object AnyRefMeta {
  implicit def anyRefMeta(ref: AnyRef) = new AnyRefMeta(ref)
}