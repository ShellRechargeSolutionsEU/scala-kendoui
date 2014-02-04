package com.thenewmotion.kendoui

import com.thenewmotion.time.Imports._
import java.sql.Timestamp
import net.liftweb.json.{Extraction, Formats, Serializer, DefaultFormats}
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonDSL._

/**
 * @author Yaroslav Klymko
 */
object DataSource {

  val JsDateFormatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")

  lazy val formats = DefaultFormats + new Serializer[Option[String]] {
    def deserialize(implicit format: Formats) = {
      case (_, JNothing) => None
      case (_, JString("")) => None
      case (_, JString(s)) => Some(s)
    }

    def serialize(implicit format: Formats) = {
      case Some(s: String) => JString(s)
      case None => JNull
    }
  } + new Serializer[DateTime] {
    def deserialize(implicit format: Formats) = {
      case (_, JString(dt)) => JsDateFormatter.parseDateTime(dt)
    }

    def serialize(implicit format: Formats) = {
      case dt: DateTime => dt.withZone(DateTimeZone.UTC).toString(JsDateFormatter)
    }
  } + new Serializer[Timestamp] {
    def deserialize(implicit format: Formats) = {
      case (_, JString(dt)) => JsDateFormatter.parseDateTime(dt).toTimestamp
    }

    def serialize(implicit format: Formats) = {
      case dt: Timestamp => dt.toDateTime.withZone(DateTimeZone.UTC).toString(JsDateFormatter)
    }
  }

  def apply(total: Long, objs: List[_], formats: Formats): JObject =
    ("total", total) ~("results", JArray(objs.map((x: Any) => Extraction.decompose(x)(formats))))

  def apply(total: Long, objs: List[_]): JObject = apply(total, objs, formats)

  def apply(objs: List[_]): JObject = apply(objs.length, objs)
}