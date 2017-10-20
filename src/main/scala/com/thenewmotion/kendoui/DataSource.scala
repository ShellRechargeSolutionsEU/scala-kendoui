package com.thenewmotion.kendoui

import java.sql.Timestamp
import java.time.{ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME

import net.liftweb.json.{DefaultFormats, Extraction, Formats, Serializer}
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JString
import net.liftweb.json.JsonDSL._

object DataSource {

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
  } + new Serializer[ZonedDateTime] {
    def deserialize(implicit format: Formats) = {
      case (_, JString(dt)) => ZonedDateTime.parse(dt, ISO_OFFSET_DATE_TIME)
    }

    def serialize(implicit format: Formats) = {
      case dt: ZonedDateTime => dt.withZoneSameInstant(ZoneId.of("Z")).format(ISO_OFFSET_DATE_TIME)
    }
  } + new Serializer[Timestamp] {
    def deserialize(implicit format: Formats) = {
      case (_, JString(dt)) => Timestamp.from(ZonedDateTime.parse(dt, ISO_OFFSET_DATE_TIME).toInstant)
    }

    def serialize(implicit format: Formats) = {
      case dt: Timestamp => ZonedDateTime.ofInstant(dt.toInstant, ZoneId.of("Z")).format(ISO_OFFSET_DATE_TIME)
    }
  }

  def apply(total: Long, objs: List[_], formats: Formats): JObject =
    ("total", total) ~("results", JArray(objs.map((x: Any) => Extraction.decompose(x)(formats))))

  def apply(total: Long, objs: List[_]): JObject = apply(total, objs, formats)

  def apply(objs: List[_]): JObject = apply(objs.length, objs)
}
