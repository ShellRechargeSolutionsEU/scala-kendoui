package com.thenewmotion.kendoui

import net.liftweb.http.Req

/**
 * @author Yaroslav Klymko
 */
package object kendoui {
  def params(req: Req, field: String): List[String] = {
    val map = req._params

    def loop(i: Int): List[String] = map.get("models[%d][%s]".format(i, field)) match {
      case Some(xs) => xs ::: loop(i + 1)
      case None => Nil
    }
    loop(0)
  }

  def param(req: Req, field: String): Option[String] =
    req._params.get(field).flatMap(_.headOption)
}
