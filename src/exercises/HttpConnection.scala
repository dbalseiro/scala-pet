package com.stackbuilders.scalapet
package exercises

import scala.util.{Random, Try}

class HttpConnection(url: String) {
  def get(): String = {
    val random = new Random(System.nanoTime())
    if (random.nextBoolean()) "<html>...</html>"
    else throw new RuntimeException (s"connection to $url lost")
  }
}

object HttpService {
  val random = new Random(System.nanoTime())
  def getConnection(host: String, port: Int): HttpConnection =
    if (random.nextBoolean()) new HttpConnection(s"$host:$port")
    else throw new RuntimeException (s"port $port already in use")
}

object HttpConnectionApp extends App {
  val hostname = "localhost"
  val port = 8080
  def renderHtml: String => Unit = println

  for {
    conn <- Try(HttpService.getConnection(hostname, port))
    page <- Try(conn.get())
  } yield renderHtml(page)

}