package com.stackbuilders.scalapet
package exercises

import scala.util.Random

class Connection (connString: String) {
  def connect = s"Connected to $connString"
}

object Connection {
  val random = new Random(System.nanoTime())
  def apply(host: String, port: String): Option[Connection] =
    if (random.nextBoolean()) Some(new Connection(s"$host:$port"))
    else None
}

object ConnectionApp extends App {
  val config: Map[String, String] = Map (
    "host" -> "173.174.1.0",
    "port" -> "8080"
  )

  def establishConnection(): Unit = {
    for {
      host <- config.get("host")
      port <- config.get("port")
      conn <- Connection(host, port)
    }
    yield println(conn.connect)
  }

  establishConnection()
}
