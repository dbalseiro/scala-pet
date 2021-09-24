package com.stackbuilders.scalapet
package exercises

import scala.collection.IterableOnce.iterableOnceExtensionMethods

object SocialNetwork extends App {
  type Person = String
  type Network = Map[Person, List[Person]]

  def add (person: Person, network: Network): Network = network + (person -> List.empty)

  def remove (person: Person, network: Network): Network =
    network
      .map(p => p._1 -> p._2.filter(_ != person))
      .view.filterKeys(_ != person).toMap

  def friend (person: Person, other: Person, network: Network): Network = {
    def befriend (relation:(Person, List[Person]), amicus: Person): (Person, List[Person]) =
      relation._1 -> (relation._2 :+ amicus)

    network.map(p =>
      if (p._1 == person) befriend(p, other)
      else if (p._1 == other) befriend (p, person)
      else p
    )
  }

  def unfriend (person: Person, other: Person, network: Network): Network = {
    def defriend(relation: (Person, List[Person]), enemicus: Person) =
      relation._1 -> relation._2.filter(_ != enemicus)

    network.map(p =>
      if (p._1 == person) defriend(p, other)
      else if (p._1 == other) defriend (p, person)
      else p
    )
  }

  def existsConnection(person: Person, other: Person, network: Network): Boolean = {
    try {
      val bffs = network(person)
      bffs.contains(other) || bffs.map(existsConnection(person, _, network)).exists(identity)
    }
    catch {
      case _: Throwable => false
    }
  }

  val network =
    friend("Lolo", "Pepe"
    , add ("Pepe"
    , unfriend("Lolo", "Arturo"
    , friend("Lolo", "Arturo"
    , friend("Lolo", "Diego"
    , add ("Lolo"
    , add ("Arturo"
    , remove("Juan"
    , add("Juan"
    , add("Diego"
    , Map.empty
    ))))))))))

  println(network)

  println (network.view.mapValues(_.length).toMap)
  println (network.maxBy(p => p._2.length)._1)
  println (network.filter(_._2.isEmpty).keys.toList.length)

  println(existsConnection("Lolo", "Pepe", network))
  println(existsConnection("Diego", "Pepe", network))
}
