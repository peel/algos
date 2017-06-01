import scala.annotation.tailrec
object g{
  type Station = String
  type State = String

  def cover(stations: Map[Station, Set[State]], statesNeeded: Set[State]) = {
    @tailrec
    def apply(stations: Map[Station, Set[State]],
              uncovered: Set[State],
              acc: Set[Station]): Set[Station] =
      if (!uncovered.isEmpty) {
        val (station, states) = best(stations,uncovered) //new best station
        apply(stations.dropWhile(_._1 == station), // drop station from list
              uncovered -- states, // update covered states
              acc + station) // accumulate picked stations
      } else {
        println(acc)
        acc //done
      }

    // step 0: from all stations find
    // a subset of stations that cover all the needs
    apply(stations,statesNeeded,Set())
  }

  //find the biggest covered set
  def best(stations: Map[Station, Set[State]],
           needed: Set[State]): (Station, Set[State]) =
    stations.map { case (n, s) => (n, s intersect needed) }.maxBy(_._2.size)

}

import g._
val stations: Map[Station, Set[State]] = Map(
  "kone" -> Set("id", "nv", "ut"),
  "ktwo" -> Set("wa", "id", "mt"),
  "kthree" -> Set("or", "nv", "ca"),
  "kfour" -> Set("nv", "ut"),
  "kfive" -> Set("ca", "az"))

val statesNeeded: Set[State] =
  Set("mt", "wa", "or", "id", "nv", "ut", "ca", "az")

g.cover(stations,statesNeeded)
