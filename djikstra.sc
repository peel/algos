object d {
  //iterate on costs and find the lowest cost node
  def findLowestCostNode(costs: Map[String, Int]): Option[(String, Int)] =
    costs.foldRight(None: Option[(String, Int)]) { (curr, acc) =>
      (curr, acc) match {
        case ((node, cost), Some((accNode, accCost))) if (cost < accCost) =>
          Some((node, cost))
        case (_, None) => Some(curr)
        case _ => acc
      }
    }

  def djikstra(graph: Map[String, Map[String, Int]], start: String, stop: String) = {
    def apply(costs: Map[String, Int],
              parents: Map[String, String],
              processed: Set[String]): Map[String, String] =
      //step 1: find the lowest cost node
      findLowestCostNode(costs) match {
        //step 2: if node is not yet processed
        case Some((node, cost)) if !processed.contains(node) =>
          //step 3: count node's neighbours costs
          val newCost: Option[Map[String, Int]] = graph.get(node).map(_.map {
              //step 4: if neighbour's cost is lower, update costs
              case (n, c) if c + cost < costs.get(node).getOrElse(0) =>
                (n, c + cost)
              case v => v
            })
          //step 5: recurse using updated costs and extended parents
          apply(newCost.getOrElse(costs),
                Map(parents.values.head -> node) ++ parents,
                processed + node)
        case _ =>
          println(s"Shortest path is: $parents")
          parents
      }

    //step 0: initialize with cost of start and stop and parents: nothing to start
    val costsOpt = for { s <- graph.get(start); f <- graph.get(stop) } yield s ++ f
    val parents: Map[String, String] = Map[String, String]("" -> start)
    apply(costsOpt.getOrElse(Map[String, Int]()), parents, Set())
  }

}

val graph: Map[String, Map[String, Int]] = Map(
  "Start" -> Map("A" -> 6, "B" -> 2),
  "A" -> Map("Fin" -> 1),
  "B" -> Map("A" -> 3, "Fin" -> 5),
  "Fin" -> Map())

d.djikstra(graph, "Start", "Fin")
