object d {
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
      findLowestCostNode(costs) match {
        case Some((node, cost)) if !processed.contains(node) =>
          val newCost: Option[Map[String, Int]] = graph.get(node) map (vs =>
            vs.map {
              case (n, c) if c + cost < costs.get(node).getOrElse(0) =>
                (n, c + cost)
              case v => v
            })
          apply(newCost.getOrElse(costs),
                Map(parents.values.head -> node) ++ parents,
                processed + node)
        case _ =>
          println(s"Shortest path is: $parents")
          parents
      }

    val costsOpt = for { s <- graph.get(start); f <- graph.get(stop) } yield
      s ++ f
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
