object d{
  def findLowestCostNode(costs: Map[String, Int]): Option[(String, Int)] =
    costs.foldRight(None: Option[(String, Int)]) { (curr, acc) =>
      (curr, acc) match {
        case ((node, cost), Some((accNode, accCost))) if (cost < accCost) =>
          Some((node, cost))
        case (_, None) => Some(curr)
        case _ => acc
      }
    }

  def djikstra(graph: Map[String, Map[String, Int]]) = {
    def apply(costs: Map[String, Int],
              parents: Map[String, String],
              processed: Set[String]): Map[String, String] =
      findLowestCostNode(costs) match {
        case Some((node, cost)) if !processed.contains(node) =>
          val neighbours: Option[Map[String, Int]] = graph.get(node)
          val newCost: Option[Map[String, Int]] = neighbours map (vs => vs.map {
            case (n, c) if c + cost < costs.get(node).getOrElse(0) =>
              (n, c + cost)
            case v => v
                                                                  })
          println(s"Parents is $parents")
          val newParents: Map[String, String] = Map(parents.values.head -> node) ++ parents
          println(s"New parents: $newParents")
          apply(newCost.getOrElse(costs), newParents, processed + node)
        case Some((node, cost)) =>
          println(s"Shortest path is: $parents")
          parents
        case None =>
          println(s"Shortest path is: $parents")
          parents
      }

    val costsOpt = for { s <- graph.get("Start"); f <- graph.get("Fin") } yield
      s ++ f
    val parents: Map[String, String] = Map[String, String]("" -> "Start")
    apply(costsOpt.getOrElse(Map[String, Int]()),
          parents,
          Set())
  }

  //graph definition
  // we're going from "start" to "fin"
  val graph: Map[String, Map[String, Int]] = Map(
    "Start" -> Map("A" -> 6, "B" -> 2),
    "A" -> Map("Fin" -> 1),
    "B" -> Map("A" -> 3, "Fin" -> 5),
    "Fin" -> Map())

}

d.djikstra(d.graph)

