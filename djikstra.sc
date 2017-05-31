def findLowestCostNode(costs: Map[String, Int]): Option[(String,Int)] =
  costs.foldRight(None: Option[(String,Int)]){(curr,acc) => 
    (curr, acc) match {
      case ((node,cost),Some((accNode,accCost))) if (cost < accCost) => Some((node,cost))
      case (_, None) => Some(curr)
      case _ => acc 
    }
  }

def djikstra(graph: Map[String, Map[String,Int]]) = {  
  def apply(costs: Map[String,Int], parents: Map[String,Set[String]], processed: Set[String]): Map[String,Set[String]] =
    findLowestCostNode(costs) match {
      case Some((node, cost)) if !processed.contains(node) =>
        println(processed)
        val neighbours: Option[Map[String, Int]] = graph.get(node)
        val newCost: Option[Map[String, Int]] = neighbours.map(_.map{ case (n, c) if c+cost < costs.get(node).getOrElse(0) =>  (n, c + cost) })
        val newParents: Map[String, String]
        apply(newCost.getOrElse(costs), parents, processed + node)
      case Some((node,cost)) =>
        println(processed)
      case None => 
        parents
    }
  
  val costsOpt = for { s <- graph.get("Start"); f <- graph.get("Fin") } yield s ++ f
  val parentsOpt: Option[Map[String, Set[String]]] = graph.get("Start").map(_.keys).map(ns => Map[String, Set[String]]("Start" -> ns.toSet))
  apply(costsOpt.getOrElse(Map[String,Int]()), parentsOpt.getOrElse(Map[String,Set[String]]()), Set())
}

//graph definition
// we're going from "start" to "fin"
val graph: Map[String, Map[String, Int]] = Map("Start" -> Map("A" -> 6, "B" -> 2), "A" -> Map("Fin" -> 1), "B" -> Map("A" -> 3, "Fin" -> 5), "Fin" -> Map("?" -> Int.MaxValue))

djikstra(graph)
