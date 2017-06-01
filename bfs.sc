import scala.collection.immutable.Queue
import scala.annotation.tailrec
object b {
  type Person = String

  def bfs(graph: Map[Person, Set[Person]]) = {
    @tailrec
    def apply(queue: Queue[Person], graph: Map[Person, Set[Person]], processed: Set[Person]): Option[Person] = {
      val personOpt = queue.headOption
      personOpt map(processed.contains) match {
        case Some(false) if personOpt.map(stop).get =>
          personOpt
        case None =>
          personOpt
        case Some(false) =>
          val person = personOpt.get
          apply(graph.get(person).map(setToQueue).getOrElse(Queue()) ++ queue.drop(1), graph, processed + person)
        case _ =>
          apply(queue.drop(1), graph, processed + personOpt.get)
      }
    }
    apply(graph.get("you").map(setToQueue).getOrElse(Queue()), graph, Set())
  }

  def setToQueue(set: Set[Person]): Queue[Person] =
    set.foldLeft(Queue[Person]())((acc,curr) => acc :+ curr)
  def stop(p: Person) = p.startsWith("m")
}

val graph: Map[String,Set[String]] = Map("you" -> Set("alice", "bob", "claire"), "bob" -> Set("anuj","peggy")
, "alice" -> Set("peggy"), "claire" -> Set("thom","jonny"), "anuj" -> Set(), "peggy"
 -> Set(), "thom" -> Set(), "jonny" -> Set("matt"))

b.bfs(graph).map(println)
