import scala.annotation.tailrec

object bs {
  @tailrec
  def apply(xs: Seq[Int]): Seq[Int] =
    sortOnce(xs) match {
      case (seq, true) => apply(seq)
      case (seq, false) => seq
    }

  @tailrec
  def sortOnce(xs: Seq[Int], it: Int = 0, switched: Boolean = false): (Seq[Int], Boolean) =
    xs.drop(it).take(2) match {
      case Seq(h, t) if h < t =>
        sortOnce((xs.take(it) :+ h :+ t) ++ xs.drop(it + 2), it + 1, switched)
      case Seq(h, t) =>
        sortOnce((xs.take(it) :+ t :+ h) ++ xs.drop(it + 2), it + 1, true)
      case _ => (xs, switched)
    }

}

println(s"Sorted list is: ${bs(Seq(8,1,2,4,3))}")
