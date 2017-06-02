import scala.annotation.tailrec

object dy {
  case class Point(x: Int, y: Int)
  def matches(a: String, b: String) = {
    val matrix = Array.ofDim[Int](a.length, b.length)
    @tailrec
    def apply(matrix: Array[Array[Int]], marker: Point): Int = {
      marker match {
        case p: Point
            if p.y < b.length - 1 || (p.y == b.length - 1 && p.x < a.length - 1) =>
          apply(update(a, b, matrix, p), move(p, a, b))
        case p: Point if p.x == a.length - 1 && p.y == b.length - 1 =>
          biggest(update(a, b, matrix, p))
      }
    }
    apply(Array.ofDim(a.length, b.length), Point(0, 0))
  }

  private def move(p: Point, a: String, b: String): Point = p match {
    case Point(x, y) if x < a.length - 1 => Point(x + 1, y)
    case Point(x, y) if x == a.length - 1 && y < b.length - 1 =>
      Point(0, y + 1)
    case Point(x, y) if y < b.length - 1 => Point(x, y + 1)
    case p => p
  }

  private def update(a: String,
                     b: String,
                     matrix: Array[Array[Int]],
                     at: Point) =
    if (compare(a(at.x), b(at.y))) {
      val newval = (at: Point) =>
        if (at.x - 1 >= 0 && at.y - 1 >= 0)
          matrix(at.x)(at.y) + matrix(at.x - 1)(at.y - 1) + 1
        else matrix(at.x)(at.y) + 1
      matrix.updated(at.x, (matrix(at.x).updated(at.y, newval(at))))
    } else
      matrix

  private def compare(x: Char, y: Char): Boolean =
    x equals y

  private def biggest(matrix: Array[Array[Int]]): Int =
    matrix.map(_.max).max
}

println(s"Longest substring is: ${dy.matches("ABC", "ABD")}")
