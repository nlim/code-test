import scala.collection.JavaConversions._



trait Common {
  class Tree(var x: Int, var l: Tree, var r: Tree)

  val example = new Tree( 5, new Tree( 3, null, null), new Tree( 10, new Tree( 12, new Tree( 21, null, null) , new Tree( 20, null, null)), null))

  val example2 = new Tree(
    5,
    new Tree(8, new Tree(12, null, null), new Tree(2, null, null)),
    new Tree(9, new Tree(7, new Tree(1, null, null), null),  new Tree(4, new Tree(3, null, null), null))
  )

  val example3 = new Tree(
    5,
    new Tree(3, new Tree(20, null, null), new Tree(21, null, null)),
    new Tree(10, new Tree(1, null, null),  null)
  )

  def createExample(depth: Int): Tree = {
    if (depth <= 0) {
      null
    } else {
      val x = scala.util.Random.nextInt(10000)
      new Tree(x, createExample(depth - 1), createExample(depth - 1))
    }
  }

  def applyChild[T](t: Tree)(f: Tree => T): List[T] =
    Option(t).map(f).toList

  def childWork[T](t: Tree)(f: Tree => T): List[T] = {
    applyChild(t.l)(f) ++ applyChild(t.r)(f)
  }
}


object Solution extends Common {

  def main(args: Array[String]): Unit = {
    println(solution(example))
  }

  def solution(T: Tree): Int = {
    val x = solutionHelper(List(((0, 0), T)), Nil)
    getMax(x)
  }

  def getMax(tuples: List[(Int, Int)]): Int = {
    val ms = tuples.foldRight((0, 0)) {
      case ((l, r), (lm, lr)) => (l.max(lm), r.max(lr))
    }
    ms._1.max(ms._2)
  }

  @annotation.tailrec
  def solutionHelper(vals: List[((Int, Int), Tree)], results: List[(Int, Int)]): List[(Int, Int)] = {
    vals match {
      case Nil => results
      case ((parentLeft, parentRight), t1) :: ts =>
        Option(t1) match {
          case None => results
          case Some(t) =>
            val newResults = (parentLeft, parentRight) :: results
            val newValues =
              applyChild(t.l)(i => ((parentLeft + 1,  0), i)) ++
            applyChild(t.r)(i => ((0, parentRight + 1), i)) ++ ts

            solutionHelper(newValues, newResults)
        }
    }
  }
}
