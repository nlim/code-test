import scala.collection.JavaConversions._

 object Solution {
   class Tree(var x: Int, var l: Tree, var r: Tree)

   val example = new Tree( 5, new Tree( 3, null, null), new Tree( 10, new Tree( 12, new Tree( 21, null, null) , new Tree( 20, null, null)), null))

 def main(args: Array[String]): Unit = {
   println(solution(example))
 }

 def solution(T: Tree): Int = {
   val x = solutionHelper3(List(((0, 0), T)), Nil)
   getMax(x)
 }

 def getMax(tuples: List[(Int, Int)]): Int = {
   val ms = tuples.foldRight((0, 0)) {
     case ((l, r), (lm, lr)) => (l.max(lm), r.max(lr))
   }
   ms._1.max(ms._2)
 }

 @annotation.tailrec
 def solutionHelper3(vals: List[((Int, Int), Tree)], results: List[(Int, Int)]): List[(Int, Int)] = {
   vals match {
     case Nil => results
     case ((parentLeft, parentRight), t) :: ts =>
       if (t == null) {
         results
       } else {
         val newResults = (parentLeft, parentRight) :: results
         val newValues =
           Option(t.l).map(left  => ((parentLeft + 1,  0), left)).toList ++
           Option(t.r).map(right => ((0, parentRight + 1), right)).toList ++
           ts

         solutionHelper3(newValues, newResults)
       }
   }
 }



}
