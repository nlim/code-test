import scala.collection.JavaConversions._

// you can write to stdout for debugging purposes, e.g.
// // println("this is a debug message")

 object Solution3 extends Common {
   def main(args: Array[String]): Unit = {
     println(solution(example3))
   }

   def solution(T: Tree): Int = {
     solutionHelper(List((0, T)), 0)
   }


   @annotation.tailrec
   def solutionHelper(treesWithMax: List[(Int, Tree)], total: Int): Int = {
     treesWithMax match {
       case Nil     => total
       case (m, t) :: ts =>
         if (t == null) {
           solutionHelper(ts, total)
         } else {
           val visible  = if (m > t.x) 0 else 1
           val newMax   = m.max(t.x)
           val newTotal = total + visible
           val newTreesWithMax = childWork(t)({(i: Tree) => (newMax, i)}) ++ ts

           solutionHelper(newTreesWithMax, newTotal)
         }
     }
   }
 }

