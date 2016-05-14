import scala.collection.JavaConversions._

object Solution2 extends Common {

 def main(args: Array[String]): Unit = {
   val ex = example2
   println(solution(ex))
 }

 def solution(T: Tree): Int = {
   val x =
     if (T == null) List.empty[(Int, Int)]
     else solutionHelper(List(((T.x, T.x), T)), Nil)

   getAmplitude(x)
 }

 def getAmplitude(tuples: List[(Int, Int)]): Int = {
   tuples match {
     case Nil   => 0
     case (min1, max1) :: rest =>
       val startingAmp = max1 - min1
       rest.foldLeft(startingAmp) {
         case (amp, (min2, max2)) => (max2 - min2).max(amp)
       }
   }
 }

 @annotation.tailrec
 def solutionHelper(trees: List[((Int, Int), Tree)], results: List[(Int, Int)]): List[(Int, Int)] = {
   trees match {
     case Nil               => results
     case (vals, t) :: ts   =>
       if (t == null) {
         solutionHelper(ts, results)
       } else {
         val (parentMin, parentMax) = vals
         val newVals    = (parentMin.min(t.x), parentMax.max(t.x))
         val newResults = newVals :: results
         val newTrees   = childWork(t)(i => (newVals, i)) ++ ts

         solutionHelper(newTrees, newResults)
       }
   }
 }
}
