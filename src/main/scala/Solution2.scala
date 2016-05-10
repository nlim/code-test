import scala.collection.JavaConversions._

// you can write to stdout for debugging purposes, e.g.
// // println("this is a debug message")

 object Solution2 {
   class Tree(var x: Int, var l: Tree, var r: Tree)

   val example = new Tree(
     5,
     new Tree(8, new Tree(12, null, null), new Tree(2, null, null)),
     new Tree(9, new Tree(7, new Tree(1, null, null), null),  new Tree(4, new Tree(3, null, null), null))
   )

   val example2 = new Tree(
     5,
     null,
     null
   )

   def createExample(depth: Int): Tree = {
     if (depth <= 0) {
       null
     } else {
       val x = scala.util.Random.nextInt(10000)
       new Tree(x, createExample(depth - 1), createExample(depth - 1))
     }
   }

 def main(args: Array[String]): Unit = {
   //val ex = createExample(15)
   val ex = example
   println(solution(ex))
 }

 def solution(T: Tree): Int = {
   val x = if (T == null) List.empty[(Int, Int)] else solutionHelper2(   List( ((T.x, T.x), T)), Nil)
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
 def solutionHelper2(trees: List[((Int, Int), Tree)], results: List[(Int, Int)]): List[(Int, Int)] = {
   trees match {
     case Nil               => results
     case (vals, t) :: ts   =>
       val (parentMin, parentMax) = vals
       if (t == null) {
         solutionHelper2(ts, results)
       } else {
         val newVals = (parentMin.min(t.x), parentMax.max(t.x))
         if (t.l != null && t.r != null) {
           solutionHelper2( (newVals, t.l) :: (newVals, t.r) :: ts, newVals :: results)
         } else if (t.l != null && t.r == null) {
           solutionHelper2( (newVals, t.l) :: ts, newVals :: results)
         } else if (t.l == null && t.r != null) {
           solutionHelper2( (newVals, t.r) :: ts, newVals :: results)
         } else {
           solutionHelper2(  ts, newVals :: results)
         }
       }
   }
 }
}
