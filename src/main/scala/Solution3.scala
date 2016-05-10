import scala.collection.JavaConversions._

// you can write to stdout for debugging purposes, e.g.
// // println("this is a debug message")

 object Solution3 {

   class Tree(var x: Int, var l: Tree, var r: Tree) {
     //override def toString = s"Tree($x, ${if(l == null) null else l.toString}, ${if(r == null) null else r.toString})"
   }

   val example = new Tree(
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

   def main(args: Array[String]): Unit = {
     //val ex = createExample(20)
     println(solution(example))
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
           val visible = if (m > t.x) 0 else 1
           val newMax  = m.max(t.x)
           if (t.l != null && t.r != null) {
             solutionHelper((newMax, t.l) :: (newMax, t.r) :: ts, total + visible)
           } else if (t.l != null && t.r == null) {
             solutionHelper((newMax, t.l) :: ts, total + visible)
           } else if (t.l == null && t.r != null) {
             solutionHelper((newMax, t.r) :: ts, total + visible)
           } else {
             solutionHelper((newMax, t.l) :: (newMax, t.r) :: ts, total + visible)
           }
         }
     }
   }
 }

