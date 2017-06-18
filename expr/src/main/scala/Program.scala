import scala.collection.immutable

object Program extends App {

  if (args.length == 2) {
    val list = args(0).split(",").map(_.toInt).toList
    val result = args(1).toInt
    val solution: immutable.Seq[Test2.Expr] = Test2.solutions(list, result)
    solution match {
      case Nil => println("No Solution")
      case _ => solution.foreach(println)
    }
  } else {
    println("Incorrect usage")
  }

}
