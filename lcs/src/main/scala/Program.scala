
object Program  extends App{
  if(args.length == 2) {
    val first = args.head
    val second = args(1)
    val result = GoodLordTest1.lcs(first, second)
    println(s"Result is: $result")
  } else {
    println("Incorrect number of strings")
    println("Usage: java lcs string1 string2")
  }
  
}
