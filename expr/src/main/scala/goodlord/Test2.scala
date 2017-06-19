package goodlord


object Test2 {

  /**
    * Operators
    *
    */
  sealed trait Op extends Product with Serializable

  case object Add extends Op {
    override def toString: String = " + "
  }

  case object Sub extends Op {
    override def toString: String = " - "
  }

  case object Mul extends Op {
    override def toString: String = " * "
  }

  lazy val ops = List(Add, Sub, Mul)

  /**
    * Expressions
    */
  sealed trait Expr extends Product with Serializable

  final case class Number(value: Int) extends Expr {
    override def toString: String = value.toString
  }

  final case class Other(o: Op, e1: Expr, e2: Expr) extends Expr {
    override def toString: String = "(" + e1 + " " + o + " " + e2 + ")"
  }

  type Result = (Expr, Int)

  //
  // Whether an operator application is valid or not
  //
  def valid(o: Op, x: Int, y: Int): Boolean = (o, x, y) match {
    case (Add, a, b) => a <= b
    case (Sub, a, b) => a > b
    case (Mul, a, b) => (a != 1) && (b != 1) & (a <= b)
  }

  //
  // apply the operator
  //
  def app(o: Op, a: Int, b: Int): Int = (o, a, b) match {
    case (Add, x, y) => x + y
    case (Sub, x, y) => x - y
    case (Mul, x, y) => x * y
  }

  //Evaluating the expression
  def eval(e: Expr): List[Int] = {
    val r =e match {
      case Number(num) => List(num)
      case Other(o, l, r) => for {
        x <- eval(l)
        y <- eval(r)
        if valid(o, x, y)
      } yield app(o, x, y)
    }
    //println(r)
    r
  }


  //gives the list of all subsequences of the argument.
  //scala> combinations(List(1,2,3))
  //res4: List[List[Int]] = List(List(), List(3), List(2), List(2, 3), List(1), List(1, 3), List(1, 2), List(1, 2, 3))
  def combinations[A](list: List[A]): List[List[A]] = {
    val r = list match {
      case Nil => List(List.empty[A])
      case x :: xs => {
        val tailComb = combinations(xs)
        tailComb ::: tailComb.map(sub => x :: sub)
      }
    }
    //println("No of combinations:" + r.size)
    r
  }

  // It returns all the instances of list which are essentially all possible ways of
  // selecting zero or more elements in every order
  //choices(List(1,2)) will return List((),(1),(2),(1,2),(2,1))
  def choices(xs: List[Int]): List[List[Int]] = {
    val c = combinations(xs).flatMap(a => a.permutations)
    //println(s"Number of choices: ${c.size}")
    c
  }


  //returns all possible ways of splitting a list into two non-empty lists
  //scala> Test2.split(List(1,2,3))
  //res2: List[(List[Int], List[Int])] = List((List(1),List(2, 3)), (List(1, 2),List(3)))
  def split[T](l: List[T]): List[(List[T], List[T])] = {
    val r = l match {
      case Nil => Nil
      case List(_) => Nil
      case (x :: xs) => (List(x), xs) :: (for {(ls, rs) <- split(xs)} yield (x :: ls, rs))
    }
    //println("in split:" + r)
    r
  }

  //all possible results
  def results(nss: List[Int]): List[Result] = {
    val r = nss match {
      case Nil => Nil
      case List(n) if n > 0 => List((Number(n), n))
      case ns =>
        for {
          (ls, rs) <- split(ns)
          l <- results(rs)
          r <- results(ls)
          e <- combine(l, r)
        } yield e
    }
    //println("In results: " + r)
    r
  }

  def combine(l: Result, r: Result): List[Result] = for {
    op <- ops
    if valid(op, l._2, r._2)
  } yield (Other(op, l._1, r._1), app(op, l._2, r._2))


  //The main function 
  def solutions(ns: List[Int], n: Int): List[Expr] = for {
    nss <- choices(ns)
    (e, m) <- results(nss)
    if m == n
  } yield e
}
