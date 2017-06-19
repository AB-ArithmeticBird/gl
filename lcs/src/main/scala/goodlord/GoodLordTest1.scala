package goodlord

import scalaz.Memo


object GoodLordTest1 {

  def lcs(left: String, right: String): String = {

    lazy val lcsMemo: ((List[Char], List[Char])) => List[Char] = Memo.mutableHashMapMemo {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (x :: xs, y :: ys) if x == y => x :: lcsMemo(xs, ys)
      case (x :: xs, y :: ys) =>
        (lcsMemo(x :: xs, ys), lcsMemo(xs, y :: ys)) match {
          case (xss, yss) if xss.length > yss.length => xss
          case (_, yss) => yss
        }
    }
    lcsMemo(left.toList, right.toList).mkString
  }

}