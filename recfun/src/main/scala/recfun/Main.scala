package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def nextRow(rowElements: List[Int]) : List[Int] =
      rowElements match {
        case x::Nil => List(1)
        case x::xs => (x + xs.head)::nextRow(xs)
      }
    def pascalAux(rowElements: List[Int], curR: Int) : Int = {
      if (curR == 0) {
        rowElements(c);
      } else {
        pascalAux(1::nextRow(rowElements), curR-1)
      }
    }
    pascalAux(List(1), r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceWithDepth(_chars : List[Char], depth : Int) : Boolean = {
      if (depth < 0) return false
      _chars match {
        case Nil      => depth == 0
        case '('::cs  => balanceWithDepth(cs, depth+1)
        case ')'::cs  => balanceWithDepth(cs, depth-1)
        case c::cs    => balanceWithDepth(cs, depth)
      }
    }
    balanceWithDepth(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0)      return 0
    if (money == 0)     return 1
    coins match {
      case Nil =>       0
      case c::rest =>   countChange(money-c, c::rest) + countChange(money, rest)
    }
  }
}
