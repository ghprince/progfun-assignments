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
    if (c == 0 || c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def procBalance(flag: Int, subchars: List[Char]): Boolean = {
      if (subchars.isEmpty) flag == 0
      else if (flag < 0) false
      else if (subchars.head == '(')
        procBalance(flag + 1, subchars.tail)
      else if (subchars.head == ')')
        procBalance(flag - 1, subchars.tail)
      else
        procBalance(flag, subchars.tail)
    }
    procBalance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sorted_coins = coins.sorted
    def count(m: Int, n: Int): Int = {
      if (n == 0) 1
      else if (n < 0) 0
      else if (m <= 0 && n >= 1) 0
      else count(m - 1, n) + count(m, n - sorted_coins(m - 1))
    }
    count(sorted_coins.length, money)
  }
}
