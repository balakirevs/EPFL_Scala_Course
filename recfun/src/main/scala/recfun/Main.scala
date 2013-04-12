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
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  /*
  def balance(chars: List[Char]): Boolean = chars.foldLeft(0){
    case (0, ')') => return false
    case (x, ')') => x - 1
    case (x, '(') => x + 1
    case (x, _  ) => x
  } == 0
  */
  
  def balance(chars: List[Char]): Boolean = {
    def balanced(chars: List[Char], open: Int): Boolean = 
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') balanced(chars.tail,open+1)
      else if (chars.head == ')') open>0 && balanced(chars.tail,open-1)
      else balanced(chars.tail,open)
    balanced(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countSorted(money: Int, coins: List[Int]): Int = coins match {
      case coin :: rest =>
        if (coin > money) 0
        else if (money == coin) 1
        else {
          val waysToCountRemaining = countSorted(money - coin, coins)
          val waysToCountSameAmountWithoutCoin = countSorted(money, rest)
          waysToCountRemaining + waysToCountSameAmountWithoutCoin
        }
      case _ => 0 //No coins
    }
   
    countSorted(money, coins.sorted)
  }
}

