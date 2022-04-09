package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || c > r) 0
    else if (r == 0) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    @tailrec
    def isBalanced(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else {
        if (chars.head == '(') isBalanced(chars.tail, open+1)
        else if (chars.head == ')') open > 0 && isBalanced(chars.tail, open-1)
        else isBalanced(chars.tail, open)
      }
    }

    isBalanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(capacity: Int, changes: List[Int]): Int = {
      if (capacity == 0) 1
      else if (capacity < 0) 0
      else if (changes.isEmpty) 0
      else count(capacity, changes.tail) + count(capacity - changes.head, changes)
    }
    coins.sorted
    count(money, coins)
  }
