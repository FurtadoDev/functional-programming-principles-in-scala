package recfun

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
    if (c == 0 || r == c) { // The first and last element of any row are always 1
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def recurse(input_list: List[Char], num_open_brackets: Int): Boolean = {
      if (num_open_brackets == -1) {
        false
      } else if (num_open_brackets == 0 && input_list.isEmpty) {
        true
      } else {
        if (input_list.head == '('){
          recurse(input_list.tail, num_open_brackets + 1)
        } else if(input_list.head == ')'){
          recurse(input_list.tail, num_open_brackets - 1)
        } else {
          recurse(input_list.tail, num_open_brackets)
        }
      }
    }

    recurse(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def recurse(target: Int, denominations: List[Int]): Int = {
      if (target == 0) {
        1
      } else if (target < 0 || denominations.isEmpty) {
        0
      } else {
        recurse(target - denominations.head, denominations) + recurse(target, denominations.tail)
      }
    }
    recurse(money, coins)
  }
