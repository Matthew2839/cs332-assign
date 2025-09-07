import scala.annotation.tailrec

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit = {
  //Exercise 1
  println("Pascal's Triangle")
  for (row <- 0 to 10) {
    for (col <- 0 to row)
      print(pascal(col, row) + " ")
    println()
  }
  //Exercise 2
  println()
  println("Parentheses Balancing")
  var testValue = 0
  val testChars = "(Enter test string here)()".toList
  println(balance(testChars))

  //Exercise 3
  println()
  println("Counting Change")
  val testMoney = 50
  val testCoins = List(3,4,5,2,1)
  println(countChange(testMoney, testCoins))
}

/**
 * Exercise 1
 */
def pascal(c: Int, r: Int): Int = {
  if (c == 0 || c == r)
    1
  else
    pascal(c-1, r-1) + pascal(c, r-1)
}

/**
 * Exercise 2
 */
def balance(chars: List[Char]): Boolean = {
  def depthCalc (head: Char, depth: Int): Int = {
    if (head == '(')
      depth + 1
    else if (head == ')')
      depth - 1
    else
      depth
  }
  @tailrec
  def loop(chars: List[Char], depth: Int) : Boolean = {
    if (chars.isEmpty || depth < 0)
      if (depth == 0)
        true
      else
        false
    else
      loop (chars.tail, depthCalc(chars.head, depth))
  }
  loop (chars, 0)
}

/**
 * Exercise 3
 */
def countChange(money: Int, coins: List[Int]): Int = {
  def loop(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money < 0 || coins.isEmpty)
      0
    else
      loop(money, coins.tail) + loop(money - coins.head, coins)
  }
  if (money == 0 || coins.isEmpty || coins.exists(_ <= 0))
    0
  else
    loop (money, coins)
}