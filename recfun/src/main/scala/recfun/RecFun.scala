package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  def pascal(c: Int, r: Int): Int = c match {
    case 0 | `r` => 1
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  def balance(chars: List[Char]): Boolean = {
    val open_paren = '('
    val close_paren = ')'

    @scala.annotation.tailrec def paranthesesBalan(chars: List[Char], numberOfOpens: Int = 0): Boolean = {
      if(chars.isEmpty) numberOfOpens == 0
      else chars.head match {
        case `open_paren` => paranthesesBalan(chars.tail, numberOfOpens + 1)
        case `close_paren` => numberOfOpens > 0 && paranthesesBalan(chars.tail, numberOfOpens - 1)
        case _ => paranthesesBalan(chars.tail, numberOfOpens)
      }
    }

    paranthesesBalan(chars)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeRec(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < 0) 0
      else if (money.equals(0)) 1
      else countChangeRec(money, coins.tail) + countChangeRec(money - coins.head, coins)
    }

    countChangeRec(money, coins)
  }
}
