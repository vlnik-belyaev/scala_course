package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    //print(countChange(7,List(5,2,1)))
    print(countChange(300,List(5,10,20,50,100,200,500)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else if (c > r) {
      throw new java.lang.IllegalArgumentException()
    }
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    val pars = chars.filter(c => c=='('||c==')')
    checkBalance {
      pars
    }
  }

  def checkBalance(pars: List[Char]): Boolean = {
    var invariant: Int = 0
    for(c <- pars){
      if(c == '(') invariant = invariant +1
      else invariant = invariant - 1
      if(invariant < 0) return false
    }
    return invariant == 0
  }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0 || coins.isEmpty) throw new IllegalArgumentException()
    return countChangeRec(money, coins.sorted.reverse)
  }

  def countChangeRec(money: Int, sortedCoins: List[Int]): Int = {
    var result: Int = 0
    if(sortedCoins.length == 1)
      {
        if(money % sortedCoins(0) == 0) result = 1
        else result = 0
      }
    else{
      val biggestCoin = sortedCoins(0)
      val biggestCoinPossibleCount = money /  biggestCoin
      for(i <- 0 to biggestCoinPossibleCount)
        result += countChangeRec(money - biggestCoin*i, sortedCoins.tail)
    }
    return result
  }

  }
