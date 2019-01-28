type AdjacencyMatrix = Array[Array[Double]]
type AdjacencyVector = Vector[Double]
type CurrencyPairAndRate = (String, String, Double)
type CurrencyPair = (String, String)
type CurrencyNames = Vector[String]

object Printer {
  val programDesc =
    s"""|
      |*********************
        |Arbitrage Detector
        |*********************
        |
      |This program is trying to solve the Bitcoin Arbitrage puzzle specified at https://priceonomics.com/jobs/puzzle/
        |
      |Used Algorithms:
        |- Bellman-Ford for detecting one possible profitable arbitrage
        |- Full Search which is an NP problem therefore for big currency numbers it is inefficient
        |  so only used for reporting purposes
        |""".stripMargin

  val arbitrageHeader =
    s"""|
          |The following arbitrage found:
        |""".stripMargin

  def printClearArbitrage(srcCcy: String) = println(
    s"""|
              |*********************
        |Detector message
        |*********************
        |              |
        |Find a clear arbitrage opportunity to invest ${srcCcy} and get back profit in ${srcCcy}
        |""".stripMargin)

  def printNonClearArbitrage(srcCcy: String, toCcy: String, srcIgnorable: Boolean) = {
    val endMsg = if (srcIgnorable)
      s"stick to ${srcCcy} (not for sure profitable) arbitrage detection"
    else
      s"find a clear opportunity in any currency"
    val profitable = if (srcIgnorable) "" else " (not for sure)"
    println(
      s"""|
        |*********************
          |Detector message
          |*********************
          |
          |There is no clear ${srcCcy} to ${srcCcy} arbitrage found with Bellman-Ford.
          |Since the srcIgnorable program argument set to '${srcIgnorable}' therefore we
          |print out a$profitable profitable ${toCcy} to ${toCcy} arbitrage opportunity
          |Please run with '--srcIgnorable ${!srcIgnorable}' program argument to $endMsg
          |""".stripMargin)
  }

  def printBesttrades(bestTrades: Vector[(String, Double)],
                      resultAmount: Double,
                      profitableTrades: Seq[Vector[(String, Double)]]) = {
    val rankOfDetectedTrades = profitableTrades.map(_.last._2).indexOf(resultAmount) + 1
    println(
      s"""|
          |*****************
          |Report
          |*****************
          |
          |Below you can see a report about the detected arbitrage.
          |The report always run with full arbitrage search which has an NP complexity.
          |Please run with '--askReport false' in order not to print a report out
          |
          |The most profitable arbitrage is:
          |""".stripMargin)
    printTradeChain(bestTrades)
    println(
      s"""|
          |The detected $resultAmount is the ${rankOfDetectedTrades}. best arbitrage
          |""".stripMargin)
  }
  def printInputArray(currencies: Vector[String], adjMatrix: Array[Array[Double]]) = {
    println(s"Currencies: ${currencies.mkString(",")}")
    println(s"Parsed (prepared) data for Bellman-Ford graph algorithm:\n${adjMatrix.map(_.mkString("\t\t")).mkString("\n")}\n")
  }
  def printInputVector(currencies: Vector[String], adjVector: Vector[Double]) = {
    println(s"Currencies: ${currencies.mkString(",")}")
    println(s"Parsed (prepared) data for Bellman-Ford graph algorithm:\n${adjVector}\n")
  }
  def printUrl(url: String) = println(
    s"""|
          |Loading fx rates from $url
        |""".stripMargin)
  def printJson(jsonStr: String) = println(s"The fx rates in JSON format:\n$jsonStr\n")
  def printTradeChain(fxChanges: Vector[(String, Double)]): Unit = {
    val trades = fxChanges.zip(fxChanges.tail)
    trades.foreach {
      case ((ccyFrom, fromAmount), (ccyTo, toAmount)) => println(s"Trade $fromAmount $ccyFrom to $toAmount $ccyTo")
    }
  }
}