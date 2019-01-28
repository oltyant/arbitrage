import $file.ArbitrageGraphs
import ArbitrageGraphs._
import $file.ArbitrageInputParser
import ArbitrageInputParser._
import ArbitrageInputParser.FxRatesImplicits._
import $file.ArbitrageAlgorithms
import ArbitrageAlgorithms._
import $file.ArbitrageTypes, ArbitrageTypes._, Printer._

import scala.collection.mutable.WrappedArray

@doc(programDesc)
@main
def main(
  srcCcy: String = "USD",
  amount: Double = 1.0,
  srcIgnorable: Boolean = true,
  askReport: Boolean = true,
  dataRepr: String = "vector"
): Unit = {
  println(programDesc)
  require(amount > 0.0,
    s"You cannot invest ($amount) $srcCcy into the fx trades. Please select a positive number!")
  ArbitrageDriver(dataRepr, srcCcy, askReport)
    .run(amount, srcIgnorable)
}

trait ArbitrageDriver {
  require(currencies.contains(srcCcy),
    s"The $srcCcy is not in the valid currency list ${currencies}")
  def currencies: Vector[String]
  def srcCcy: String
  def askReport: Boolean
  def run(amount: Double, srcIgnorable: Boolean): Unit
  def report(fxChanges: Vector[(String, Double)], startAmount: Double, resultAmount: Double): Unit
  def inputUrl: String

  protected def printSolution(fxChanges: Vector[(String, Double)], startAmount: Double, resultAmount: Double) = {
    println(arbitrageHeader)
    printTradeChain(fxChanges)
  }
}

object ArbitrageDriver {
  def apply(dataReprStr: String, srcCcy: String, askReport: Boolean) = dataReprStr.toLowerCase.trim match {
    case ("array"|"matrix") => new ArbitrageDriverOnArray(srcCcy, askReport)
    case _ => new ArbitrageDriverOnVector(srcCcy, askReport)
  }
}

class ArbitrageDriverOnVector(
  val srcCcy: String,
  val askReport: Boolean,
  val inputUrl: String = "https://fx.priceonomics.com/v1/rates/"
) extends ArbitrageDriver {
  lazy val (currencies, g) = FxRatesParser[AdjacencyVector](inputUrl)
  lazy val graph = ArbitrageGraph(g, currencies)

  def run(amount: Double, srcIgnorable: Boolean) = {
    val src = currencies.indexOf(srcCcy)
    val detector = ArbitrageDetector[Vector](graph, src, srcIgnorable)
    detector.calc(amount).lastOption match {
      case Some(fxChanges) =>
        val (_, resultAmount) = fxChanges.last
        printSolution(fxChanges, amount, resultAmount)
        if (askReport) report(fxChanges, amount, resultAmount)
      case None =>
        report(Vector(), amount, amount)
        throw new Exception(s"There is no arbitrage detected!")
    }
  }

  def report(fxChanges: Vector[(String, Double)], startAmount: Double, resultAmount: Double): Unit = {
    val newSrcCcy = fxChanges.headOption.map(_._1).getOrElse(srcCcy)
    val src = currencies.indexOf(newSrcCcy)
    val allIn = ArbitrageDetector.allIn[Vector](graph, src)
    val allTrades = allIn.calc(startAmount)
    val profitableTrades = allTrades.filter(_.last._2 > 1.0).sortWith(_.last._2 > _.last._2)
    val bestTrades = profitableTrades.head
    printBesttrades(bestTrades, resultAmount, profitableTrades)
  }
}

class ArbitrageDriverOnArray(
  val srcCcy: String,
  val askReport: Boolean,
  val inputUrl: String = "https://fx.priceonomics.com/v1/rates/"
) extends ArbitrageDriver {
  lazy val (currencies, g) = FxRatesParser[AdjacencyMatrix](inputUrl)
  lazy val graph = ArbitrageGraph(g, currencies)

  def run(amount: Double, srcIgnorable: Boolean) = {
    val src = currencies.indexOf(srcCcy)
    val detector = ArbitrageDetector[WrappedArray](graph, src, srcIgnorable)
    detector.calc(amount).lastOption match {
      case Some(fxChanges) =>
        val (_, resultAmount) = fxChanges.last
        printSolution(fxChanges, amount, resultAmount)
        if (askReport) report(fxChanges, amount, resultAmount)
      case None =>
        //TODO test this branch
        report(Vector(), amount, amount)
        throw new Exception(s"There is no arbitrage detected!")
    }
  }

  def report(fxChanges: Vector[(String, Double)], startAmount: Double, resultAmount: Double): Unit = {
    val newSrcCcy = fxChanges.headOption.map(_._1).getOrElse(srcCcy)
    val src = currencies.indexOf(newSrcCcy)
    val allIn = ArbitrageDetector.allIn[WrappedArray](graph, src)
    val allTrades = allIn.calc(startAmount)
    val profitableTrades = allTrades.filter(_.last._2 > 1.0).sortWith(_.last._2 > _.last._2)
    val bestTrades = profitableTrades.head
    printBesttrades(bestTrades, resultAmount, profitableTrades)
  }
}