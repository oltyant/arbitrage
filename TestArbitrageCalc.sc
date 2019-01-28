import $ivy.`org.scalatest::scalatest:3.0.5`
import $ivy.`org.scalacheck::scalacheck:1.14.0`
import $file.ArbitrageGraphs
import ArbitrageGraphs._
import $file.ArbitrageInputParser
import ArbitrageInputParser._
import ArbitrageInputParser.FxRatesImplicits._
import $file.ArbitrageAlgorithms
import ArbitrageAlgorithms._
import $file.ArbitrageTypes
import ArbitrageTypes._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.collection.mutable.WrappedArray

trait ArbitrageInputGenerator {
  private val currencies = Array("USD", "JPY", "EUR", "BTC")
  private val seedArr = Array(
    Array(    1.0,    88.62,    0.67,   0.0071),
    Array( 0.0107,      1.0,  0.0086, 0.000092),
    Array(  1.334,   136.68,   1.000,   0.0116),
    Array(115.815, 11855.55, 85.4281,      1.0)
  )
  private val changeFactorGen = Gen.chooseNum(0.98, 1.03)

  def genArr =
    Gen.zip(Gen.const(seedArr), changeFactorGen).map {
      case (seed, changeFactor) =>
        seed.map(row => row.map(_ * changeFactor))
    }

  implicit val arbFxMap = Arbitrary {
    genArr.map {
      arr =>
        val arrFxRatePairs = for {
          i <- (0 until arr.size)
          fromCcy = currencies(i)
          j <- (0 until arr.size)
          toCcy = currencies(j)
          fxRate = arr(i)(j)
        } yield (fromCcy -> (toCcy -> fxRate))
        arrFxRatePairs
          .groupBy(_._1)
          .mapValues(_.map(_._2).toMap)
        }
  }

  implicit val arbJsonString = Arbitrary {
    arbFxMap.arbitrary.map {
      fxMap =>
        f"""|{"USD_JPY": "${fxMap("USD")("JPY")}%.6f",
            |"USD_USD": "1.0000000",
            |"JPY_EUR": "${fxMap("JPY")("EUR")}%.6f",
            |"BTC_USD": "${fxMap("BTC")("USD")}%.6f",
            |"JPY_BTC": "${fxMap("JPY")("BTC")}%.6f",
            |"USD_EUR": "${fxMap("USD")("EUR")}%.6f",
            |"EUR_USD": "${fxMap("EUR")("USD")}%.6f",
            |"EUR_JPY": "${fxMap("EUR")("JPY")}%.6f",
            |"JPY_USD": "${fxMap("JPY")("USD")}%.6f",
            |"BTC_BTC": "1.0000000",
            |"EUR_BTC": "${fxMap("EUR")("BTC")}%.6f",
            |"BTC_JPY": "${fxMap("BTC")("JPY")}%.6f",
            |"JPY_JPY": "1.0000000",
            |"BTC_EUR": "${fxMap("BTC")("EUR")}%.6f",
            |"EUR_EUR": "1.0000000",
            |"USD_BTC": "${fxMap("USD")("BTC")}%.6f"}""".stripMargin
    }
  }

  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
}

class TestArbitrageCalculator extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with ArbitrageInputGenerator {

  behavior of "ArbitrageDetector"

  it should "calculate the proper arbitrage on adjacency vector" in {
    forAll((minSuccessful(100))) { (jsonStr: String) =>
      println(jsonStr)
      val (currencies, vector) = FxRatesParser.parseJsonStr[AdjacencyVector](jsonStr)
      val srcCcy = "USD"
      val src = currencies.indexOf(srcCcy)
      val amount = 1.0
      val graph = ArbitrageGraph(vector, currencies)
      val fxTrades = ArbitrageDetector[Vector](graph, src)
        .calc(amount)
        .head
      val (newSrc, actual) = fxTrades.last
      val newSrcCcy = graph.currencyVertexMap(newSrc)
      println(s"Detected (${newSrc}): $actual")
      val allPossibleTrades = ArbitrageDetector.allIn[Vector](graph, newSrcCcy)
        .calc(amount)
      val allProfitableTradeNums = allPossibleTrades.map(_.last._2).filter(_ > 1.0)
      allProfitableTradeNums should contain (actual)
      printRank(allProfitableTradeNums, actual)
    }
  }

  it should "calculate the proper arbitrage on adjacency matrix" in {
    forAll((minSuccessful(100))) { (jsonStr: String) =>
      println(jsonStr)
      val (currencies, matrix) = FxRatesParser.parseJsonStr[AdjacencyMatrix](jsonStr)
      val srcCcy = "USD"
      val src = currencies.indexOf(srcCcy)
      val amount = 1.0
      val graph = ArbitrageGraph(matrix, currencies)
      val fxTrades = ArbitrageDetector[WrappedArray](graph, src)
        .calc(amount)
        .head
      val (newSrc, actual) = fxTrades.last
      val newSrcCcy = graph.currencyVertexMap(newSrc)
      println(s"Detected (${newSrc}): $actual")
      println(s"${fxTrades.mkString("->")}")
      val allPossibleTrades = ArbitrageDetector
        .allIn[WrappedArray](graph, newSrcCcy)
        .calc(amount)
      val allProfitableTradeNums = allPossibleTrades.map(_.last._2).filter(_ > 1.0)
      allProfitableTradeNums should contain (actual)
      printRank(allProfitableTradeNums, actual)
    }
  }

  it should "calculate the same regardless of the data representation" in {
    forAll((minSuccessful(10))) { (jsonStr: String) =>
      println(jsonStr)
      val (currencies, matrix) = FxRatesParser.parseJsonStr[AdjacencyMatrix](jsonStr)
      val (currencies2, vector) = FxRatesParser.parseJsonStr[AdjacencyVector](jsonStr)
      val srcCcy = "USD"
      currencies shouldEqual currencies2
      val src = currencies.indexOf(srcCcy)
      val amount = 1.0
      val graph = ArbitrageGraph(matrix, currencies)
      val graph2 = ArbitrageGraph(vector, currencies2)
      val fxTrades = ArbitrageDetector[WrappedArray](graph, src)
        .calc(amount)
        .head
      val (_, actual) = fxTrades.last
      val fxTrades2 = ArbitrageDetector[Vector](graph2, src)
        .calc(amount)
        .head
      val (_, actual2) = fxTrades.last
      fxTrades shouldEqual fxTrades2
      actual shouldEqual actual2
    }
  }

  def printRank(allProfitableTradeNums: Seq[Double], actual: Double) = {
    val rank = allProfitableTradeNums.sortWith(_ > _).indexOf(actual) + 1
    println(s"The detected result $actual is the $rank. biggest profitable arbitrage")
  }
}

(new TestArbitrageCalculator).execute()