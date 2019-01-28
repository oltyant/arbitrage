import $ivy.`org.scala-lang.modules::scala-parser-combinators:1.1.1`
import $file.ArbitrageTypes, ArbitrageTypes._, ArbitrageTypes.Printer._

import scala.util.parsing.combinator.RegexParsers
import scala.io.Source
import scala.annotation.implicitNotFound

@implicitNotFound(msg = "No implicit FX Rates Parser defined for ${T}.")
trait FxRatesParser[T] extends RegexParsers {
  override val skipWhitespace = true

  def ccyGraphPair     : Parser[(CurrencyNames, T)] = "{" ~> repsep(fxRate, ",") <~ "}" ^^ makeGraphInput
  def fxRate           : Parser[CurrencyPairAndRate] = (ccyPair <~ ":") ~ rate ^^ makeFxRate
  def ccyPair          : Parser[CurrencyPair] = "\"" ~> rep1sep(ccy, "_") <~ "\"" ^^ makeCcyPair
  def ccy              : Parser[String] = "[a-zA-Z]+".r
  def rate             : Parser[Double] = "\"" ~> """\d+(\.\d*)?""".r <~ "\"" ^^ makeRate

  def makeFxRate(ccyPairRate: ~[CurrencyPair, Double]) = (ccyPairRate._1._1, ccyPairRate._1._2, ccyPairRate._2)
  def makeCcyPair(ccys: List[String]) = ccys match { case fromCcy :: toCcy :: Nil => (fromCcy, toCcy) }
  def makeRate(rateStr: String) = rateStr.toDouble
  def makeGraphInput(parsedFxData: List[CurrencyPairAndRate]): (Vector[String], T)

  def parsedResult(parserResult: ParseResult[(Vector[String], T)]): (Vector[String], T)
}

object FxRatesImplicits {
  implicit object FxRatesMatrixParser extends FxRatesParser[AdjacencyMatrix] {
    def makeGraphInput(parsedData: List[CurrencyPairAndRate]) = {
      val ccyMap = parsedData.map(_._1).distinct.zipWithIndex.toMap
      val n = ccyMap.size
      val adjMatrix = Array.fill(n)(Array.fill(n)(0.0))
      for ((ccyFrom, ccyTo, rate) <- parsedData) {
        val rowIndex = ccyMap(ccyFrom)
        val colIndex = ccyMap(ccyTo)
        adjMatrix(rowIndex)(colIndex) = -1 * math.log10(rate)
      }
      (ccyMap.keys.toVector, adjMatrix)
    }

    def parsedResult(parserResult: ParseResult[(Vector[String], AdjacencyMatrix)]) = parserResult match {
      case Success(result, _)  =>
        val (currencies, adjMatrix) = result
        printInputArray(currencies, adjMatrix)
        result
      case failure : NoSuccess =>
        throw new Exception(failure.msg)
    }
  }

  implicit object FxRatesVectorParser extends FxRatesParser[AdjacencyVector] {
    def makeGraphInput(parsedData: List[CurrencyPairAndRate]) = {
      val ccyMap = parsedData.map(_._1).distinct.zipWithIndex.toMap
      val size = ccyMap.size
      val adjVector = parsedData.foldLeft(Vector.fill(size * size)(0.0)) {
        case (matrix, (ccyFrom, ccyTo, rate)) =>
          val rowIndex = ccyMap(ccyFrom)
          val colIndex = ccyMap(ccyTo)
          matrix.updated(rowIndex * size + colIndex, -1 * math.log10(rate))
      }
      (ccyMap.keys.toVector, adjVector)
    }

    def parsedResult(parserResult: ParseResult[(Vector[String], AdjacencyVector)]) = parserResult match {
      case Success(result, _) =>
        val (currencies, adjVector) = result
        printInputVector(currencies, adjVector)
        result
      case failure : NoSuccess => throw new Exception(failure.msg)
    }
  }
}

object FxRatesParser extends RegexParsers {
  @throws(classOf[java.io.IOException])
  private def fxRatesJsonStr(url: String) = {
    printUrl(url)
    val resultJson = Source.fromURL(url).mkString
    printJson(resultJson)
    resultJson
  }

  def parseJsonStr[T: FxRatesParser](jsonStr: String) = {
    val parser = implicitly[FxRatesParser[T]]
    val parsedResult = parser.parseAll(parser.ccyGraphPair, jsonStr)
    parser.parsedResult(parsedResult)
  }

  def apply[T: FxRatesParser](url: String) = parseJsonStr[T](fxRatesJsonStr(url))
}