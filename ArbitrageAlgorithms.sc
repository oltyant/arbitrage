import $ivy.`org.scala-lang:scala-reflect:2.12.8`
import $file.ArbitrageGraphs
import ArbitrageGraphs._
import $file.ArbitrageTypes, ArbitrageTypes.Printer._

import scala.reflect.runtime.universe._
import scala.collection.mutable.WrappedArray

import scala.annotation.tailrec

trait ArbitrageDetector[M[T] <: IndexedSeq[T]] {
  protected def g: ArbitrageGraph[M]
  protected def src: Int
  protected def decodeRate(rate: Double): Double = math.pow(10, -1 * rate)
  protected def srcIgnorable = true

  def find: Seq[IndexedSeq[FxGraphEdge]]

  def calc(amount: Double = 1.0): Seq[Vector[(String, Double)]] = find.map {
    edges =>
      val srcCcy = srcCcyName(edges.headOption)
      edges.foldLeft(Vector((srcCcy, amount))) {
        case (fxTrades, edge) =>
          val (_, fromAmount) = fxTrades.last
          val newAmount = fromAmount * decodeRate(edge.fxRate)
          val newFxTrades = fxTrades :+ (edge.toCcy, newAmount)
          newFxTrades
      }
  }

  private def srcCcyName(edge: Option[FxGraphEdge]) = edge match {
    case Some(FxGraphEdge(_,_,fromCcy,_,_)) => fromCcy
    case None => g.currencies(src)
  }
}

/**
  * Find all the arbitrages in the graph with combinations and permutations
  * (generating all the different permutations of the vertices, starting and ending with the source).
  * This has an NP complexity so on relatively small |V| this explodes and signifacntly slow down the all in one detecting.
  * This detector's purpose is to support testing so as to make sure that we find valid arbitrage opportunities with other methods.
  * It is guaranteed to find all the arbitrage opportunities
  */
class AllInDetector[M[T] <: IndexedSeq[T]](
  override val g: ArbitrageGraph[M],
  override val src: Int
) extends ArbitrageDetector[M] {

  lazy val find = {
    val allTrades = validTrades(g.vertices.filterNot(_ == src))
    val edges = allTrades
      .map {
        intermediateTrades =>
          val trades = (src +: (intermediateTrades :+ src))
          trades match {
            case _ +: tail if trades.size > 2 =>
              trades.zip(tail).map { case (from, to) => g.edge(from, to) }
            case _ => IndexedSeq()
          }
      }
    edges
  }

  //All levels' combination and then permutation gives all the possible trading activities
  private def validTrades(vertices: IndexedSeq[Int]): Seq[IndexedSeq[Int]] = {
    val resultWrapped = for {
      i <- (vertices.size to 1 by -1)
      combination <- vertices.combinations(i)
    } yield combination.permutations
    resultWrapped.flatten
  }
}

trait BellmanFordDetector[M[T] <: IndexedSeq[T]] extends ArbitrageDetector[M] {
  lazy val find = {
    val (distance, pred) = bellmanFord(initDistance, initPredecessors)
    val arbitrage = detectNegativeCycles(distance) match {
      case Some(edge) => findArbitrage(edge, pred)
      case None => Vector()
    }
    Seq(arbitrage.toIndexedSeq)
  }
  protected def initDistance: M[Double]
  protected def initPredecessors: M[Int]
  protected def bellmanFord(dist: M[Double], pred: M[Int]): (M[Double], M[Int])
  protected def relaxingEdge(edge: FxGraphEdge, d: M[Double], p: M[Int]): (M[Double], M[Int])

  protected def detectNegativeCycles(dist: M[Double]) =
    g.edges.find(edge => dist(edge.to) > dist(edge.from) + edge.fxRate)

  protected def findArbitrage(negativeEdge: FxGraphEdge, pred: Seq[Int]): List[FxGraphEdge] = {
    @tailrec def loop(from: Int, acc: List[Int]): List[Int] = {
      val to = pred(from)
      val srcCcy = g.currencies(src)
      val toCcy = g.currencies(to)
      if (to == src) {
        printClearArbitrage(srcCcy)
        (to :: acc).reverse
      } else if (acc.contains(to) && srcIgnorable) {
        printNonClearArbitrage(srcCcy, toCcy, srcIgnorable)
        acc.reverse.dropWhile(_ != to) :+ to
      } else if (acc.contains(to)) {
        printNonClearArbitrage(srcCcy, srcCcy, srcIgnorable)
        (src :: acc).reverse
      } else {
        loop(to, to :: acc)
      }
    }
    val vertices = loop(src, List(src))
    vertices.zip(vertices.tail).map { case (from, to) => g.edge(from, to) }
  }
}

final class BellmanFordDetectorOnVector(
  override val g: ArbitrageGraph[Vector],
  override val src: Int,
  override val srcIgnorable: Boolean
) extends BellmanFordDetector[Vector] {

  override val initDistance = g.vertices.map(v => if (v == src) 0.0 else Double.MaxValue)
  override val initPredecessors = g.vertices.map(_ => -1)

  protected def bellmanFord(dist: Vector[Double], pred: Vector[Int]) =
    Stream.iterate((dist, pred)) {
      case (d, p) =>
        g.edges.foldLeft((d, p)) {
          case ((distance, predecessors), edge) =>
            relaxingEdge(edge, distance, predecessors)
        }
    }.take(g.vertices.size).lastOption.getOrElse((dist, pred))

  override def relaxingEdge(edge: FxGraphEdge, d: Vector[Double], p: Vector[Int]) = {
    val edgeDist = edge.fxRate + d(edge.from)
    if (d(edge.to) > edgeDist)
      (d.updated(edge.to, edgeDist), p.updated(edge.to, edge.from))
    else
      (d, p)
  }
}


final class BellmanFordDetectorOnArray(
  override val g: ArbitrageGraph[WrappedArray],
  override val src: Int,
  override val srcIgnorable: Boolean
) extends BellmanFordDetector[WrappedArray] {

  override val initDistance = g.vertices.map(v => if (v == src) 0 else Double.MaxValue)
  override val initPredecessors = g.vertices.map(_ => -1)

  protected def bellmanFord(dist: WrappedArray[Double], pred: WrappedArray[Int]) = {
    for {
      v <- g.vertices
      edge <- g.edges
    } relaxingEdge(edge, dist, pred)
    (dist, pred)
  }

  override def relaxingEdge(edge: FxGraphEdge, d: WrappedArray[Double], p: WrappedArray[Int]) = {
    val edgeDist = edge.fxRate + d(edge.from)
    if (d(edge.to) > edgeDist) {
      d(edge.to) = edgeDist
      p(edge.to) = edge.from
    }
    (d, p)
  }
}


object ArbitrageDetector {
  def apply[M[T] <: IndexedSeq[T]](graph: ArbitrageGraph[M], source: Int, srcIgnorable: Boolean = true)
    (implicit tag: TypeTag[ArbitrageGraph[M]]) =
    typeOf[ArbitrageGraph[M]] match {
      case t if t =:= typeOf[ArbitrageGraph[Vector]] =>
        val g = graph.asInstanceOf[ArbitrageGraph[Vector]]
        new BellmanFordDetectorOnVector(g, source, srcIgnorable)
      case t if t =:= typeOf[ArbitrageGraph[WrappedArray]] =>
        val g = graph.asInstanceOf[ArbitrageGraph[WrappedArray]]
        new BellmanFordDetectorOnArray(g, source, srcIgnorable)
      case u => throw new UnsupportedOperationException(s"$u is a not supported type for initializing Bellman-Ford Detector")
    }

  def allIn[M[T] <: IndexedSeq[T]](graph: ArbitrageGraph[M], source: Int)
    (implicit tag: TypeTag[ArbitrageGraph[M]]) =
    typeOf[ArbitrageGraph[M]] match {
      case t if t =:= typeOf[ArbitrageGraph[Vector]] =>
        val g = graph.asInstanceOf[ArbitrageGraph[Vector]]
        new AllInDetector[Vector](g, source)
      case t if t =:= typeOf[ArbitrageGraph[WrappedArray]] =>
        val g = graph.asInstanceOf[ArbitrageGraph[WrappedArray]]
        new AllInDetector[WrappedArray](g, source)
      case u => throw new UnsupportedOperationException(s"$u is a not supported type for initializing AllInDetector")
    }
}
