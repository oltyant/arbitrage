import scala.collection.mutable.WrappedArray

case class FxGraphEdge(
  from: Int,
  to: Int,
  fromCcy: String,
  toCcy: String,
  fxRate: Double
)

trait ArbitrageGraph[M[T] <: IndexedSeq[T]] {
  def vertices: M[Int]
  def edges: M[FxGraphEdge]
  def edge(u: Int, v: Int): FxGraphEdge
  def edgesFrom(u: Int): M[FxGraphEdge]
  def neighbors(u: Int): M[Int]
  def rateAt(source: Int, target: Int): Double
  def rateOf(fromCcy: String, toCcy: String): Double = rateAt(currencyVertexMap(fromCcy), currencyVertexMap(toCcy))
  def currencies: M[String]
  def currencyVertexMap: Map[String, Int]
}

object ArbitrageGraph {

  def apply(g: Vector[Double], ccies: Vector[String]) = new ArbitrageGraph[Vector] {
    require(g.nonEmpty, s"The graph should be non empty")
    require(ccies.size * ccies.size == g.size,
      s"Arbitrage graph init failed: $g does not have the size of currencies * currencies (${ccies.size * ccies.size})")

    val currencies: Vector[String] = ccies
    val vertices: Vector[Int] = (0 until currencies.size).toVector
    val edges: Vector[FxGraphEdge] = for {
      u <- vertices
      v <- vertices
    } yield edge(u, v)
    val currencyVertexMap: Map[String, Int] = currencies.zip(vertices).toMap

    def neighbors(vertex: Int): Vector[Int] = {
      val u = vertex / vertices.size
      val v = vertex % vertices.size
      (for (i <- u until (u + vertices.size) if i != u + v) yield i).toVector
    }
    def edge(u: Int, v: Int) = FxGraphEdge(u, v, currencies(u), currencies(v), rateAt(u, v))
    def edgesFrom(u: Int): Vector[FxGraphEdge] = neighbors(u).map(v => FxGraphEdge(u, v, currencies(u), currencies(v), rateAt(u, v)))
    def rateAt(source: Int, target: Int): Double = g(source * vertices.size + target)
  }

  def apply(g: Array[Array[Double]], ccies: Vector[String]) = new ArbitrageGraph[WrappedArray] {
    require(g.nonEmpty, s"The graph should be non empty")
    require(ccies.size == g.size && g.forall(_.size == ccies.size),
      s"Arbitrage graph init failed: $g does not have the size of currencies * currencies (${ccies.size * ccies.size})")

    val currencies: WrappedArray[String] = ccies.toArray.array
    val vertices: WrappedArray[Int] = (0 until currencies.size).toArray
    val edges: WrappedArray[FxGraphEdge] = for {
      u <- vertices
      v <- vertices
    } yield edge(u, v)
    val currencyVertexMap: Map[String, Int] = currencies.zip(vertices).toMap

    def neighbors(u: Int) = vertices.filterNot(_ == u)
    def edgesFrom(u: Int): WrappedArray[FxGraphEdge] = neighbors(u).map(v => FxGraphEdge(u, v, currencies(u), currencies(v), rateAt(u, v)))
    def edge(u: Int, v: Int) = FxGraphEdge(u, v, currencies(u), currencies(v), rateAt(u, v))
    def rateAt(source: Int, target: Int): Double = g(source)(target)
  }

}
