package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    val bVal = b()
    Signal((bVal * bVal) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val delta = computeDelta(a, b, c)
    Signal(
      if (delta() < 0) Set()
      else {
        // (-b ± √Δ) / 2a
        val bVal = b()
        val aTimes2 = 2 * a()
        val squareRoot = scala.math.sqrt(delta())
        val x1 = (-1 * bVal + squareRoot) / aTimes2
        val x2 = (-1 * bVal - squareRoot) / aTimes2
        Set(x1, x2)
      }
    )
  }
}
