package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    Signal { b() * b() - 4 * a() * c() }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      def root: Double => Double = calculateRoot(a, b, delta)

      if(delta() < 0) Set()
      else if(delta() == 0) Set(root(1D))
      else Set(root(1D), root(2D))
    }

  private def calculateRoot(a: Signal[Double], b: Signal[Double], delta: Signal[Double])(exp: Double): Double =
    (-b() + Math.pow(-1, exp) * Math.sqrt(delta())) / 2 * a()
}
