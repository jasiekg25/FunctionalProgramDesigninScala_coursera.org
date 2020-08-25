package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] =
    Signal {
      val val_a = a()
      val val_b = b()
      val val_c = c()
      (val_b * val_b) - (4 * val_a * val_c)
    }


  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal{
    val a_val = a()
    val b_val = b()
    val delta_val = computeDelta(a, b, c)
    delta_val() match {
      case d if (d < 0) =>
        Set()
      case d if (d == 0) =>
        val x0 = ((-b_val) / (2 * a_val))
        Set(x0)
      case d if (d > 0) =>
        val x1 = (-b_val + Math.sqrt(d)) / (2 * a_val)
        val x2 = (-b_val - Math.sqrt(d)) / (2 * a_val)
        Set(x1, x2)
    }
  }
}
