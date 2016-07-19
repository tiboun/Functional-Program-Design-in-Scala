package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      val bVal: Double = b.apply()
      bVal * bVal - 4 * a.apply() * c.apply()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val deltaVal: Double = delta.apply()
      deltaVal match {
        case v if v < 0 => Set()
        case 0 => Set(-b.apply()/(2*a.apply()))
        case _ => {
          val sqrtDelta: Double = Math.sqrt(deltaVal)
          val bVal: Double = b.apply()
          val aVal: Double = a.apply()
          Set((bVal + sqrtDelta)/ (2*aVal), (bVal - sqrtDelta)/ (2*aVal))
        }
      }
    }
  }
}
