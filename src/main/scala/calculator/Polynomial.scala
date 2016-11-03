package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    import math.pow
    Signal(pow(b(),2) - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      import math.sqrt
      if(delta() < 0 || a() == 0) Set()
      else Set((-b()+sqrt(delta()))/(2*a())) ++ Set((-b()-sqrt(delta()))/(2*a()))
    }
  }
}
