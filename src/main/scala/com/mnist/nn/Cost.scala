package com.mnist.nn

import com.mnist.linalg.Matrix

trait Cost {
  /* Cost function */
  def fn(expected: Matrix, actual: Matrix): Double
  /* Derivative of the Cost Function in respect to each activation */
  def derivative(expected: Matrix, actual: Matrix): Matrix
}

object Costs {
  /* Mean Squared Error Cost Function: C = (1/2n)Σ{ (y - y')² } */
  object MSE extends Cost {
    def fn(expected: Matrix, actual: Matrix): Double = {
      val sqDiff = (expected - actual)
        .apply {i: Double => Math.pow(i,2)}
        .flatten
      (1.0 / (2.0 * sqDiff.length)) * sqDiff.sum
    }

    /* d(y'((i))/d(C) = (y'(i) - y(i)) / ||y|| */
    def derivative(expected: Matrix, actual: Matrix): Matrix = {
       val diffVec = (actual - expected).flatten
       Matrix.fromVector(diffVec.map{ diff => diff / diffVec.length })
    }
  }

  /* Cross-Entropy Cost Function: C = (-1/n)Σ{ yln(y') + (1 - y)ln(1 - y') } */
  object XEntropy extends Cost {
    def fn(expected: Matrix, actual: Matrix): Double = {
      val zipped = (expected.flatten zip actual.flatten)
      zipped.foldLeft(0.0) { case (cost, (e, a)) => {
        cost + ((e * Math.log(a)) + ((1 - e) * Math.log(1 - a)))
      }} * (-1.0 / zipped.length)
    }

    /* d(y'(i))/d(C) = (1/n)((y(i) - y'(i)) / (y'(i) * (1 - y'(i)))) */
    def derivative(expected: Matrix, actual: Matrix): Matrix = {
      val n = expected.flatten.length
      Matrix.fromVector(
        (expected.flatten zip actual.flatten).map{ case (e, a) =>
          (a - e) / (n * a * (1 - a))
        }
      )
    }
  }
}
