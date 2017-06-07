package com.mnist.nn

trait Activation {
  /* Activation Function */
  def fn(x: Double): Double
  /* Derivative of the Activation Function */
  def derivative(x: Double): Double
}

object Activations {
  /* Identity Activation Function: a = x */
  object Identity extends Activation {
    def fn(x: Double): Double = x
    def derivative(x: Double) = 1
  }

  /* Sigmoid Activation Function: a = 1 / (1 + e^(-x)) */
  object Sigmoid extends Activation {
    def fn(x: Double) = 1.0 / (1.0 + Math.pow(Math.E, (-1.0 * x)))
    def derivative(x: Double): Double = fn(x) * (1 - fn(x))
  }
}
