package com.mnist.nn

import com.mnist.linalg.Matrix

trait Activation {
  /* Activation Function */
  def fn(x: Double): Double
  def fn(x: Matrix): Matrix = x.apply(fn)
  /* Derivative of the Activation Function */
  def derivative(x: Double): Double
  def derivative(x: Matrix): Matrix = x.apply(derivative)
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
