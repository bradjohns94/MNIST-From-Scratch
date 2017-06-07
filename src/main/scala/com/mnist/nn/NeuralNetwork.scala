package com.mnist.nn

import com.mnist.linalg.Matrix

trait Layer {
  val activationFn: Activation = Activations.Identity
  /* Apply the activation function to an entire matrix */
  def activate(x: Matrix) = x.apply(activationFn.fn)
  /* Calculate the activation of the network given the inputs */
  def run(inputs: Matrix): Matrix
  /* Reweight the network based on the inputs and the weighted error */
  def fit(inputs: Matrix, weightedError: Matrix): Matrix
}

class NeuralNetwork {

}
