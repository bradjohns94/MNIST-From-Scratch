package com.mnist.nn.layer

import com.mnist.linalg.Matrix
import com.mnist.nn.{Activation, Layer}

class FeedForwardLayer(inputSize: Int, layerSize: Int,
  override val activationFn: Activation,
  initWeights: Option[Matrix], initBiases: Option[Matrix]) extends Layer {

  var weights = initWeights.getOrElse(initializeWeights)
  var biases = initBiases.getOrElse(initializeBiases)

  def initializeWeights: Matrix = new Matrix(inputSize, layerSize).randomize

  def initializeBiases: Matrix = new Matrix(1, layerSize).randomize

  def activate(x: Double): Double = activationFn.fn(x)

  def run(inputs: Matrix): Matrix = {
    if (inputs.getShape != (1, inputSize))
      throw new IllegalArgumentException(s"Input matrix must be of shape (1x${inputSize})")
    activate((inputs * weights) + biases)
  }

  def fit(inputs: Matrix, weightedError: Matrix): Matrix = {
    null // TODO
  }
}
