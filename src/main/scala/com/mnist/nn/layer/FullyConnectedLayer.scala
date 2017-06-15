package com.mnist.nn.layer

import com.mnist.linalg.Matrix
import com.mnist.nn.{Activation, Layer}

class FullyConnectedLayer(inputSize: Int, layerSize: Int,
  override val activationFn: Activation, learningRate: Double,
  initWeights: Option[Matrix], initBiases: Option[Matrix]) extends Layer {

  var weights = initWeights.getOrElse(initializeWeights)
  var biases = initBiases.getOrElse(initializeBiases)

  def initializeWeights: Matrix = new Matrix(inputSize, layerSize).randomize

  def initializeBiases: Matrix = new Matrix(1, layerSize).randomize

  def activate(x: Double): Double = activationFn.fn(x)

  /* Calculate the pre-activation output value Z */
  private def calcZ(inputs: Matrix): Matrix = {
    if (inputs.getShape != (1, inputSize))
      throw new IllegalArgumentException(s"Input matrix must be of shape (1x${inputSize})")
    (inputs * weights) + biases
  }

  def run(inputs: Matrix): Matrix = activate(calcZ(inputs))

  /* Reweight the weights and biases of the layer and return the weighted error
   * for the previous layer */
  def fit(inputs: Matrix, weightedError: Matrix): Matrix = {
    /* *:* -> component multiplication */
    val error = weightedError *:* activationFn.derivative(calcZ(inputs)).transpose
    val res = this.weights.transpose * error
    this.biases -= error.transpose * learningRate
    this.weights -= (inputs * error) * learningRate
    res
  }

  def getWeights: Matrix = weights
  def getBiases: Matrix = biases
}
