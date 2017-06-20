package com.mnist.nn.layer

import com.mnist.linalg.Matrix
import com.mnist.nn.{Activation, Layer}

class FullyConnectedLayer(inputSize: Int, layerSize: Int,
  override val activationFn: Activation, learningRate: Double,
  initWeights: Option[Matrix], initBiases: Option[Matrix]) extends Layer {

  var weights = initWeights.getOrElse(initializeWeights)
  var biases = initBiases.getOrElse(initializeBiases)

  // TODO add actual weight initializations that are statistically relevant
  def initializeWeights: Matrix = new Matrix(inputSize, layerSize).randomize

  // TODO add actual bias initializations that are statistically relevant
  def initializeBiases: Matrix = new Matrix(1, layerSize).randomize

  def activate(x: Double): Double = activationFn.fn(x)

  /* Calculate the pre-activation output value Z */
  private def calcZ(inputs: Matrix): Matrix = {
    if (inputs.getShape != (1, inputSize))
      throw new IllegalArgumentException(s"Input matrix must be of shape (1x${inputSize}) (got ${inputs.getShape})")
    (inputs * weights) + biases
  }

  def run(inputs: Matrix): Matrix = {
    activate(calcZ(inputs))
  }

  /* Reweight the weights and biases of the layer and return the weighted error
   * for the previous layer */
  def fit(inputs: Matrix, weightedError: Matrix): Matrix = {
    /* *:* -> component multiplication */
    val error = weightedError *:* activationFn.derivative(calcZ(inputs))
    val res = error * this.weights.transpose
    this.biases -= error * learningRate
    this.weights -= (inputs.transpose * error) * learningRate
    res
  }

  def getWeights: Matrix = weights
  def getBiases: Matrix = biases
  def getShape: (Int, Int) = (inputSize, layerSize)
}
