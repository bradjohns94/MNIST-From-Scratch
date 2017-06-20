package com.mnist.nn

import collection.mutable.{ArrayBuffer, Stack}

import com.mnist.linalg.Matrix

trait Layer {
  val activationFn: Activation = Activations.Identity
  /* Apply the activation function to an entire matrix */
  def activate(x: Matrix) = x.apply(activationFn.fn)
  /* Calculate the activation of the network given the inputs */
  def run(inputs: Matrix): Matrix
  /* Reweight the network based on the inputs and the weighted error */
  def fit(inputs: Matrix, weightedError: Matrix): Matrix
  /* Get the input/output shape of the layer */
  def getShape: (Int, Int)
}

trait LayerBuilder {
  def build(size: Int, activation: Activation): Layer
}

class NeuralNetwork(numInputs: Int, layers: List[Layer], costFn: Cost) {
  /* Verify the size of each layer */
  layers.foldLeft(numInputs) { (lastOutputs, layer) =>
    val (inputSize, layerSize) = layer.getShape
    if (lastOutputs != inputSize)
      throw new IllegalArgumentException(s"Cannot connect layer with ${lastOutputs} outputs to layer with ${inputSize} inputs")
    layerSize
  }

  private def runStack(inputs: Matrix): (Stack[Matrix], Matrix) = {
    layers.foldLeft( (new Stack[Matrix](), inputs) ) { case ((inputStack, layerInputs), layer) =>
      inputStack.push(layerInputs)
      (inputStack, layer.run(layerInputs))
    }
  }

  /* Feed a value into the network and get the result */
  def run(inputs: Matrix): Matrix = runStack(inputs)._2

  /* Train the network based on the expected output and return what it output */
  def fit(inputs: Matrix, expected: Matrix): Matrix = {
    val (inputStack, outputs) = runStack(inputs)
    layers.foldRight(costFn.derivative(expected, outputs)) { (layer, weightedError) =>
      layer.fit(inputStack.pop, weightedError)
    }
    outputs
  }
}
