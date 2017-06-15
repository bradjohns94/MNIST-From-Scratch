package com.mnist.test

import com.mnist.common.Types._
import com.mnist.linalg.Matrix
import com.mnist.nn.Activations
import com.mnist.nn.layer.FullyConnectedLayer

class FullyConnectedLayerTest extends Suite {
  def generateSampleLayer: FullyConnectedLayer = new FullyConnectedLayer(
    3, 2, Activations.Sigmoid, 0.1,
    Some(genSampleWeights), Some(genSampleBiases)
  )

  def genSampleWeights: Matrix = Matrix.from2DVector(
    List(List(0.2, 0.3), List(0.4, 0.5), List(0.6, 0.7))
  )

  def genSampleBiases: Matrix = Matrix.fromVector(List(0.1, 0.2))

  object TestLayerInit extends Test {
    def testRandomInit: Boolean = {
      val layer = new FullyConnectedLayer(3,2,Activations.Sigmoid,0.1,None,None)
      layer.getWeights.getRows.flatten.exists(_ != 0.0) &&
      layer.getBiases.getRows.flatten.exists(_ != 0.0)
    }

    def testDefinedInit: Boolean = {
      val layer = generateSampleLayer
      (layer.getWeights equals genSampleWeights) && (layer.getBiases equals genSampleBiases)
    }
  }

  object TestNetworkIntegration extends Test {
    def testActivations: Boolean = {
      val layer = generateSampleLayer
      val inputs = List(1, 0, 1)
      val expected = Activations.Sigmoid.fn(Matrix.fromVector(List(0.9, 1.2)))
      layer.run(Matrix.fromVector(inputs)) equals expected
    }

    def testBackProp: Boolean = {
      val layer = generateSampleLayer
      val inputs = Matrix.fromVector(List(1, 0, 1))
      val weightsOut = Matrix.from2DVector( List( List(0.1, 0.2), List(0.3, 0.4) ) )
      val nextError = Matrix.fromVector( List(0.2, 0.8) )
      val weightedError = weightsOut.transpose * nextError
      // error = (weights.transpose * errorOfNextLayer) (*) a(l)
      // error = ( [ 0.1  0.3 ] x [ 0.2  0.8 ] ) (*) Activations.Sigmoid.derivative(inputs * weights + biases)
      //         ( [ 0.2  0.4 ]                )
      // error = [ 0.24 1.20 ] x Activations.Sigmoid.derivative(inputs * weights + biases)
      val z = (inputs * layer.getWeights) + layer.getBiases
      val expectedError = Matrix.fromVector(List(0.24, 1.20)) * Activations.Sigmoid.derivative(z)
      val expectedWeights = layer.getWeights - ((inputs * expectedError) * 0.1)
      val expectedBiases = layer.getBiases - expectedError
      val expectedWeightedError = layer.getWeights.transpose * expectedError
      val resWeightedError = layer.fit(inputs, weightedError)
      (resWeightedError equals expectedWeightedError) &&
      (layer.getWeights equals expectedWeights) &&
      (layer.getBiases equals expectedBiases)
    }
  }
}
