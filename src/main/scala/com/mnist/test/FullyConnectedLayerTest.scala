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
      /* I did the math. Please just trust me? */
      val expectedWeightedError = Matrix.fromVector(List(
        (0.036 * Activations.Sigmoid.derivative(0.9)) + (0.38 * 0.3 * Activations.Sigmoid.derivative(1.2)),
        (0.072 * Activations.Sigmoid.derivative(0.9)) + (0.19 * Activations.Sigmoid.derivative(1.2)),
        (0.108 * Activations.Sigmoid.derivative(0.9)) + (0.38 * 0.7 * Activations.Sigmoid.derivative(1.2))
      ))
      val expectedWeights = Matrix.from2DVector(List(
        List(0.2 - (0.018 * Activations.Sigmoid.derivative(0.9)), 0.3 - (0.038 * Activations.Sigmoid.derivative(1.2))),
        List(0.4, 0.5),
        List(0.6 - (0.018 * Activations.Sigmoid.derivative(0.9)), 0.7 - (0.038 * Activations.Sigmoid.derivative(1.2)))
      ))
      val expectedBiases = Matrix.fromVector(List(
        0.1 - (0.018 * Activations.Sigmoid.derivative(0.9)),
        0.2 - (0.038 * Activations.Sigmoid.derivative(1.2))
      ))
      val res = layer.fit(inputs, nextError * weightsOut.transpose)
      /* Use approximately equals because floating point math */
      (res ~= expectedWeightedError) &&
      (layer.getWeights ~= expectedWeights) &&
      (layer.getBiases ~= expectedBiases)
    }
  }
}
