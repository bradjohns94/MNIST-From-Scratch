package com.mnist.test

import com.mnist.common.Types._
import com.mnist.linalg.Matrix
import com.mnist.nn.{Activations, Costs}

class FunctionTest extends Suite {
  def approxEquals(a: Double, b: Double): Boolean
    = Math.abs(a - b) < 0.00001

  /* Test Activation Functions */
  object TestActivationFunctions extends Test {
    def testSigmoidActivation: Boolean = {
      approxEquals(Activations.Sigmoid.fn(1), 0.731058578630049) &&
      approxEquals(Activations.Sigmoid.fn(0), 0.5) &&
      approxEquals(Activations.Sigmoid.fn(0.5), 0.622459331201854)
    }

    def testSigmoidDerivative: Boolean = {
      approxEquals(Activations.Sigmoid.derivative(1), 0.19661193324148185) &&
      approxEquals(Activations.Sigmoid.derivative(0), 0.25)
      approxEquals(Activations.Sigmoid.derivative(0.5), 0.2350037122015945)
    }
  }

  /* Test Cost Functions */
  object TestCostFunctions extends Test {
    def testMSECost: Boolean = {
      val actual = Matrix.fromVector(List(8,6,7,5))
      val expected = Matrix.fromVector(List(3,0,9,0))
      approxEquals(Costs.MSE.fn(expected, actual), 11.25)
    }

    def testMSEDerivative: Boolean = {
      val actual = Matrix.fromVector(List(8,6,7,5))
      val expected = Matrix.fromVector(List(3,0,9,0))
      val expectedRes = Matrix.fromVector(List(1.25,1.5,-0.5,1.25))
      Costs.MSE.derivative(expected, actual) ~= expectedRes
    }

    /* NOTE: X Entropy Expects a value between 0 and 1 */
    def testXEntropyCost: Boolean = {
      val actual = Matrix.fromVector(List(0.8,0.6,0.7,0.5))
      val expected = Matrix.fromVector(List(0.3,0.01,0.9,0.01))
      approxEquals(Costs.XEntropy.fn(expected, actual), 0.8100843988571511)
    }

    def testXEntropyDerivative: Boolean = {
      val actual = Matrix.fromVector(List(0.8,0.6,0.7,0.5))
      val expected = Matrix.fromVector(List(0.3,0.01,0.9,0.01))
      val expectedRes = Matrix.fromVector(List(
        0.7812500000000001,
        0.6145833333333334,
        -0.23809523809523817,
        0.49
      ))
      Costs.XEntropy.derivative(expected, actual) ~= expectedRes
    }
  }
}
