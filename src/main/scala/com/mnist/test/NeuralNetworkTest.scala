package com.mnist.test

import com.mnist.common.Types._
import com.mnist.linalg.Matrix
import com.mnist.nn._
import com.mnist.nn.layer.FullyConnectedLayer

class NeuralNetworkTest extends Suite {
  def sumDifference(a: Matrix, b: Matrix): Double =
    (a - b).flatten.foldLeft(0.0) { (sum, diff) =>
      sum + Math.abs(diff)
    }

  object TestFullyConnectedNetwork extends Test {
    // TODO define a weight initialization so this test is deterministic
    def testXORNetwork: Boolean = {
      val dataset = List(
        (Matrix.fromVector(List(0,0)), Matrix.fromVector(List(0))),
        (Matrix.fromVector(List(0,1)), Matrix.fromVector(List(1))),
        (Matrix.fromVector(List(1,0)), Matrix.fromVector(List(1))),
        (Matrix.fromVector(List(1,1)), Matrix.fromVector(List(0)))
      )
      val layers = List(
        new FullyConnectedLayer(2, 2, Activations.Sigmoid, 0.1, None, None),
        new FullyConnectedLayer(2, 1, Activations.Sigmoid, 0.1, None, None)
      )
      val nn = new NeuralNetwork(2, layers, Costs.MSE)
      (0 to 10000).foreach{ _ => dataset.foreach{ case (input, output) =>
        nn.fit(input, output)
      }}
      !dataset.exists{ case (input, output) =>
        sumDifference(nn.run(input), output) > 0.1
      }
    }
  }
}
