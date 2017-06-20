package com.mnist

import com.mnist.test._

object Main {
  def main(args: Array[String]): Unit = {
    runTests
  }

  def runTests: Unit = {
    new MatrixTest().runTests
    new FullyConnectedLayerTest().runTests
    new FunctionTest().runTests
    new NeuralNetworkTest().runTests
  }
}
