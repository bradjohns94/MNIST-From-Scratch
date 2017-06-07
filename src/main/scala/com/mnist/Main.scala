package com.mnist

import com.mnist.test.MatrixTest

object Main {
  def main(args: Array[String]): Unit = {
    runTests
  }

  def runTests: Unit = {
    new MatrixTest().runTests
  }
}
