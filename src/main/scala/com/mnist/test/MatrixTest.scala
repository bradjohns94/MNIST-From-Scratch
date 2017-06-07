package com.mnist.test

import com.mnist.common.Types._
import com.mnist.linalg.Matrix

class MatrixTest extends Suite {
  /* Matrix/List equality check for convenience */
  def matrixListEquals(mat: Matrix, l: Mat): Boolean = {
    !(mat.getRows zip l).exists{ pair =>
      (pair._1 zip pair._2).exists{ vals => vals._1 != vals._2 }
    }
  }

  /* Test various methods of creating a matrix */
  object TestMatrixConstructors extends Test {
    def testEmptyInit: Boolean = {
      val matrix = new Matrix(3,3)
      val passValues = !matrix.getRows.exists{ row =>
        row.exists{ e => e != 0 }
      }
      passValues && matrix.getNumRows == 3 && matrix.getNumColumns == 3
    }

    def testFromVector: Boolean = {
      val sample = List(1,2,3)
      val matrix = Matrix.fromVector(sample)
      matrixListEquals(matrix, List(sample))
    }

    def testFrom2DVector: Boolean = {
      val sample = List(List(1,2,3), List(4,5,6), List(7,8,9))
      val matrix = Matrix.from2DVector(sample)
      matrixListEquals(matrix, sample)
    }
  }

  /* Test matrix addition, subtraction, and multiplication */
  object TestMatrixMath extends Test {
    def testMatrixAddition: Boolean = {
      val mat1 = Matrix.from2DVector(List(List(1,2,3), List(4,5,6)))
      val mat2 = Matrix.from2DVector(List(List(4,5,6), List(7,8,9)))
      val res = mat1 + mat2
      val expected = List(List(5,7,9), List(11, 13, 15))
      matrixListEquals(res, expected)
    }

    def testMatrixSubtraction: Boolean = {
      val mat1 = Matrix.from2DVector(List(List(4,5,6), List(7,8,9)))
      val mat2 = Matrix.from2DVector(List(List(1,2,3), List(4,5,6)))
      val res = mat1 - mat2
      val expected = List(List(3,3,3), List(3,3,3))
      matrixListEquals(res, expected)
    }

    def testMatrixMultiplication: Boolean = {
      val mat1 = Matrix.from2DVector(List(List(1,2), List(3,4), List(5,6)))
      val mat2 = Matrix.from2DVector(List(List(1,2,3,4), List(5,6,7,8)))
      val res = mat1 * mat2
      val expected = List(List(11,14,17,20), List(23,30,37,44), List(35,46,57,68))
      matrixListEquals(res, expected)
    }
  }

  def getClasses: List[Class[_]] = this.getClass.getClasses.toList
}
