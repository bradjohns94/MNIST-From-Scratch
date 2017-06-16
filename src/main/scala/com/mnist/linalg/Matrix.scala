package com.mnist.linalg

import java.util.Random

import com.mnist.common.Types._

class Matrix(rows: Int, columns: Int) {
  var mat = Array.ofDim[Double](rows, columns)

  /* Get the size parameters of the Matrix */
  def getNumRows: Int = rows
  def getNumColumns: Int = columns

  /* Get one or all of the rows */
  def getRows: Mat = mat
  def getRow(n: Int): Vec = mat(n)

  /* Get the shape of the matrix */
  def getShape: (Int, Int) = (rows, columns)

  /* Get one or all of the columns */
  def getColumns: Mat = transpose.getRows
  def getColumn(n: Int): Vec = mat.map{_(n)}

  /* Replace an individual element */
  def replace(row: Int, column: Int, value: Double): Matrix = {
    mat(row)(column) = value
    this
  }

  /* Replace an entire row */
  def replaceRow(row: Int, values: Vec): Matrix = {
    values.zipWithIndex.map{ case (v, i) => this.replace(row, i, v) }
    this
  }

  /* Replace an entire column */
  def replaceColumn(column: Int, values: Vec): Matrix = {
    values.zipWithIndex.map{ case (v, i) => this.replace(i, column, v) }
    this
  }

  /* Swap the matrix's rows and columns */
  def transpose: Matrix = Matrix.from2DVector(mat.transpose)

  /* Do element-wise addition between two matricies */
  def +(addend: Matrix): Matrix = {
    Matrix.from2DVector(
      (mat zip addend.getRows).map{ vecs =>
        (vecs._1 zip vecs._2).map{ elements =>
          elements._1 + elements._2
        }
      }
    )
  }

  /* Do element-wise subtraction between two matricies */
  def -(subtrahend: Matrix): Matrix = {
    Matrix.from2DVector(
      (mat zip subtrahend.getRows).map{ vecs =>
        (vecs._1 zip vecs._2).map{ elements =>
          elements._1 - elements._2
        }
      }
    )
  }

  /* Perform matrix multiplication with another Matrix */
  def *(factor: Matrix): Matrix = {
    if (this.getNumColumns != factor.getNumRows)
      throw new IllegalArgumentException(s"Cannot multiply (${this.getNumRows}x${this.getNumColumns}) matrix with (${factor.getNumRows}x${factor.getNumColumns}) matrix.")

    def dotProduct(a: Vec, b: Vec): Double
      = (a zip b).foldLeft(0.0) { (product, pair) => product + (pair._1 * pair._2) }

    Matrix.from2DVector(
      this.getRows.map{ row =>
        factor.getColumns.map{ col =>
          dotProduct(row, col)
        }
      }
    )
  }

  /* Perform matrix multiplcation by a single value */
  def *(factor: Double): Matrix = {
    Matrix.from2DVector(
      this.getRows.map{ row =>
        row.map{ element => element * factor }
      }
    )
  }

  /* Perform Hadamard Multiplication (component-wise) over 2 vectors */
  def *:*(factor: Matrix): Matrix = {
    if (factor.getShape != this.getShape)
      throw new IllegalArgumentException(s"Cannot perform hadamard multiplication between (${this.getRows}x${this.getColumns}) matrix and (${factor.getRows}x${factor.getColumns}) matrix")
    Matrix.from2DVector(
      (this.mat.flatten.toList zip factor.getRows.flatten.toList)
        .map{ pair => pair._1 * pair._2 }.toArray
        .grouped(this.getNumColumns).toArray
    )
  }

  def apply(fn: Double => Double): Matrix =
    Matrix.from2DVector( mat.map{ row =>
      row.map{ e => fn(e) }
    })

  /* Randomize the values in the matrix between 0 and 1 */
  def randomize: Matrix = {
    val r = new Random()
    this.mat = this.mat.map{ row =>
      row.map{ element => r.nextDouble }
    }
    this
  }

  def flatten: Array[Double] = mat.flatten

  /* Neatly print the matrix */
  override def toString: String = (this.getRows.map{ _.mkString("\t") }).mkString("\n")

  /* Compare each value between two matricies */
  def equals(other: Matrix): Boolean = {
    this.getShape == other.getShape &&
    !(this.flatten zip other.flatten).exists{ case (a, b) => a != b }
  }

  /* Compare each value between two matricies to 5 significant figures */
  def ~=(other: Matrix): Boolean = {
    this.getShape == other.getShape &&
    !(this.flatten zip other.flatten).exists{ case (a, b) => Math.abs(a - b) > 0.00001 }
 }

}

object Matrix {
  /* Create a matrix from a vector */
  def fromVector(vec: Vec): Matrix
    = new Matrix(1, vec.length).replaceRow(0, vec)

  /* create a matrix from a matrix */
  def from2DVector(mat: Mat): Matrix = {
    val matrix = new Matrix(mat.length, mat(0).length)
    (mat zipWithIndex).map{ case (row, i) =>
      if (row.length != matrix.getNumColumns)
        throw new IllegalArgumentException("Argument Matrix has Uneven Rows")
      matrix.replaceRow(i, row)
    }
    matrix
  }
}
