package com.mnist.common

package object Types {
  type Vec = Array[Double]
  type Mat = Array[Vec]

  implicit def intToDouble(i: Int): Double = i.toDouble
  implicit def intArrayToVec(a: Array[Int]): Vec
    = a.map{intToDouble(_)}
  implicit def intSeqToVec(s: Seq[Int]): Vec
    = s.map{intToDouble(_)}.toArray
  implicit def intArrayToMat(a: Array[Array[Int]]): Mat
    = a.map{ _.map{ v => intToDouble(v) } }
  implicit def intSeqToMat(s: Seq[Seq[Int]]): Mat
    = s.map{ _.map{ v => intToDouble(v) }.toArray }.toArray
  implicit def boolToInt(b: Boolean) = if (b) 1 else 0
}
