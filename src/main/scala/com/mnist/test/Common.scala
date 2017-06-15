package com.mnist.test

import com.mnist.common.Types._

trait Suite {
  def getTests: List[Class[_]] = this.getClass.getClasses.toList

  def printHeader: Unit = {
    val header = s"Running Test Suite: ${this.getClass.getName}"
    println("#" * header.length)
    println(header)
    println("#" * header.length)
    println()
  }

  def printFooter(passed: Int, total: Int): Unit = {
    val footer = s"Test Suite ${this.getClass.getName} Passed ${passed} of ${total} Tests"
    println()
    println("#" * footer.length)
    println(footer)
    println("#" * footer.length)
    println()
  }

  def runTests: Unit = {
    printHeader
    val res = getTests.foldLeft(0, 0) { (pair, test) =>
      /* XXX This is a HUGE hack - but it works */
      val runTests = test.getMethods.toList.filter{_.getName.toString == "runTests"}
      if (runTests.length != 1)
        throw new IllegalStateException("Unexpeted Module Types")
      val testConstructor = test.getDeclaredConstructor(this.getClass)
      val testInst = testConstructor.newInstance(this)
      val res = runTests(0).invoke(testInst).asInstanceOf[(Int, Int)]
      (pair._1 + res._1, pair._2 + res._2)
    }
    printFooter(res._1, res._1 + res._2)
  }
}

trait Test {
  val defaults = Test.getClass.getMethods.toSet

  def listMethods: List[java.lang.reflect.Method]
    = (this.getClass.getMethods.toSet &~ defaults)
      .toList
      /* Filter generic functions and anonymous functions */
      .filter{ m => (m.getReturnType == true.getClass) && !m.isSynthetic }

  def runTests: (Int, Int) = {
    val methods = listMethods
    val successful = methods.foldLeft(0) { (total, m) =>
      println(s"###Running Test: ${m.getName}###")
      /* m.invoke() literally HAS to return a Boolean */
      val res = try {
        if (m.invoke(this).asInstanceOf[Boolean]) "PASSED" else "FAILED"
      } catch {
        case ex: Exception => {
          println("Traceback:")
          ex.printStackTrace
          "ERROR in"
        }
      }
      println(s"###${res} Test: ${m.getName}###")
      total + (res equals "PASSED")
    }
    (successful, methods.length - successful)
  }
}

object Test extends Test {
  /* This is literally just a hack to get default methods */
}
