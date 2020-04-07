package org.wartremover
package contrib.test

import org.scalatest.funsuite.AnyFunSuite
import org.wartremover.contrib.warts.FutureSequence
import org.wartremover.test.WartTestTraverser

import scala.concurrent.Future

class FutureSequenceTest extends AnyFunSuite with ResultAssertions {
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  test("warn if Future.sequence is used ") {
    val result = WartTestTraverser(FutureSequence) {
      val f1 = Future.successful(1)
      val f2 = Future.successful(2)
      Future.sequence(Seq(f1, f2))
    }
    assertWarnings(result)(FutureSequence.message, 1)
  }

  test("can suppress warnings") {
    val result = WartTestTraverser(FutureSequence) {
      @SuppressWarnings(Array("org.wartremover.contrib.warts.FutureSequence"))
      def m() = {
        val f1 = Future.successful(1)
        val f2 = Future.successful(2)
        Future.sequence(Seq(f1, f2))
      }
    }
    assertEmpty(result)
  }

}
