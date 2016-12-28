package org.zalando.grafter

import org.specs2.Specification
import cats.data._
import TransitiveReader._
import shapeless._

class TransitiveReaderSpec extends Specification { def is = s2"""
    If we have Reader[A, B], Reader[B, C] and Reader[C, D], then Reader[A, D]
    can be retrieved automatically $transitiveReader

    Direct transitive reader can be obtained $directTransitiveReader
  """

  def transitiveReader = {
    val a2d: TransitiveReader[A, D] = the[TransitiveReader[A, D]]

    a2d.reader.run(A(1)) === D(4)
  }

  def directTransitiveReader = {
    val a2d: TransitiveReader[A, B] = the[TransitiveReader[A, B]]

    a2d.reader.run(A(1)) === B(2)
  }

  // Simulate grafter implicit definitions structure (implicits are in the companion objects)
  // because it is important to check that implicit resolution can be performed in that conditions.

  case class A(n: Int)
  object A {
    implicit def reader: Reader[A, B] = Reader(x => B(x.n + 1))
  }

  case class B(n: Int)
  object B {
    implicit def reader: Reader[B, C] = Reader(x => C(x.n + 1))
  }

  case class C(n: Int)

  case class D(n: Int)
  object D {
    implicit def reader: Reader[C, D] = Reader(x => D(x.n + 1))
  }
}
