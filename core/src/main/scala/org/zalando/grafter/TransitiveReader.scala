package org.zalando.grafter

import cats.data._

sealed trait TransitiveReader[F, T] {
  def reader: Reader[F, T]
}

object TransitiveReader {
  def apply[F, T](implicit tr: TransitiveReader[F, T]): Reader[F, T] = tr.reader

  implicit def link[F, T](implicit r: Reader[F, T]): Link[F, T] = Link(r)

  implicit def rightChain[F, B, T](implicit
    l: Reader[B, T],
    tr: TransitiveReader[F, B]
  ): RightChain[F, B, T] = RightChain(tr, l)

  implicit def leftChain[F, B, T](implicit
    l: Reader[F, B],
    tr: TransitiveReader[B, T]
  ): LeftChain[F, B, T] = LeftChain(l, tr)
}

final case class Link[F, T](reader: Reader[F, T]) extends TransitiveReader[F, T]

sealed trait Chain[F, B, T] extends TransitiveReader[F, T]

final case class RightChain[F, B, T](tr: TransitiveReader[F, B], l: Reader[B, T]) extends Chain[F, B, T] {
  override def reader: Reader[F, T] = tr.reader.andThen(l)
}

final case class LeftChain[F, B, T](l: Reader[F, B], tr: TransitiveReader[B, T]) extends Chain[F, B, T] {
  override def reader: Reader[F, T] = l.andThen(tr.reader)
}
