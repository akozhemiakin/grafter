package org.zalando.grafter

import shapeless._
import shapeless.labelled._
import cats.data._

/**
 * The following functions are used to
 * derive reader instances for case classes
 * through the generic deriving mechanism from Shapeless
 *
 * See this blog post for more details: https://meta.plasm.us/posts/2015/11/08/type-classes-and-generic-derivation
 */
trait GenericReader {

  implicit def hnilReader[R]: Reader[R, HNil] =
    Reader(_ => HNil)

  implicit def hconsReader[R, K <: Symbol, H, T <: HList](implicit
                                                           key: Witness.Aux[K],
                                                           readerHead: Lazy[TransitiveReader[R, H]],
                                                           readerTail: Lazy[Reader[R, T]]
  ): Reader[R, FieldType[K, H] :: T] =
    Reader((r: R) => field[K](readerHead.value.reader(r)) :: readerTail.value(r))

  implicit def genericReader[R, A, Repr](implicit
    gen: LabelledGeneric.Aux[A, Repr],
    repr: Lazy[Reader[R, Repr]]
  ): Reader[R, A] =
    Reader((r: R) => gen.from(repr.value(r)))

  /**
   * this implicit conversion is useful to get contravariance for Reader instances
   * because cats' Reader is not contravariant and should be:
   *
   * If you have a Reader[C, Apple] you also have a Reader[C, Fruit] when Apple <: Fruit
   */
  implicit def widenReader[R, A, B](r: Reader[R, A])(implicit ev: A <:< B): Reader[R, B] =
    r.map(a => ev(a))

}

object GenericReader extends GenericReader {

  def apply[R, A](implicit ev: Reader[R, A]): Reader[R, A] =
    ev

}
