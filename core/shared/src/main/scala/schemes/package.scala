/*
 * Copyright 2018 David Gregory and the Schemes project contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import cats.{ ~>, Functor, Monad, Traverse }
import cats.free.Cofree

package object schemes {
  val Fix: FixModule = FixImpl
  type Fix[F[_]] = Fix.Fix[F]

  implicit class AnyOps[A](private val a: A) extends AnyVal {
    def ana[F[_]](coalgebra: A => F[A])(implicit F: Functor[F]): Fix[F] =
      Schemes.ana(a)(coalgebra)

    def anaM[M[_], F[_]](coalgebra: A => M[F[A]])(implicit M: Monad[M], T: Traverse[F]): M[Fix[F]] =
      Schemes.anaM(a)(coalgebra)

    def hylo[F[_], B](coalgebra: A => F[A], algebra: F[B] => B)(implicit F: Functor[F]): B =
      Schemes.hylo(a)(coalgebra, algebra)

    def hyloM[M[_], F[_], B](coalgebra: A => M[F[A]], algebra: F[B] => M[B])(implicit M: Monad[M], T: Traverse[F]): M[B] =
      Schemes.hyloM(a)(coalgebra, algebra)

    def postpro[F[_]](coalgebra: A => F[A], post: F ~> F)(implicit F: Functor[F]): Fix[F] =
      Schemes.postpro(a)(coalgebra, post)

    def elgot[F[_], B](elgotCoalgebra: A => Either[B, F[A]], algebra: F[B] => B)(implicit F: Functor[F]): B =
      Schemes.elgot(a)(elgotCoalgebra, algebra)

    def coelgot[F[_], B](coalgebra: A => F[A], elgotAlgebra: (A, () => F[B]) => B)(implicit F: Functor[F]): B =
      Schemes.coelgot(a)(coalgebra, elgotAlgebra)

    def apo[F[_]](coalgebra: A => F[Either[Fix[F], A]])(implicit F: Functor[F]): Fix[F] =
      Schemes.apo(a)(coalgebra)
  }

  implicit class FixOps[F[_]](private val fix: Fix[F]) extends AnyVal {
    def unfix: F[Fix[F]] = Fix.unfix(fix)

    def cata[A](algebra: F[A] => A)(implicit F: Functor[F]): A =
      Schemes.cata(fix)(algebra)

    def cataM[M[_], A](algebra: F[A] => M[A])(implicit M: Monad[M], T: Traverse[F]): M[A] =
      Schemes.cataM(fix)(algebra)

    def prepro[A](pre: F ~> F, algebra: F[A] => A)(implicit F: Functor[F]): A =
      Schemes.prepro(fix)(pre, algebra)

    def histo[A](algebra: F[Cofree[F, A]] => A)(implicit F: Functor[F]): A =
      Schemes.histo(fix)(algebra)
  }
}
