/*
 * Copyright 2017 David Gregory and the Schemes project contributors
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

package schemes

import cats.{ Functor, Monad, Traverse }

object Schemes {
  def cata[F[_], A](fix: Fix[F])(algebra: F[A] => A)(implicit F: Functor[F]): A = {
    var fn: Fix[F] => A = null
    fn = f => algebra(F.map(Fix.unfix(f))(fn))
    fn(fix)
  }

  def cataM[M[_], F[_], A](fix: Fix[F])(algebra: F[A] => M[A])(implicit M: Monad[M], T: Traverse[F]): M[A] = {
    var fn: Fix[F] => M[A] = null
    fn = f => M.flatMap(T.traverse(Fix.unfix(f))(fn))(algebra)
    fn(fix)
  }

  def ana[F[_], A](a: A)(coalgebra: A => F[A])(implicit F: Functor[F]): Fix[F] = {
    var fn: A => Fix[F] = null
    fn = aa => Fix.fix[F](F.map(coalgebra(aa))(fn))
    fn(a)
  }

  def anaM[M[_], F[_], A](a: A)(coalgebra: A => M[F[A]])(implicit M: Monad[M], T: Traverse[F]): M[Fix[F]] = {
    var fn: A => M[Fix[F]] = null
    fn = aa => M.flatMap(coalgebra(aa)) { fa =>
      M.map(T.traverse(fa)(fn))(Fix.fix[F])
    }
    fn(a)
  }

  def hylo[F[_], A, B](a: A)(coalgebra: A => F[A], algebra: F[B] => B)(implicit F: Functor[F]): B = {
    var fn: A => B = null
    fn = aa => algebra(F.map(coalgebra(aa))(fn))
    fn(a)
  }

  def hyloM[M[_], F[_], A, B](a: A)(coalgebra: A => M[F[A]], algebra: F[B] => M[B])(implicit M: Monad[M], T: Traverse[F]): M[B] = {
    var fn: A => M[B] = null
    fn = aa => M.flatMap(coalgebra(aa)) { fa =>
      M.flatMap(T.traverse(fa)(fn))(algebra)
    }
    fn(a)
  }
}
