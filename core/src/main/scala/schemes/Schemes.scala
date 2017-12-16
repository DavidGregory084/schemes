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

import cats.{ Functor, Monad, Traverse, ~> }

object Schemes {
  def cata[F[_], A](fix: Fix[F])(algebra: F[A] => A)(implicit F: Functor[F]): A = {
    def loop(fix: Fix[F]): A = algebra(F.map(Fix.unfix(fix))(loop))
    loop(fix)
  }

  def cataM[M[_], F[_], A](fix: Fix[F])(algebra: F[A] => M[A])(implicit M: Monad[M], T: Traverse[F]): M[A] = {
    def loop(fix: Fix[F]): M[A] = M.flatMap(T.traverse(Fix.unfix(fix))(loop))(algebra)
    loop(fix)
  }

  def ana[F[_], A](a: A)(coalgebra: A => F[A])(implicit F: Functor[F]): Fix[F] = {
    def loop(a: A): Fix[F] = Fix[F](F.map(coalgebra(a))(loop))
    loop(a)
  }

  def anaM[M[_], F[_], A](a: A)(coalgebra: A => M[F[A]])(implicit M: Monad[M], T: Traverse[F]): M[Fix[F]] = {
    def loop(a: A): M[Fix[F]] = M.flatMap(coalgebra(a)) { fa =>
      M.map(T.traverse(fa)(loop))(Fix.apply[F])
    }

    loop(a)
  }

  def hylo[F[_], A, B](a: A)(coalgebra: A => F[A], algebra: F[B] => B)(implicit F: Functor[F]): B = {
    def loop(a: A): B = algebra(F.map(coalgebra(a))(loop))
    loop(a)
  }

  def hyloM[M[_], F[_], A, B](a: A)(coalgebra: A => M[F[A]], algebra: F[B] => M[B])(implicit M: Monad[M], T: Traverse[F]): M[B] = {
    def loop(a: A): M[B] = M.flatMap(coalgebra(a)) { fa =>
      M.flatMap(T.traverse(fa)(loop))(algebra)
    }

    loop(a)
  }

  def prepro[F[_], A](fix: Fix[F])(pre: F ~> F, algebra: F[A] => A)(implicit F: Functor[F]): A = {
    def loop(fixf: Fix[F]): A = {
      val fa = F.map(Fix.unfix(fixf)) { fixf =>
        loop(cata[F, Fix[F]](fixf) { fa =>
          Fix[F](pre(fa))
        })
      }
      algebra(fa)
    }

    loop(fix)
  }

  def postpro[F[_], A](a: A)(coalgebra: A => F[A], post: F ~> F)(implicit F: Functor[F]): Fix[F] = {
    def loop(a: A): Fix[F] = {
      val ffixf = F.map(coalgebra(a)) { aa =>
        ana[F, Fix[F]](loop(aa)) { fixf =>
          post(Fix.unfix(fixf))
        }
      }
      Fix[F](ffixf)
    }

    loop(a)
  }
}
