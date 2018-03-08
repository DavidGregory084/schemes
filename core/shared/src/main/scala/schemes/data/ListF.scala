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
package data

import cats._

sealed abstract class ListF[A, B]
final case class ConsF[A, B](head: A, tail: B) extends ListF[A, B]
final case class NilF[A, B]() extends ListF[A, B]

object ListF {
  def cons[A](head: A, tail: Fix[ListF[A, ?]]): Fix[ListF[A, ?]] =
    Fix(ConsF(head, tail))

  def nil[A]: Fix[ListF[A, ?]] =
    Fix(NilF())

  def apply[A](as: A*): Fix[ListF[A, ?]] =
    Schemes.ana[ListF[A, ?], List[A]](as.toList) {
      case Nil => NilF()
      case h :: t => ConsF(h, t)
    }

  def toList[A](list: Fix[ListF[A, ?]]): List[A] =
    Schemes.cata[ListF[A, ?], List[A]](list) {
      case NilF() => Nil
      case ConsF(h, t) => h :: t
    }

  implicit def schemesListFEq[A, B](implicit A: Eq[A], B: Eq[B]) = new Eq[ListF[A, B]] {
    def eqv(l: ListF[A, B], r: ListF[A, B]) = (l, r) match {
      case (NilF(), NilF()) => true
      case (NilF(), _) => false
      case (_, NilF()) => false
      case (ConsF(la, lb), ConsF(ra, rb)) =>
        A.eqv(la, ra) && B.eqv(lb, rb)
    }
  }

  implicit def schemesListFTraverse[A]: Traverse[ListF[A, ?]] = new Traverse[ListF[A, ?]] {
    override def map[B, C](fa: ListF[A, B])(f: B => C): ListF[A, C] = fa match {
      case ConsF(head, tail) => ConsF(head, f(tail))
      case nil @ NilF() => nil.asInstanceOf[ListF[A, C]]
    }

    def foldLeft[B, C](fa: ListF[A, B], c: C)(f: (C, B) => C): C =
      fa match {
        case ConsF(_, b) => f(c, b)
        case NilF() => c
      }

    def foldRight[B, C](fa: ListF[A, B], ec: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] =
      fa match {
        case ConsF(_, b) => f(b, ec)
        case NilF() => ec
      }

    def traverse[F[_], B, C](fa: ListF[A, B])(f: B => F[C])(implicit F: Applicative[F]): F[ListF[A, C]] = fa match {
      case ConsF(a, b) => F.map(f(b))(c => ConsF(a, c))
      case NilF() => F.pure(NilF())
    }
  }
}
