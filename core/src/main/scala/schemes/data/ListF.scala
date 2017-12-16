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
  def apply[A](as: A*): Fix[ListF[A, ?]] =
    Schemes.ana[ListF[A, ?], List[A]](as.toList) {
      case Nil => NilF()
      case h :: t => ConsF(h, t)
    }

  def toList[A](list: Fix[ListF[A, ?]]) =
    Schemes.cata[ListF[A, ?], List[A]](list) {
      case NilF() => Nil
      case ConsF(h, t) => h :: t
    }

  implicit def schemesListFFunctor[A]: Functor[ListF[A, ?]] = new Functor[ListF[A, ?]] {
    def map[B, C](fa: ListF[A, B])(f: B => C): ListF[A, C] = fa match {
      case ConsF(head, tail) => ConsF(head, f(tail))
      case nil @ NilF() => nil.asInstanceOf[ListF[A, C]]
    }
  }
}
