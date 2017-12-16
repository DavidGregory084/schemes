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

import cats._

sealed trait ListF[A, B]
case class ConsF[A, B](head: A, tail: B) extends ListF[A, B]
case class NilF[A, B]() extends ListF[A, B]

object ListF {
  implicit def schemesListFFunctor[A]: Functor[ListF[A, ?]] = new Functor[ListF[A, ?]] {
    def map[B, C](fa: ListF[A, B])(f: B => C): ListF[A, C] = fa match {
      case ConsF(head, tail) => ConsF(head, f(tail))
      case nil @ NilF() => nil.asInstanceOf[ListF[A, C]]
    }
  }
}
