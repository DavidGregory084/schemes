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

trait FixModule {
  type Fix[F[_]]

  def apply[F[_]](f: F[schemes.Fix[F]]): Fix[F]
  def unfix[F[_]](f: Fix[F]): F[schemes.Fix[F]]
}

private[schemes] object FixImpl extends FixModule {
  type Fix[F[_]] = F[schemes.Fix[F]]

  def apply[F[_]](f: F[schemes.Fix[F]]): Fix[F] = f
  def unfix[F[_]](f: Fix[F]): F[schemes.Fix[F]] = f
}
