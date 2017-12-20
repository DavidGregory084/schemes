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
    def loop(fix: Fix[F]): A = {
      // Peel off a layer of Fix to get to the F inside
      val peeled = Fix.unfix(fix)
      // Apply the algebra to all the layers beneath this one
      val prepared = F.map(peeled)(loop)
      // Finally apply the algebra to this layer
      algebra(prepared)
    }

    loop(fix)
  }

  def cataM[M[_], F[_], A](fix: Fix[F])(algebra: F[A] => M[A])(implicit M: Monad[M], T: Traverse[F]): M[A] = {
    def loop(fix: Fix[F]): M[A] = {
      // Peel off a layer of Fix to get to the F inside
      val peeled = Fix.unfix(fix)
      // Traverse the layers below this to apply the transformation
      val prepared = T.traverse(peeled)(loop)
      // Finally apply the transformation to this layer
      M.flatMap(prepared)(algebra)
    }
    loop(fix)
  }

  def ana[F[_], A](a: A)(coalgebra: A => F[A])(implicit F: Functor[F]): Fix[F] = {
    def loop(a: A): Fix[F] = {
      // Unfold the current seed into a layer of F containing the next seed using the coalgebra
      val currentLayer = coalgebra(a)
      // Unfold each inner seed A into a subsequent new layer of F recursively
      val allLayers = F.map(currentLayer)(loop)
      // Finish off this layer with a topping of Fix
      Fix[F](allLayers)
    }

    loop(a)
  }

  def anaM[M[_], F[_], A](a: A)(coalgebra: A => M[F[A]])(implicit M: Monad[M], T: Traverse[F]): M[Fix[F]] = {
    def loop(a: A): M[Fix[F]] = {
      // Unfold the current seed into a layer of F containing the next seed using the coalgebra
      val currentLayer = coalgebra(a)
      M.flatMap(currentLayer) { fa =>
        // Unfold each inner seed A into a subsequent new layer of F recursively
        val allLayers = T.traverse(fa)(loop)
        // Finish off this layer with a topping of Fix
        M.map(allLayers)(Fix.apply[F])
      }
    }

    loop(a)
  }

  def hylo[F[_], A, B](a: A)(coalgebra: A => F[A], algebra: F[B] => B)(implicit F: Functor[F]): B = {
    def loop(a: A): B = {
      // Unfold a layer of F using the seed
      val outerLayer = coalgebra(a)
      // Recursively unfold down to the deepest leaves and then fold back up again
      val refolded = F.map(outerLayer)(loop)
      // Tear the remaining outermost layer back down again
      algebra(refolded)
    }

    loop(a)
  }

  def hyloM[M[_], F[_], A, B](a: A)(coalgebra: A => M[F[A]], algebra: F[B] => M[B])(implicit M: Monad[M], T: Traverse[F]): M[B] = {
    def loop(a: A): M[B] = {
      // Unfold a layer of F using the seed
      val outerLayer = coalgebra(a)
      M.flatMap(outerLayer) { fa =>
        // Recursively unfold down to the deepest leaves and then fold back up again
        val refolded = T.traverse(fa)(loop)
        // Tear the remaining outermost layer back down again
        M.flatMap(refolded)(algebra)
      }
    }

    loop(a)
  }

  def prepro[F[_], A](fix: Fix[F])(pre: F ~> F, algebra: F[A] => A)(implicit F: Functor[F]): A = {
    def loop(fix: Fix[F]): A = {
      // Peel off a layer of Fix to get to the F inside
      val peeled = Fix.unfix(fix)
      // Apply the algebra to all the layers beneath this one
      val prepared = F.map(peeled) { fixf =>
        // Apply the transformation at each layer before handing over to the recursive call
        val transformed = cata[F, Fix[F]](fixf) { fa => Fix[F](pre(fa)) }
        loop(transformed)
      }
      // Finally apply the algebra to this layer
      algebra(prepared)
    }

    loop(fix)
  }

  def postpro[F[_], A](a: A)(coalgebra: A => F[A], post: F ~> F)(implicit F: Functor[F]): Fix[F] = {
    def loop(a: A): Fix[F] = {
      // Unfold the current seed into a layer of F containing the next seed using the coalgebra
      val outerLayer = coalgebra(a)
      // Unfold each inner seed A into a subsequent new layer of F recursively
      val allLayers = F.map(outerLayer) { aa =>
        ana[F, Fix[F]](loop(aa)) { fixf =>
          // Apply the transformation to each newly unfolded layer
          post(Fix.unfix(fixf))
        }
      }
      // Finish off this layer with a topping of Fix
      Fix[F](allLayers)
    }

    loop(a)
  }
}
