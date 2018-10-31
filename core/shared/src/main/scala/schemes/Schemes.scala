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

package schemes

import cats.{ Eval, Functor, Monad, Traverse, ~> }
import cats.free.Cofree

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
          // Apply the transformation to each newly unfolded layer after receiving the results of the recursive call
          post(Fix.unfix(fixf))
        }
      }
      // Finish off this layer with a topping of Fix
      Fix[F](allLayers)
    }

    loop(a)
  }

  def elgot[F[_], A, B](a: A)(elgotCoalgebra: A => Either[B, F[A]], algebra: F[B] => B)(implicit F: Functor[F]): B = {
    def loop(a: A): B = {
      // Unfold a layer of F using the seed
      elgotCoalgebra(a) match {
        // Each unfolding can either continue the computation or complete it with B
        case Right(fa) =>
          // If Right, continue the refold to at least the next layer
          val allLayers = F.map(fa)(loop)
          // Finally apply the algebra to tear down this layer
          algebra(allLayers)
        case Left(b) =>
          // If Left, short-circuit by returning a B to complete the refold early
          b
      }
    }

    loop(a)
  }

  def coelgot[F[_], A, B](a: A)(coalgebra: A => F[A], elgotAlgebra: (A, () => F[B]) => B)(implicit F: Functor[F]): B = {
    def loop(a: A): B = {
      // Construct a function which continues the computation
      val continue = () => {
        // Unfold the current seed into a layer of F containing the next seed using the coalgebra
        val currentLayer = coalgebra(a)
        // Recursively unfold down to the deepest leaves and then fold back up again
        F.map(currentLayer)(loop)
      }

      // Pass the current seed and the continuation to an algebra which decides whether to continue
      elgotAlgebra(a, continue)
    }

    loop(a)
  }

  def para[F[_], A](fix: Fix[F])(algebra: F[(Fix[F], A)] => A)(implicit F: Functor[F]): A = {
    def loop(fix: Fix[F]): A = {
      // Peel off a layer of Fix to get to the F inside
      val peeled = fix.unfix
      // Apply the algebra to all the layers beneath this one, passing along the current subtree
      val prepared = F.map(peeled)(f => (f, loop(f)))
      // Finally apply the algebra to this layer
      algebra(prepared)
    }

    loop(fix)
  }

  def apo[F[_], A](a: A)(coalgebra: A => F[Either[Fix[F], A]])(implicit F: Functor[F]): Fix[F] = {
    def continue(choice: Either[Fix[F], A]): Fix[F] = choice match {
      // If Right, continue unfolding the structure
      case Right(a) => loop(a)
      // If Left, complete with the current structure
      case Left(fix) => fix
    }

    def loop(a: A): Fix[F] = {
      // Unfold the current seed into a layer of F containing the next seed using the coalgebra
      val currentLayer = coalgebra(a)
      // Unfold each inner seed A into a subsequent new layer of F recursively
      val allLayers = F.map(currentLayer)(continue)
      // Finish off this layer with a topping of Fix
      Fix[F](allLayers)
    }

    loop(a)
  }

  def histo[F[_], A](fix: Fix[F])(algebra: F[Cofree[F, A]] => A)(implicit F: Functor[F]): A = {
    def continue(fix: Fix[F]): F[Cofree[F, A]] = {
      // Peel off a layer of Fix to get to the F inside
      val peeled = fix.unfix
      // Annotate the contents
      F.map(peeled)(annotate)
    }

    def annotate(fix: Fix[F]): Cofree[F, A] = {
      // Get the folded value for this layer
      val head = loop(fix)
      // Apply the algebra to all the layers beneath this one
      val tail = Eval.later(continue(fix))
      // Return the subtree annotated with the folded value
      Cofree(head, tail)
    }

    def loop(fix: Fix[F]): A = {
      // Start the fold from the current layer
      val annotated = continue(fix)
      // Apply the algebra to this layer
      algebra(annotated)
    }

    loop(fix)
  }
}
