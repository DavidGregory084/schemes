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

import cats.{ Eval, Monad, Traverse, ~> }
import cats.free.Cofree
import cats.syntax.functor._

object Schemes {
  def cata[F[_], A](fix: Fix[F])(algebra: F[A] => A)(implicit T: Traverse[F]): Eval[A] = {
    def loop(fix: Fix[F]): Eval[A] = Eval.defer {
      // Peel off a layer of Fix to get to the F inside
      val peeled = Fix.unfix(fix)
      // Apply the algebra to all the layers beneath this one
      val prepared = T.traverse(peeled)(loop)
      // Finally apply the algebra to this layer
      prepared.map(algebra)
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

  def ana[F[_], A](a: A)(coalgebra: A => F[A])(implicit T: Traverse[F]): Eval[Fix[F]] = {
    def loop(a: A): Eval[Fix[F]] = Eval.defer {
      // Unfold the current seed into a layer of F containing the next seed using the coalgebra
      val currentLayer = coalgebra(a)
      // Unfold each inner seed A into a subsequent new layer of F recursively
      val allLayers = T.traverse(currentLayer)(loop)
      // Finish off this layer with a topping of Fix
      allLayers.map(Fix(_))
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

  def hylo[F[_], A, B](a: A)(coalgebra: A => F[A], algebra: F[B] => B)(implicit T: Traverse[F]): Eval[B] = {
    def loop(a: A): Eval[B] = Eval.defer {
      // Unfold a layer of F using the seed
      val outerLayer = coalgebra(a)
      // Recursively unfold down to the deepest leaves and then fold back up again
      val refolded = T.traverse(outerLayer)(loop)
      // Tear the remaining outermost layer back down again
      refolded.map(algebra)
    }

    loop(a)
  }

  def hyloM[M[_], F[_], A, B](a: A)(coalgebra: A => M[F[A]], algebra: F[B] => M[B])(implicit M: Monad[M], T: Traverse[F]): M[B] = {
    def loop(a: A): M[B] = {
      // Unfold a layer of F using the seed
      M.flatMap(coalgebra(a)) { fa =>
        // Recursively unfold down to the deepest leaves and then fold back up again
        val refolded = T.traverse(fa)(loop)
        // Tear the remaining outermost layer back down again
        M.flatMap(refolded)(algebra)
      }
    }

    loop(a)
  }

  def prepro[F[_], A](fix: Fix[F])(pre: F ~> F)(algebra: F[A] => A)(implicit T: Traverse[F]): Eval[A] = {
    def loop(fix: Fix[F]): Eval[A] = Eval.defer {
      // Peel off a layer of Fix to get to the F inside
      val peeled = pre(Fix.unfix(fix))
      // Apply the algebra to all the layers beneath this one
      val prepared = T.traverse(peeled)(loop)
      // Finally apply the algebra to this layer
      prepared.map(algebra)
    }

    loop(fix)
  }

  def postpro[F[_], A](a: A)(post: F ~> F)(coalgebra: A => F[A])(implicit T: Traverse[F]): Eval[Fix[F]] = {
    def loop(a: A): Eval[Fix[F]] = Eval.defer {
      // Unfold the current seed into a layer of F containing the next seed using the coalgebra
      val outerLayer = post(coalgebra(a))
      // Unfold each inner seed A into a subsequent new layer of F recursively
      val allLayers = T.traverse(outerLayer)(loop)
      // Finish off this layer with a topping of Fix
      allLayers.map(Fix(_))
    }

    loop(a)
  }

  def elgot[F[_], A, B](a: A)(elgotCoalgebra: A => Either[B, F[A]], algebra: F[B] => B)(implicit T: Traverse[F]): Eval[B] = {
    def loop(a: A): Eval[B] = Eval.defer {
      // Unfold a layer of F using the seed
      elgotCoalgebra(a) match {
        // Each unfolding can either continue the computation or complete it with B
        case Right(fa) =>
          // If Right, continue the refold to at least the next layer
          val allLayers = T.traverse(fa)(loop)
          // Finally apply the algebra to tear down this layer
          allLayers.map(algebra)
        case Left(b) =>
          // If Left, short-circuit by returning a B to complete the refold early
          Eval.now(b)
      }
    }

    loop(a)
  }

  def coelgot[F[_], A, B](a: A)(coalgebra: A => F[A], elgotAlgebra: (A, () => Eval[F[B]]) => Eval[B])(implicit T: Traverse[F]): Eval[B] = {
    def loop(a: A): Eval[B] = Eval.defer {
      // Construct a function which continues the computation
      val continue = () => {
        // Unfold the current seed into a layer of F containing the next seed using the coalgebra
        val currentLayer = coalgebra(a)
        // Recursively unfold down to the deepest leaves and then fold back up again
        T.traverse(currentLayer)(loop)
      }

      // Pass the current seed and the continuation to an algebra which decides whether to continue
      elgotAlgebra(a, continue)
    }

    loop(a)
  }

  def para[F[_], A](fix: Fix[F])(algebra: F[(Fix[F], A)] => A)(implicit T: Traverse[F]): Eval[A] = {
    def loop(fix: Fix[F]): Eval[A] = Eval.defer {
      // Peel off a layer of Fix to get to the F inside
      val peeled = Fix.unfix(fix)
      // Apply the algebra to all the layers beneath this one, passing along the current subtree
      val prepared = T.traverse(peeled)(f => loop(f).tupleLeft(f))
      // Finally apply the algebra to this layer
      prepared.map(algebra)
    }

    loop(fix)
  }

  def apo[F[_], A](a: A)(coalgebra: A => F[Either[Fix[F], A]])(implicit T: Traverse[F]): Eval[Fix[F]] = {
    def continue(choice: Either[Fix[F], A]): Eval[Fix[F]] = choice match {
      // If Right, continue unfolding the structure
      case Right(a) => loop(a)
      // If Left, complete with the current structure
      case Left(fix) => Eval.now(fix)
    }

    def loop(a: A): Eval[Fix[F]] = Eval.defer {
      // Unfold the current seed into a layer of F containing the next seed using the coalgebra
      val currentLayer = coalgebra(a)
      // Unfold each inner seed A into a subsequent new layer of F recursively
      val allLayers = T.traverse(currentLayer)(continue)
      // Finish off this layer with a topping of Fix
      allLayers.map(Fix(_))
    }

    loop(a)
  }

  def histo[F[_], A](fix: Fix[F])(algebra: F[Cofree[F, A]] => A)(implicit T: Traverse[F]): Eval[A] = {
    def continue(fix: Fix[F]): Eval[F[Cofree[F, A]]] = Eval.defer {
      // Peel off a layer of Fix to get to the F inside
      val peeled = fix.unfix
      // Annotate the contents
      T.traverse(peeled)(annotate)
    }

    def annotate(fix: Fix[F]): Eval[Cofree[F, A]] = Eval.defer {
      // Get the folded value for this layer
      val head = loop(fix)
      // Apply the algebra to all the layers beneath this one
      val tail = continue(fix)
      // Return the subtree annotated with the folded value
      head.map(Cofree(_, tail))
    }

    def loop(fix: Fix[F]): Eval[A] = Eval.defer {
      // Start the fold from the current layer
      val annotated = continue(fix)
      // Apply the algebra to this layer
      annotated.map(algebra)
    }

    loop(fix)
  }
}
