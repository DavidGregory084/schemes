package schemes

import cats.{ ~>, Monad, Traverse }
import cats.free.Cofree

/* Check that usages of extension methods compile */
object SyntaxSpec {
  def mock[A]: A = ???

  def testAnySyntax[F[_]: Traverse, M[_]: Monad, A, B]: Unit = {
    val a = mock[A]
    val coalgebra = mock[A => F[A]]
    val coalgebraM = mock[A => M[F[A]]]
    val algebra = mock[F[B] => B]
    val algebraM = mock[F[B] => M[B]]
    val natTrans = mock[F ~> F]
    val elgotCoalgebra = mock[A => Either[B, F[A]]]
    val elgotAlgebra = mock[(A, () => F[B]) => B]
    val rCoalgebra = mock[A => F[Either[Fix[F], A]]]
    a.ana(coalgebra)
    a.anaM(coalgebraM)
    a.hylo(coalgebra, algebra)
    a.hyloM(coalgebraM, algebraM)
    a.postpro(coalgebra, natTrans)
    a.elgot(elgotCoalgebra, algebra)
    a.coelgot(coalgebra, elgotAlgebra)
    a.apo(rCoalgebra)
    ()
  }

  def testFixSyntax[F[_]: Traverse, M[_]: Monad, A, B]: Unit = {
    val fix = mock[Fix[F]]
    val algebra = mock[F[B] => B]
    val algebraM = mock[F[B] => M[B]]
    val natTrans = mock[F ~> F]
    val cvAlgebra = mock[F[Cofree[F, A]] => A]
    fix.unfix
    fix.cata(algebra)
    fix.cataM(algebraM)
    fix.prepro(natTrans, algebra)
    fix.histo(cvAlgebra)
    ()
  }
}
