package schemes

import org.scalatest._

import cats._
import schemes.data._

sealed abstract class MathExpr[A] extends Product with Serializable
case class Num[A](num: Int) extends MathExpr[A] {
  def retag[B]: Num[B] = this.asInstanceOf[Num[B]]
}
case class Mul[A](l: A, r: A) extends MathExpr[A]
case class Add[A](l: A, r: A) extends MathExpr[A]

object MathExpr {
  type Expr = Fix[MathExpr]
  def num(i: Int): Expr = Fix[MathExpr](Num(i))
  def add(l: Expr, r: Expr): Expr = Fix[MathExpr](Add(l, r))
  def mul(l: Expr, r: Expr): Expr = Fix[MathExpr](Mul(l, r))
  val evalAlgebra: MathExpr[Int] => Int = {
    case Num(i) => i
    case Mul(l, r) => l * r
    case Add(l, r) => l + r
  }

  implicit val mathExprTraverse: Traverse[MathExpr] = new Traverse[MathExpr] {
    override def map[A, B](expr: MathExpr[A])(f: A => B) = expr match {
      case num @ Num(_) => num.retag[B]
      case Mul(l, r) => Mul(f(l), f(r))
      case Add(l, r) => Add(f(l), f(r))
    }

    def foldLeft[A, B](expr: MathExpr[A], b: B)(f: (B, A) => B): B = expr match {
      case Num(_) => b
      case Mul(l, r) => f(f(b, l), r)
      case Add(l, r) => f(f(b, l), r)
    }

    def foldRight[A, B](expr: MathExpr[A], eb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = expr match {
      case Num(_) => eb
      case Mul(l, r) => f(l, f(r, eb))
      case Add(l, r) => f(l, f(r, eb))
    }

    def traverse[F[_], A, B](expr: MathExpr[A])(f: A => F[B])(implicit F: Applicative[F]): F[MathExpr[B]] = expr match {
      case num @ Num(_) => F.pure(num.retag[B])
      case Mul(l, r) => F.map2(f(l), f(r))(Mul(_, _))
      case Add(l, r) => F.map2(f(l), f(r))(Add(_, _))
    }
  }
}

class SchemesSpec extends FlatSpec with Matchers {
  "unfix" should "unwrap a single layer of Fix" in {
    import MathExpr._
    add(num(1), num(1)).unfix shouldBe Add[Fix[MathExpr]](num(1), num(1))
  }

  "cata" should "evaluate MathExprs" in {
    import MathExpr._

    val two = add(num(1), num(1))
    two.cata(evalAlgebra) shouldBe 2

    val four = mul(num(2), num(2))
    four.cata(evalAlgebra) shouldBe 4

    val sixteen = add(num(2), add(num(3), num(11)))
    sixteen.cata(evalAlgebra) shouldBe 16

    two.cataM[Id, Int](evalAlgebra) shouldBe 2
  }

  "ana" should "unfold MathExprs" in {
    import MathExpr._

    val unfoldAdd = 5.ana[MathExpr] { i =>
      if (i < 2)
        Num(i)
      else
        Add(1, i - 1)
    }

    val unfoldAddM = 5.anaM[Id, MathExpr] { i =>
      if (i < 2)
        Num(i)
      else
        Add(1, i - 1)
    }

    unfoldAdd shouldBe add(num(1), add(num(1), add(num(1), add(num(1), num(1)))))

    unfoldAddM shouldBe add(num(1), add(num(1), add(num(1), add(num(1), num(1)))))
  }

  "hylo" should "unfold and then evaluate MathExprs" in {
    import MathExpr._

    5.hylo[MathExpr, Int](
      i => if (i < 2) Num(i) else Add(1, i - 1),
      evalAlgebra) shouldBe 5

    5.hyloM[Id, MathExpr, Int](
      i => if (i < 2) Num(i) else Add(1, i - 1),
      evalAlgebra) shouldBe 5
  }

  "prepro" should "apply a transformation at each layer before folding some structure" in {
    val sum: ListF[Int, Int] => Int = {
      case ConsF(h, t) => h + t
      case NilF() => 0
    }

    val stopAtFive = Lambda[ListF[Int, ?] ~> ListF[Int, ?]] {
      case ConsF(n, _) if n > 5 => NilF()
      case other => other
    }

    val `1 to 10` = ListF(1 to 10: _*)

    `1 to 10`.prepro[Int](
      stopAtFive,
      sum) shouldBe 15
  }

  "postpro" should "apply a transformation at each layer after unfolding some structure" in {
    val stopAtFive = Lambda[ListF[Int, ?] ~> ListF[Int, ?]] {
      case ConsF(n, _) if n > 5 => NilF()
      case other => other
    }

    val `1 to 5` = ListF(1 to 5: _*)

    1.postpro[ListF[Int, ?]](
      i => if (i > 100) NilF() else ConsF(i, i + 1),
      stopAtFive) shouldBe `1 to 5`
  }
}
