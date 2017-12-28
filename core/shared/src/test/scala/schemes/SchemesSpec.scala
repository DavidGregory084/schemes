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

  implicit val mathExprFunctor: Functor[MathExpr] = new Functor[MathExpr] {
    def map[A, B](expr: MathExpr[A])(f: A => B) = expr match {
      case num @ Num(_) => num.retag[B]
      case Mul(l, r) => Mul(f(l), f(r))
      case Add(l, r) => Add(f(l), f(r))
    }
  }
}

class SchemesSpec extends FlatSpec with Matchers {
  "cata" should "evaluate MathExprs" in {
    import MathExpr._

    val two = add(num(1), num(1))
    Schemes.cata(two)(evalAlgebra) shouldBe 2

    val four = mul(num(2), num(2))
    Schemes.cata(four)(evalAlgebra) shouldBe 4

    val sixteen = add(num(2), add(num(3), num(11)))
    Schemes.cata(sixteen)(evalAlgebra) shouldBe 16
  }

  "ana" should "unfold MathExprs" in {
    import MathExpr._

    val unfoldAdd = Schemes.ana[MathExpr, Int](5) { i =>
      if (i < 2)
        Num(i)
      else
        Add(1, i - 1)
    }

    unfoldAdd shouldBe add(num(1), add(num(1), add(num(1), add(num(1), num(1)))))
  }

  "hylo" should "unfold and then evaluate MathExprs" in {
    import MathExpr._

    Schemes.hylo[MathExpr, Int, Int](5)(
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

    Schemes.prepro[ListF[Int, ?], Int](`1 to 10`)(
      stopAtFive,
      sum) shouldBe 15
  }

  "postpro" should "apply a transformation at each layer after unfolding some structure" in {
    val stopAtFive = Lambda[ListF[Int, ?] ~> ListF[Int, ?]] {
      case ConsF(n, _) if n > 5 => NilF()
      case other => other
    }

    val `1 to 5` = ListF(1 to 5: _*)

    Schemes.postpro[ListF[Int, ?], Int](1)(
      i => if (i > 100) NilF() else ConsF(i, i + 1),
      stopAtFive) shouldBe `1 to 5`
  }
}
