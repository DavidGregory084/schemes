package schemes
package data

import cats.laws.discipline._
import cats.tests.CatsSuite
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Arbitrary.arbitrary

class ListFSpec extends CatsSuite {
  implicit def arbListF[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[ListF[A, B]] =
    Arbitrary(Gen.oneOf(
      Gen.const(NilF[A, B]()),
      for {
        a <- arbitrary[A]
        b <- arbitrary[B]
      } yield ConsF(a, b)))

  checkAll("ListF[Int, Int]", TraverseTests[ListF[Int, ?]].traverse[Int, Int, Int, Set[Int], Option, Option])

  val list = List(1, 2, 3, 4, 5)
  val listF = ListF(1 to 5: _*).value

  test("ListF.apply") {
    listF shouldBe ListF.cons(1, ListF.cons(2, ListF.cons(3, ListF.cons(4, ListF.cons(5, ListF.nil[Int])))))
  }

  test("ListF.toList") {
    ListF.toList(listF).value shouldBe list
  }

  test("ListF.apply is stack-safe") {
    ListF(1 to 1000000: _*).value
  }

  test("ListF.toList is stack-safe") {
    ListF.toList(ListF(1 to 1000000: _*).value).value
  }
}
