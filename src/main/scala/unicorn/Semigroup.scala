package unicorn

import simulacrum._

@typeclass trait Semigroup[A] {
  @op("|+|") def append(x: A, y: A): A
}

object Semigroup{
  implicit def IntSemiGroup = new Semigroup[Int] {
    def append(x: Int, y: Int) = x + y
  }

  implicit def Listsemigroup[A] = new Semigroup[List[A]]{
    def append(x: List[A], y: List[A]) = x ++ y
  }
}

