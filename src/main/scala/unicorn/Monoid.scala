package unicorn
import simulacrum._

@typeclass trait Monoid[A] extends Semigroup[A] {
  def zero: A
}

object Monoid {
  implicit def IntMonoid = new Monoid[Int] {
    def append(x: Int, y: Int) = x + y
     def zero = 0
  }
}

