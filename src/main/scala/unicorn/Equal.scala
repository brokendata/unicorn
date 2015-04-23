package unicorn
import unicorn.ADT._
trait Equal[A]{
  def equals(a: A, b: A): Boolean
}

object Equal{
  def apply[A: Equal] = implicitly[Equal[A]]

  def eqInstance[A](f: (A,A) => Boolean): Equal[A] = new Equal[A] {
    def equals(a: A, b: A) = f(a,b)
  }

  implicit def IntEqual = eqInstance[Int](_ == _)

  implicit def OptionEqual[A: Equal] = new Equal[Option[A]]{
    def equals(a: Option[A], b: Option[A]) = implicitly[Equal[A]].equals(a.get,b.get)
  }


}
