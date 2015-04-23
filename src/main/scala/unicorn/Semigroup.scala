package unicorn

trait Semigroup[A]{
  def append(a1: A, a2: A): A
}

object Semigroup{
  def apply[A: Semigroup] = implicitly[Semigroup[A]]

  def groupInstance[A](f:(A,A) => A) = new Semigroup[A] {
    def append(a1: A, a2: A) = f(a1,a2)
  }

  def IntSemiGroup = groupInstance[Int](_+_)

  def StringSemiGroup = groupInstance[String](_+_)


}