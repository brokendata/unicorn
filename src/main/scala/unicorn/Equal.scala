package unicorn

trait Equal[A]{
  def equals(a: A, b: A): Boolean = a ==b
}

object Equal{
  def apply[A: Equal] = implicitly[Equal[A]]

  implicit def IntEqual = new Equal[Int]{
    def equal(a: Int, b: Int): Boolean = a == b
  }

  implicit def OptionEqual = new Equal[Option]{
    def equal(a: Option, b: Option): Boolean = a == b
  }

}
