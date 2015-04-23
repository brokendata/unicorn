package unicorn

trait Monoid[A] extends Semigroup[A] {
  def append(a1: A, a2: A): A
  def zero: A
}


object Monoid{
  def apply[A: Monoid] = implicitly[Monoid[A]]

  implicit def IntMonoid = new Monoid[Int]{
    def append(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }

  implicit def StringMonoid = new Monoid[String]{
    def append(a1: String, a2: String) = a1 + a2
    def zero = ""
  }

  implicit def OptionMonoid[A: Semigroup] = new Monoid[Option[A]]{
    def append(a1: Option[A], a2: Option[A]) = (a1,a2) match {
      case (Some(l), Some(r)) => Some(Semigroup[A].append(l,r))
      case (Some(l), None) => Some(l)
      case (None,Some(r)) => Some(r)
      case (None, None) => None
    }

    def zero: Option[A] = None 
  }

}