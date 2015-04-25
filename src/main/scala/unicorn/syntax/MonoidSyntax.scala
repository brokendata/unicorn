package unicorn.syntax
import unicorn.Monoid

object MonoidSyntax {
  implicit class MEnhance[A: Monoid](m: A){
    def |+|(that: A) = implicitly[Monoid[A]].append(m,that)
  }

  def mzero[A: Monoid] = implicitly[Monoid[A]].zero
}
