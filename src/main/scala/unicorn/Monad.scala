package unicorn

import simulacrum._

@typeclass trait Monad[M[_]] extends Functor[M] {
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def unit[A](a: A): M[A]

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => unit(f(x)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  // *flattens*
  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(x => x)

  def flatMapbyJoin[A,B](ma: M[A])(f: A => M[B]): M[B] =
}

object Monad {
  implicit def ListMonad: Monad[List] = new Monad[List] {
    def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] = la flatMap f

    def unit[A](a: A) = List(a)
  }
}

