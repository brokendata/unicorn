import unicorn.Monad._
import unicorn.Monad
def join[M[_]: Monad,A](mma: M[M[A]]): M[A] =
