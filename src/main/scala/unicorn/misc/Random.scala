package unicorn.misc

trait RNG {
  def nextInt: (Int, RNG)
}


object RNG {

  type Rand[+A] = (RNG) => (A,RNG)

  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }
  def unit[A](a: A): (RNG => (A,RNG)) = rng => (a,rng)

  def map[A,B](s: Rand[A])(f: A =>B): Rand[B] = {
    rng =>
      val (i1, s2) = s(rng)
      (f(i1),s2)
  }

  def map2[A,B,C](s1: Rand[A], s2: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng =>
      val (i1,sa) = s1(rng)
      val (i2,sc) = s2(sa)
      (f(i1,i2),sc)
  }

  def sequence[A](fa: List[Rand[A]]): Rand[List[A]] = {
    fa.foldRight(unit(List[A]()))((f,acc) => map2(f,acc)(_::_))
  }

  def flatMap[A,B](s1: Rand[A])(f: A => Rand[B]): Rand[B] = {
    rng =>
      val (i1,s2) = s1(rng)
      f(i1)(s2)
  }
}

object StateMonad{
  case class State[S,A](run: S => (A,S)){
    // to create an instance of Stat
    def unit[A](a: A): State[S,A] = State(S => (a,S))
    def map[B](f: A =>B): State[S,B] = {
      State(s => {
        val (a,s1)  = run(s)
        (f(a),s)}
        )
    }
    def flatMap[B](f: A => State[S,B]): State[S,B] = {
      State(s => {
        val (a,s1) = run(s)
        f(a).run(s1)
      })
    }

    def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] = {
      State(s=>{
        val (a,s2) = run(s)
        val (b,s3) = sb.run(s2)
        (f(a,b),s3)
      }
      )
    }
  }
}
