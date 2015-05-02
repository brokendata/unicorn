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
}
