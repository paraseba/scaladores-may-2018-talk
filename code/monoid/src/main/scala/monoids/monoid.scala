package monoids

import scala.collection.immutable._


trait Monoid[M] {

  def zero: M

  def append(a: M, b: => M): M

}

object Monoid {
  def apply[A:Monoid]: Monoid[A] = implicitly[Monoid[A]]
}

object MonoidSyntax {
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitConversion"))
  implicit def ToMonoidOps[A:Monoid](v: A): MonoidOps[A] =
    new MonoidOps[A](v)

  final class MonoidOps[A:Monoid](val self: A) {
    def |+|(other: => A): A =
      Monoid[A].append(self, other)
  }
}

object SimpleMonoids {
  import MonoidSyntax._

  implicit def intAddMon: Monoid[Int] = new Monoid[Int] {
    def zero = 0
    def append(a: Int, b: => Int): Int = a + b
  }


  // is this really a monoid?
  implicit def floatAddMon: Monoid[Float] = new Monoid[Float] {
    def zero = 0
    def append(a: Float, b: => Float): Float = a + b
  }

  implicit def intMulMon: Monoid[Int] = new Monoid[Int] {
    def zero = 1
    def append(a: Int, b: => Int): Int = a * b
  }

  implicit def freeMon[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def zero: List[A] = List()
    def append(as: List[A], bs: => List[A]): List[A] = as ++ bs
  }

  implicit def firstMon[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def zero = None
    def append(a: Option[A], b: => Option[A]): Option[A] = a orElse b
  }

  def optionMon[A:Monoid]: Monoid[Option[A]] =
    new Monoid[Option[A]] {

      def zero: Option[A] = None

      def append(a: Option[A], b: => Option[A]): Option[A] =
        (a,b) match {
          case (a, None) => a
          case (None, b) => b
          case (Some(a), Some(b)) => Some(a |+| b)
        }
  }

  implicit def endoMon[A]: Monoid[A => A] = new Monoid[A => A] {
    def zero: A => A = identity
    def append(f: A => A, g: => (A => A)): A => A = g andThen f
  }

  implicit def monFunMon[A, B:Monoid]: Monoid[A => B] =
    new Monoid[A => B] {

      def zero: A => B = _ => Monoid[B].zero

      def append(f: A => B, g: => (A => B)): A => B =
        a => f(a) |+| g(a)
  }

  //(implicit am: Monoid[A], implicit bm: Monoid[B])
  implicit def pairMon[A: Monoid, B: Monoid]: Monoid[(A,B)] = new Monoid[(A, B)] {

    def zero: (A,B) = (Monoid[A].zero, Monoid[B].zero)

    def append(a: (A, B), b: => (A, B)): (A,B) =
      (a._1 |+| b._1, a._2 |+| b._2)
  }


  def mconcat[A:Monoid](as: Traversable[A]): A =
    as.foldLeft(Monoid[A].zero)(_ |+| _)

  def sum(xs: Traversable[Int]): Int =
    mconcat(xs)(intAddMon)

  def first[A](xs: Traversable[Option[A]]): Option[A] =
    mconcat(xs)(firstMon)

  def concat[A](xs: Traversable[List[A]]): List[A] =
    mconcat(xs)(freeMon)

  def sumO[A](xs: Traversable[Option[Int]]): Option[Int] =
    mconcat(xs)(optionMon(intAddMon))

  def foldMap[A, M:Monoid](f: A => M)(as: Traversable[A]): M =
    as.foldRight(Monoid[M].zero) { (x,res) =>
      f(x) |+| res
    }

  def filter[A](f: A => Boolean)(as: List[A]): List[A] =
    foldMap((a:A) => if (f(a)) List(a) else List())(as)(freeMon)

  val allMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def zero: Boolean = true
    def append(a: Boolean, b: => Boolean): Boolean = a && b
  }


  implicit val mon: Monoid[Boolean] = allMonoid

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  val numbers: Iterable[Int] = 1.to(100).filter(
    ((n:Int) => n % 2 == 0) |+| ((n:Int) => n >= 10) |+| ((n:Int) => n < 20)
  )


  sealed abstract class Min[A]
  final case class EmptyMin[A]() extends Min[A]
  final case class MinValue[A](value: A) extends Min[A]

  object Min {
    def apply[A](a: A): Min[A] = MinValue(a)


    implicit def minMonoid[A:Ordering]: Monoid[Min[A]] = new Monoid[Min[A]] {
      def zero = EmptyMin()

      def append(a: Min[A], b: => Min[A]): Min[A] = (a, b) match {
        case (EmptyMin(), x) => x
        case (x, EmptyMin()) => x
        case (MinValue(x), MinValue(y)) => Min(List(x,y).min)
      }
    }
  }

  sealed abstract class Max[A]
  final case class EmptyMax[A]() extends Max[A]
  final case class MaxValue[A](value: A) extends Max[A]

  object Max {
    def apply[A](a: A): Max[A] = MaxValue(a)

    implicit def maxMonoid[A:Ordering]: Monoid[Max[A]] = new Monoid[Max[A]] {
      def zero = EmptyMax()

      def append(m1: Max[A], m2: => Max[A]): Max[A] = (m1, m2) match {
        case (EmptyMax(), x) => x
        case (x, EmptyMax())=> x
        case (MaxValue(x), MaxValue(y)) => Max(List(x,y).max)
      }
    }
  }

  def min[A: Ordering](as: Traversable[A]): Option[A] = {
    def mapper(a: A) = Min(a)

    foldMap(mapper)(as) match {  // map and reduce
      case MinValue(x) => Some(x)
      case _ => None
    }
  }

  def minmax[A: Ordering](as: Traversable[A]): Option[(A,A)] = {
    def mapper(a: A) = (Min(a), Max(a))

    foldMap(mapper)(as) match {
      case (MinValue(x), MaxValue(y)) => Some((x,y))
      case _ => None
    }
  }

}

object Parallel {

  import MonoidSyntax._

  def mconcat[A:Monoid](as: Traversable[A]): A =
    as.par.fold(Monoid[A].zero)(_ |+| _)
}

object Stats {

  sealed abstract class MeanVar
  final case object EmptyMeanVar extends MeanVar
  final case class MeanVarV(m1: Double, m2: Double, n: Long) extends MeanVar

  object MeanVar {
    import SimpleMonoids.foldMap

    def singleton(x: Double): MeanVar = MeanVarV(x, 0, 1)

    def sample(xs: Traversable[Double]): MeanVar =
      foldMap(singleton(_))(xs)

    def mean: MeanVar => Option[Double] = {
      case MeanVarV(m1, _, _) => Some(m1)
      case _ => None
    }

    def variance: MeanVar => Option[Double] = {
      case MeanVarV(_, _, 1) => Some(0)
      case MeanVarV(_, m2, n) => Some(m2/(n - 1.0))
      case _ => None
    }

    def sampleSize: MeanVar => Long = {
      case MeanVarV(_, _, n) => n
      case _ => 0
    }

    implicit val mvMonoid: Monoid[MeanVar] = new Monoid[MeanVar] {
      def zero: MeanVar = EmptyMeanVar

      def append(a: MeanVar, b: => MeanVar): MeanVar = (a, b) match {
        case (MeanVarV(m1a, m2a, na), MeanVarV(m1b, m2b, nb)) => {
          val nt = na + nb
          val delta = m1b - m1a
          MeanVarV((na * m1a + nb * m1b ) / nt,
                   m2a + m2b + delta * delta * na * nb / nt,
                   nt)
        }

        case (EmptyMeanVar, a) => a
        case (a, EmptyMeanVar) => a
      }
    }
  }



}
