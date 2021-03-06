/*

Copyright 2018 Sebastian B. Galkin

This file is part of paraseba/scaladores-may-2018-talk.

paraseba/scaladores-may-2018-talk is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

paraseba/scaladores-may-2018-talk is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

*/

package monoid.monoids

import scala.collection.immutable._
import scala.collection.parallel.TaskSupport

trait Semigroup[M] {

  def append(a: M, b: => M): M

}

trait Monoid[M] extends Semigroup[M] {

  def zero: M

}

object Monoid {
  def apply[A:Monoid]: Monoid[A] = implicitly[Monoid[A]]
}

object Semigroup {
  def apply[A:Semigroup]: Semigroup[A] = implicitly[Semigroup[A]]
}

object SemigroupSyntax {
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitConversion"))
  implicit def ToSemigroupOps[A:Semigroup](v: A): SemigroupOps[A] =
    new SemigroupOps[A](v)

  final class SemigroupOps[A:Semigroup](val self: A) {
    def |+|(other: => A): A =
      Semigroup[A].append(self, other)
  }
}

object SimpleMonoids {
  import SemigroupSyntax._

  implicit def intAddMon: Monoid[Int] = new Monoid[Int] {
    def zero = 0
    def append(a: Int, b: => Int): Int = a + b
  }


  // is this really a monoid?
  implicit def floatAddMon: Monoid[Double] = new Monoid[Double] {
    def zero: Double = 0
    def append(a: Double, b: => Double): Double = a + b
  }

  implicit def intMulMon: Monoid[Int] = new Monoid[Int] {
    def zero = 1
    def append(a: Int, b: => Int): Int = a * b
  }

  implicit def freeMon[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def zero: List[A] = List()
    def append(as: List[A], bs: => List[A]): List[A] = as ++ bs
  }

  def firstMon[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def zero = None
    def append(a: Option[A], b: => Option[A]): Option[A] = a orElse b
  }

  implicit def optionMon[A:Semigroup]: Monoid[Option[A]] =
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

  implicit def monFunSemi[A, B:Semigroup]: Semigroup[A => B] =
    new Semigroup[A => B] {

      def append(f: A => B, g: => (A => B)): A => B =
        a => f(a) |+| g(a)
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

    def append(x: (A, B), y: => (A, B)): (A,B) =
      (x._1 |+| y._1, x._2 |+| y._2)
  }

  implicit def tripleMon[A: Monoid, B: Monoid, C: Monoid]: Monoid[(A,B,C)] = new Monoid[(A, B, C)] {

    def zero: (A,B,C) = (Monoid[A].zero, Monoid[B].zero, Monoid[C].zero)

    def append(x: (A, B, C), y: => (A, B, C)): (A,B, C) =
      (x._1 |+| y._1, x._2 |+| y._2, x._3 |+| y._3)
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

  def foldMap[A, M:Monoid](as: Traversable[A], f: A => M): M =
    as.foldLeft(Monoid[M].zero) { _ |+| f(_) }

  def foldMap2[A, M:Monoid](as: Traversable[A], f: A => M): M =
    mconcat(as.map(f))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldMap(as, (a:A) => if (f(a)) List(a) else List())(freeMon)

  val allMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def zero: Boolean = true
    def append(a: Boolean, b: => Boolean): Boolean = a && b
  }


  implicit val mon: Monoid[Boolean] = allMonoid

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  val numbers: Iterable[Int] = 1.to(100).filter(
    // we only add zero to exercise the method, not really needed
    ((n:Int) => n % 2 == 0) |+| ((n:Int) => n >= 10) |+| ((n:Int) => n < 20) |+| Monoid[ Int=> Boolean].zero
  )

  def minMon[A:Ordering]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def zero: Option[A] = None

    def append(a: Option[A], b: => Option[A]): Option[A] = (a, b) match {
      case (None, x) => x
      case (x, None) => x
      case (Some(x), Some(y)) => Some(Ordering[A].min(x,y))
    }
  }

  def maxMon[A:Ordering]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def zero: Option[A] = None

    def append(a: Option[A], b: => Option[A]): Option[A] = (a, b) match {
      case (None, x) => x
      case (x, None) => x
      case (Some(x), Some(y)) => Some(Ordering[A].max(x,y))
    }
  }

  def maxSemi[A:Ordering]: Semigroup[A] = new Semigroup[A] {
    def append(a: A, b: => A): A = Ordering[A].max(a,b)
  }

  def minSemi[A:Ordering]: Semigroup[A] = new Semigroup[A] {
    def append(a: A, b: => A): A = Ordering[A].min(a,b)
  }

  def min[A: Ordering](as: Traversable[A]): Option[A] =
    foldMap(as, (a:A) => Option(a))(minMon) // map and reduce

  def max[A: Ordering](as: Traversable[A]): Option[A] =
    foldMap(as, (a:A) => Option(a))(maxMon) // map and reduce

  def minMaxMon[A:Ordering]: Monoid[(Option[A], Option[A])] = pairMon(minMon, maxMon)

  def minmax[A: Ordering](as: Traversable[A]): Option[(A,A)] =
    foldMap(as, (a:A) => (Option(a), Option(a)))(minMaxMon) match {
      case (Some(mi), Some(ma)) => Some((mi,ma))
      case _ => None
    }
}


object Parallel {

  import SemigroupSyntax._

  // We receive a Task Support to make testing easier, thread pools are shared between tests
  def mconcat[A:Monoid](as: Traversable[A])(ts: TaskSupport): A = {
    val parAs = as.par
    parAs.tasksupport = ts
    parAs.fold(Monoid[A].zero)(_ |+| _)
  }
}

object Stats {

  sealed abstract class MeanVar
  final case object EmptyMeanVar extends MeanVar
  final case class MeanVarV(m1: Double, m2: Double, n: Long) extends MeanVar

  object MeanVar {
    import SimpleMonoids.foldMap

    def singleton(x: Double): MeanVar = MeanVarV(x, 0, 1)

    def sample(xs: Traversable[Double]): MeanVar =
      foldMap(xs, singleton)

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

    implicit val meanVarMonoid: Monoid[MeanVar] = new Monoid[MeanVar] {
      def zero: MeanVar = EmptyMeanVar

      def append(a: MeanVar, b: => MeanVar): MeanVar = (a, b) match {
        case (MeanVarV(m1a, m2a, na), MeanVarV(m1b, m2b, nb)) => {
          val (nt, delta) = (na + nb, m1b - m1a)
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
