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

package monoid.diagrams

import scala.collection.immutable._
import monoid.monoids._


object Diagrams {
  import SemigroupSyntax._

  sealed trait Prim
  final case class Point() extends Prim
  final case class Line() extends Prim
  final case class Ellipse() extends Prim

  final case class Dual[M](undual: M)

  implicit def dualMonoid[M:Monoid]: Monoid[Dual[M]] = new Monoid[Dual[M]] {
    def zero = Dual(implicitly[Monoid[M]].zero)
    def append(a: Dual[M], b : => Dual[M]) = Dual(b.undual |+| a.undual)
  }

  def wSemi[Out, In:Semigroup](unwrap: Out => In, wrap: In => Out): Semigroup[Out] = new Semigroup[Out] {
    def append(x:Out, y: => Out) = wrap(unwrap(x) |+| unwrap(y))
  }

  def wMon[Out, In:Monoid](unwrap: Out => In, wrap: In => Out): Monoid[Out] = new Monoid[Out] {
    def append(x: Out, y: => Out) = wrap(unwrap(x) |+| unwrap(y))
    def zero = wrap(implicitly[Monoid[In]].zero)
  }

  type P2 = (Double, Double)
  type V2 = (Double, Double)
  def v2mul(s: Double, v: V2): V2 = v match {
    case (x,y) => (s*x, s*y)
  }


  import SimpleMonoids._

  final case class Diagram1(primitives: Dual[List[Prim]])

  final case class Max[A](unmax: A)
  final case class Min[A](unmin: A)

  implicit def maxSemi[A:Ordering]: Semigroup[Max[A]] = new Semigroup[Max[A]] {
    def append(a: Max[A], b: => Max[A]): Max[A] = Max(Ordering[A].max(a.unmax, b.unmax))
  }

  implicit def minSemi[A:Ordering]: Semigroup[Min[A]] = new Semigroup[Min[A]] {
    def append(a: Min[A], b: => Min[A]): Min[A] = Min(Ordering[A].min(a.unmin, b.unmin))
  }

  final case class EnvelopeBad(f: V2 => Max[Double])
  final case class Envelope(f: Option[V2 => Max[Double]])

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def envelopeP(p: Prim): Envelope = {throw new Exception(p.toString)}

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def translateP(v: V2, p: Prim): Prim = {throw new Exception(p.toString + v.toString)}

  def envelope(d: Diagram1): Envelope = mconcat(d.primitives.undual.map(envelopeP))
  def translate(v: V2, d: Diagram1): Diagram1 = Diagram1(Dual(d.primitives.undual.map(translateP(v, _))))

  def beside(v: V2, d1: Diagram1, d2: Diagram1): Diagram1 =
    (envelope(d1).f, envelope(d2).f) match {
      case (Some(f1), Some(f2)) => d1 |+| translate(v2mul(f1(v).unmax + f2(v).unmax, v), d2)
      case _ => d1 |+| d2
    }

  object Envelope {
    val badSemi: Semigroup[EnvelopeBad] = wSemi(_.f, EnvelopeBad(_))

    implicit val monoid: Monoid[Envelope] = wMon(_.f, Envelope(_))

  }


  final case class TraceBad(f: P2 => V2 => Min[Double])
  final case class Trace(f: P2 => V2 => Option[Min[Double]])

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def traceP(p: Prim): Trace = {throw new Exception(p.toString)}

  object Trace {
    val semiBad: Semigroup[TraceBad] = wSemi(_.f, TraceBad(_))
    implicit val monoid: Monoid[Trace] = wMon(_.f, Trace(_))

  }

  object Diagram1 {
    implicit val monoid: Monoid[Diagram1] = wMon(_.primitives, Diagram1(_))
  }

  final case class Diagram2 private (primitives: Dual[List[Prim]], env: Envelope, tr: Trace)
  def envelope2(d: Diagram2): Envelope = d.env
  def trace2(d: Diagram2): Trace = d.tr

  def primDiag(p: Prim): Diagram2 = Diagram2(Dual(List(p)), envelopeP(p), traceP(p))

  object Diagram2 {
    def mkDiag(ps: List[Prim]): Diagram2 = mconcat(ps.map(primDiag))

    def d2Tuple(d: Diagram2): (Dual[List[Prim]], (Envelope, Trace)) =  (d.primitives, (d.env, d.tr))
    def tuple2d2(t: (Dual[List[Prim]], (Envelope, Trace))): Diagram2  =  Diagram2(t._1,t._2._1,t._2._2)

    implicit val monoid: Monoid[Diagram2] = wMon(d2Tuple, tuple2d2)
  }

}

