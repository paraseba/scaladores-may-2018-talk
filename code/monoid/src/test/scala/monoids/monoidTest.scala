package monoids

import scala.annotation.tailrec
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import org.scalacheck.Prop.{forAll}
import org.scalactic.anyvals.PosInt

import scala.collection.immutable._

import Arbitrary.arbitrary
import SimpleMonoids._


class MonoidSpec extends FunSuite with Checkers with Matchers {
  import MonoidSyntax._

  def assoc[M:Monoid](f: (M,M) => Boolean)(a: M, b: M, c: M): Boolean =
    f((a |+| b) |+| c, a |+| (b |+| c))

  def leftId[M:Monoid](f: (M,M) => Boolean)(a: M): Boolean =
    f(Monoid[M].zero |+| a, a)

  def rightId[M:Monoid](f: (M,M) => Boolean)(a: M): Boolean =
    f(a |+| Monoid[M].zero, a)

  def monoidLawsE[M:Monoid](f: (M,M) => Boolean)(a: M, b:M, c:M): Boolean =
    assoc(f)(a,b,c) &&
      leftId(f)(a) && leftId(f)(b) && leftId(f)(c) &&
      rightId(f)(a) && rightId(f)(b) && rightId(f)(c)

  def monoidLaws[M:Monoid](a: M, b:M, c:M): Boolean = monoidLawsE((a:M,b:M) => a === b)(a,b,c)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(1000), workers = 8)

  test("intAddMon is a lawful monoid") {
    check((a: Int, b: Int, c: Int) => monoidLaws(a,b,c)(intAddMon))
  }

  // This test is expected to fail, (Float, 0, +) is not a monoid
  ignore("Float Sum is not a monoid!") {
    check((a: Float, b: Float, c: Float) => monoidLaws(a,b,c)(floatAddMon))
  }

  test("intMulMon is a lawful monoid") {
    check((a: Int, b: Int, c: Int) => monoidLaws(a,b,c)(intMulMon))
  }

  test("freeMon is a lawful monoid") {
    check((a: List[Int], b: List[Int], c: List[Int]) => monoidLaws(a,b,c)(freeMon))
  }

  test("firstMon is a lawful monoid") {
    check((a: Option[Int], b: Option[Int], c: Option[Int]) => monoidLaws(a,b,c)(firstMon))
  }

  test("optionMon is a lawful monoid") {
    // use a non-commutative underlying monoid
    check((a: Option[List[Int]], b: Option[List[Int]], c: Option[List[Int]]) =>
      monoidLaws(a,b,c)(optionMon(freeMon)))
  }

  // fixme: missing tests for endoMon and monFunMon

  test("pairMon is a lawful monoid") {
    // use a non-commutative underlying monoids
    check((a: (List[Int], Option[Int]), b: (List[Int], Option[Int]), c: (List[Int], Option[Int])) =>
      monoidLaws(a,b,c)(pairMon))
  }

  test("mconcat of Lists is flatten") {
    check((as: List[List[Int]]) =>
      mconcat(as) === as.flatten
    )
  }

  test("sum adds") {
    check((as: List[Int]) =>
      sum(as) === as.sum
    )
  }

  test("first returns first Some") {
    check((as: List[Option[Int]]) =>
      first(as) === as.find(_.isDefined).flatten
    )
  }

  test("foldMap maps and reduces") {
    check((as: List[Int]) =>
      foldMap((a: Int) => List(a,a))(as) === as.flatMap(a => List(a,a))
    )
  }

  test("allMonoid is a lawful monoid") {
    check((a: Boolean, b: Boolean, c: Boolean) =>
      monoidLaws(a,b,c)(allMonoid))
  }

  test("allMonoid mconcat == and") {
    check((as: List[Boolean]) =>
      mconcat(as) === as.forall(identity)
    )
  }

  test("appending predicates is and") {
    numbers === 1.to(100)
      .filter(_ % 2 === 0)
      .filter(_ >= 10)
      .filter(_ < 20)
  }

  def minGenerator[A:Arbitrary]: Gen[Min[A]] = arbitrary[Option[A]].map {
    case None => EmptyMin()
    case Some(n) => Min(n)
  }

  def maxGenerator[A:Arbitrary]: Gen[Max[A]] = arbitrary[Option[A]].map {
    case None => EmptyMax()
    case Some(n) => Max(n)
  }

  implicit def arbMin[A:Arbitrary]: Arbitrary[Min[A]] = Arbitrary(minGenerator)
  implicit def arbMax[A:Arbitrary]: Arbitrary[Max[A]] = Arbitrary(maxGenerator)


  test("minMonoid is a lawful monoid") {
    check((a: Min[Int], b: Min[Int], c: Min[Int]) =>
      monoidLaws(a,b,c)(Min.minMonoid))
  }

  test("minMonoid computes minimum") {
    check((a: Min[Int], b: Min[Int]) => (a,b) match {
            case (MinValue(aa), MinValue(bb)) => (a |+| b) === Min(List(aa,bb).min)
            case (EmptyMin(), b) => (a |+| b) === b
            case (a, EmptyMin()) => (a |+| b) === a
          }
    )
  }

  test("maxMonoid is a lawful monoid") {
    check((a: Max[Int], b: Max[Int], c: Max[Int]) =>
      monoidLaws(a,b,c)(Max.maxMonoid))
  }

  test("maxMonoid computes maximum") {
    check((a: Max[Int], b: Max[Int]) => (a,b) match {
            case (MaxValue(aa), MaxValue(bb)) => (a |+| b) === Max(List(aa,bb).max)
            case (EmptyMax(), b) => (a |+| b) === b
            case (a, EmptyMax()) => (a |+| b) === a
          }
    )
  }

  test("min computes minimum") {
    check((as: Vector[Int]) => min(as) match {
            case Some(m) => m === as.min
            case None => as.isEmpty
          })
  }

  test("minmax computes minimum and maximum") {
    check((as: Vector[Int]) => minmax(as) match {
            case Some((min,max)) => min === as.min && max === as.max
            case None => as.isEmpty
          })
  }

  test("Parallel same result") {
    // use non-commutative underlying monoid
    check((as: Vector[List[Int]]) => Parallel.mconcat(as) === mconcat(as))
  }

  test("Parallel faster") {
    val slowMonoid = new Monoid[Int] {
      def zero = 0

      def append(a: Int, b: => Int): Int = {
        val startTime = System.nanoTime();

        @tailrec
        def spin(ms: Int): Unit = {
          val sleepTime = ms*1000000L;
          if ((System.nanoTime() - startTime) < sleepTime) spin(ms)
        }

        spin(2)
        a + b
      }
    }

    def time[R](block: => R): Long = {
      val t0 = System.nanoTime()
      val _ = block
      val t1 = System.nanoTime()
      t1 - t0
    }

    val longVectors: Gen[Traversable[Int]] =
      Gen.choose(300, 600).flatMap {n =>
        Gen.containerOfN[Traversable, Int](n, arbitrary[Int])
      }

    check(
      forAll(longVectors)(
        as => {
          val timeSeq = time(mconcat(as)(slowMonoid))
          val timePar = time(Parallel.mconcat(as)(slowMonoid))
          timeSeq > timePar
        })
        , minSuccessful(50))
    }

  import Stats._

  val sampleGen: Gen[Double] = Gen.choose(-1000.0,1000.0)
  val samplesGen: Gen[Vector[Double]] = Gen.containerOf[Vector, Double](sampleGen)
  val mvGenerator: Gen[MeanVar] = samplesGen.map(MeanVar.sample)

  implicit val arbMeanV: Arbitrary[MeanVar] = Arbitrary(mvGenerator)

  def eqMV(a: MeanVar, b: MeanVar): Boolean = (a,b) match {
    case (MeanVarV(m1a,m2a,na), MeanVarV(m1b,m2b,nb)) =>
      na === nb && m1a === m1b +- 0.0001 && m2a === m2b +- 0.0001
    case (EmptyMeanVar, EmptyMeanVar) => true
    case _ => false
  }

  test("MeanVar has a lawful monoid") {
    check((a: MeanVar, b: MeanVar, c: MeanVar) =>
      monoidLawsE(eqMV)(a,b,c))
  }

  test("MeanVar computes trivial statistics") {
    check(
      forAll(Gen.choose(1,100))(
        size => {
          val mv = MeanVar.sample(Vector.fill(size)(42.0))
          MeanVar.mean(mv).get === (42.0 +- 1e-6) &&
            MeanVar.variance(mv).get === (0.0 +- 1e-6) &&
            MeanVar.sampleSize(mv) === size
        }
      )
    )
  }

  val batchGen: Gen[Vector[Vector[Double]]] = for {
    // total number of samples
    n <- Gen.choose(1000, 2000)

    // number of batches: samples will be splitted in this many parts
    numBatches <- Gen.chooseNum[Int](1,100)

    cuts <- Gen.containerOfN[Vector, Int](numBatches - 1, Gen.chooseNum(0, n-1)).map(_.sorted :+ n)
    sizes = (cuts.+:(0), cuts).zipped.map((from,to) => to - from)
    batches <- Gen.sequence[Vector[Vector[Double]], Vector[Double]](sizes.map (Gen.containerOfN[Vector, Double](_, sampleGen)))
  } yield batches

  test("MeanVar computes mean/var incrementally") {
    import org.scalacheck.Shrink
    implicit val noShrink: Shrink[Vector[Vector[Double]]] = Shrink.shrinkAny

    check(
      forAll(batchGen)(
        groups => {
          val mv = foldMap(MeanVar.sample)(groups)
          val (expectedMean, expectedVar) = (0.0, 2000*2000/12.0)
          val meanEstimatorDispersion = math.sqrt(expectedVar/MeanVar.sampleSize(mv))
          val expectedKurtosis = 6.0/5
          val varianceEstimatorDispersion =
            math.sqrt(math.abs(expectedKurtosis - 1) * expectedVar * expectedVar / MeanVar.sampleSize(mv))
          MeanVar.mean(mv).get === expectedMean +- 6*meanEstimatorDispersion &&
            MeanVar.variance(mv).get === expectedVar +- 8*varianceEstimatorDispersion
        }
      )
    )
  }
}
