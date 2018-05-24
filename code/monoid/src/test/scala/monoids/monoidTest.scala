package monoids

import scala.annotation.tailrec
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import org.scalacheck.Prop.{forAll}
import org.scalactic.anyvals.PosInt

import scala.collection.immutable._
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool

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

  test("intAddMon adds") {
    check((a: Int, b: Int) =>  intAddMon.append(a,b) === a + b)
  }

  // This test is expected to fail, (Float, 0, +) is not a monoid
  ignore("Float Sum is not a monoid!") {
    check((a: Float, b: Float, c: Float) => monoidLaws(a,b,c)(floatAddMon))
  }

  test("intMulMon is a lawful monoid") {
    check((a: Int, b: Int, c: Int) => monoidLaws(a,b,c)(intMulMon))
  }

  test("intMulMon multiplies") {
    check((a: Int, b: Int) =>  intMulMon.append(a,b) === a * b)
  }

  test("freeMon is a lawful monoid") {
    check((a: List[Int], b: List[Int], c: List[Int]) => monoidLaws(a,b,c)(freeMon))
  }

  test("freeMon concatenates") {
    check((a: List[Int], b: List[Int]) =>  freeMon.append(a,b) === a ++ b)
  }

  test("firstMon is a lawful monoid") {
    check((a: Option[Int], b: Option[Int], c: Option[Int]) => monoidLaws(a,b,c)(firstMon))
  }

  test("firstMon returns first Some") {
    check((as: List[Option[Int]]) => mconcat(as)(firstMon) === as.filter(_.isDefined).headOption.flatten)
  }

  test("optionMon is a lawful monoid") {
    // use a non-commutative underlying monoid
    check((a: Option[List[Int]], b: Option[List[Int]], c: Option[List[Int]]) =>
      monoidLaws(a,b,c)(optionMon(freeMon)))
  }

  test("optionMon combines with the underlying monoid") {
    // use a non-commutative underlying monoid
    check((a: Option[List[Int]], b: Option[List[Int]]) => {
            val res = optionMon(freeMon[Int]).append(a,b)
            res === ((a,b) match {
              case (None, Some(bb)) => Some(bb)
              case (Some(aa), None) => Some(aa)
              case (Some(aa), Some(bb)) => Some(aa ++ bb)
              case _ => None
            })
          }
    )
  }

  // fixme: missing tests for endoMon and monFunMon

  test("pairMon is a lawful monoid") {
    // use a non-commutative underlying monoids
    check((a: (List[Int], Option[Int]), b: (List[Int], Option[Int]), c: (List[Int], Option[Int])) =>
      monoidLaws(a,b,c)(pairMon))
  }

  test("pairMon uses underlying monoid") {
    // use a non-commutative underlying monoids
    check((a: (List[Int], Option[Int]), b: (List[Int], Option[Int])) => (a,b) match {
            case ((a1,a2), (b1,b2)) => {
              val (res1,res2) = pairMon(freeMon[Int], optionMon(intAddMon)).append(a,b)
              res1 === freeMon[Int].append(a1,b1) &&
                res2 === optionMon(intAddMon).append(a2,b2)
            }
          })
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

  test("foldMap maps and reduces") {
    check((as: List[Int]) =>
      foldMap(as, (a: Int) => List(a,a)) === as.flatMap(a => List(a,a))
    )
  }

  test("foldMap2 same as foldMap") {
    check((as: Vector[Int]) =>
      foldMap(as, (a: Int) => List(a,a)) === foldMap2(as, (a: Int) => List(a,a)))
  }

  test("concat same result as scala's") {
    check((as: List[List[Int]]) =>
      concat(as) === List.concat(as:_*)
    )
  }

  test("sumO of empty is None") {
    check(sumO[Int](List()) === None)
  }

  test("sumO filters ignore Nothings") {
    check((as: Vector[Option[Int]]) =>
      sumO[Int](as) === sumO[Int](as.filter(_.isDefined))
    )
  }

  test("sumO adds Somes") {
    check(forAll(Gen.nonEmptyListOf[Int](arbitrary[Int]))(as =>
      sumO[Int](as.map(Some(_))) === Some(as.sum)
    ))
  }

  test("sumO example") {
    check(sumO[Int](Vector(Some(5), Some(1), None, Some(3))) === Some(9))
  }

  test("filter same results as scala's") {
    check((as: List[Int], pred: Int => Boolean) =>
      filter(as, pred) === as.filter(pred)
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
    numbers === (1 to 100)
      .filter(_ % 2 === 0)
      .filter(_ >= 10)
      .filter(_ < 20)
  }

  test("min computes minimum") {
    check((as: Vector[Int]) => min(as) match {
            case Some(m) => m === as.min
            case None => as.isEmpty
          })
  }

  test("max computes maximum") {
    check((as: Vector[Int]) => max(as) match {
            case Some(m) => m === as.max
            case None => as.isEmpty
          })
  }

  test("minmax computes minimum and maximum") {
    check((as: Vector[Int]) => minmax(as) match {
            case Some((min,max)) => min === as.min && max === as.max
            case None => as.isEmpty
          })
  }

  def mkTaskSupport(warmup: Boolean): ForkJoinTaskSupport = {
    val pool = new java.util.concurrent.ForkJoinPool
    val ts = new scala.collection.parallel.ForkJoinTaskSupport(pool)
    val _: Any = if (warmup) Parallel.mconcat(1.to(100))(ts)(intAddMon)
    ts
  }

  def shutdownTS(ts: ForkJoinTaskSupport): Unit = {
    val _ = ts.forkJoinPool.shutdownNow
    val _2 = ts.forkJoinPool.awaitTermination(1, java.util.concurrent.TimeUnit.SECONDS)
  }

  def withTaskSupport[A](warmup: Boolean)(task: ForkJoinTaskSupport => A): A = {
    val ts = mkTaskSupport(warmup)
    try { task(ts) }
    finally { shutdownTS(ts) }
  }

  test("Parallel same result") {
    // use non-commutative underlying monoid
    check((as: Vector[List[Int]]) =>

      withTaskSupport(false) { ts =>
        Parallel.mconcat(as)(ts) === mconcat(as)
      }
    )
  }

  if (Runtime.getRuntime.availableProcessors > 1)
    test("Parallel faster") {

      val slowMonoid = new Monoid[Int] {
        def zero = 0

        def append(a: Int, b: => Int): Int = {
          Thread.sleep(2)
          a + b
        }
      }

      def time[R](block: => R): Long = {
        val t0 = System.nanoTime()
        val _ = block
        val t1 = System.nanoTime()
        t1 - t0
      }

      val longVectors: Gen[Vector[Int]] =
        Gen.choose(300, 600).flatMap {n =>
          Gen.containerOfN[Vector, Int](n, arbitrary[Int])
        }

      import org.scalacheck.Shrink
      implicit val noShrink: Shrink[Vector[Int]] = Shrink.shrinkAny

      check(
        forAll(longVectors)(
          as =>
            withTaskSupport(false) { ts =>
              val timePar = time(Parallel.mconcat(as)(ts)(slowMonoid))
              val timeSeq = time(mconcat(as)(slowMonoid))
              timeSeq > timePar
            }
        ), minSuccessful(50))
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
          val mv = foldMap(groups, MeanVar.sample)
          val (expectedMean, expectedVar) = (0.0, 2000*2000/12.0)
          val meanEstimatorDispersion = math.sqrt(expectedVar/MeanVar.sampleSize(mv))
          val expectedKurtosis = 6.0/5
          val varianceEstimatorDispersion =
            math.sqrt(math.abs(expectedKurtosis - 1) * expectedVar * expectedVar / MeanVar.sampleSize(mv))
          MeanVar.mean(mv).get === expectedMean +- 8*meanEstimatorDispersion &&
            MeanVar.variance(mv).get === expectedVar +- 8*varianceEstimatorDispersion
        }
      )
    )
  }
}
