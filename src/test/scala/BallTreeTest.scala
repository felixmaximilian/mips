import java.io._

import breeze.linalg.DenseVector
import org.specs2.specification.core.Fragment

import scala.collection.immutable

class BallTreeTest extends org.specs2.mutable.Specification {
  def naiveSearch(haystack: IndexedSeq[DenseVector[Double]], needle: DenseVector[Double]) = {

    haystack.map(c => (c, c.t * needle)).sorted(Ordering.by((_: (DenseVector[Double], Double))._2).reverse)
  }

  def time[R](identifier: String = "", block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println(s"Elapsed time for ${identifier}: " + (t1 - t0) + "ms")
    result
  }

  def compareToGroundTruth(tree: BallTree, haystack: IndexedSeq[IdWithFeatures], needle: DenseVector[Double], k: Int = 1) = {
    val nearest = tree.findMaximumInnerProducts(needle, k)
    val groundTruth = haystack.indices.map { point => (point, haystack(point).features.t * needle) }

    val better = groundTruth.filter(n => n._2 > nearest.head.value).sorted
    (better, nearest)
  }

  def test(haystack: IndexedSeq[IdWithFeatures], needles: DenseVector[Double]*) = {
    println
    val tree = new BallTree(haystack)
    //    if (haystack.size < 20) tree.foreach(println)
    val needle = needles(0)
    val nearest = time("knn", tree.findMaximumInnerProducts(needle).head)
    //      println(nearest)
    // Brute force proof
    val groundTruth = time("naive", haystack.map { point => point.features.t * needle })
    val better = groundTruth.filter(n => n > nearest.value)
      .sorted
    //      assert(better.isEmpty, s"Found ${better.size} closer than ${nearest.value} e.g. ${better.head}")
    nearest
  }



  "search" should {
    "be exact" in {
      val uniform: immutable.IndexedSeq[DenseVector[Double]] = for (x <- 1 to 10; y <- 1 to 10; z <- 1 to 10) yield DenseVector((x * 2).toDouble, (y * 2).toDouble, (z * 2).toDouble)
      assume(uniform.size == 1000)
      val uniformAsMap: immutable.IndexedSeq[IdWithFeatures] = uniform.indices.map{i:Int =>IdWithFeatures(i, uniform(i))}
      val needle = DenseVector(9.0, 10.0, 11.0)
      val nearest = test(uniformAsMap, needle)

      uniform(nearest.index) mustEqual naiveSearch(uniform, needle).head._1
      //      DenseVector(0.0, 0.0, 0.0), DenseVector(2.0, 2.0, 20.0),
    }
  }

  "Balltree with uniform data" should {
    val uniform = for (x <- 1 to 10; y <- 1 to 10; z <- 1 to 10) yield DenseVector((x * 2).toDouble, (y * 2).toDouble, (z * 2).toDouble)
    assume(uniform.size == 1000)

    val uniformAsMap: immutable.IndexedSeq[IdWithFeatures] = uniform.indices.map{i:Int =>IdWithFeatures(i, uniform(i))}

    val tree = new BallTree(uniformAsMap)

    Fragment.foreach(Seq(DenseVector(0.0, 0.0, 0.0), DenseVector(2.0, 2.0, 20.0), DenseVector(9.0, 10.0, 11.0))) { vec =>
      "find vector most similar to " + vec ! {
        val (better, _) = compareToGroundTruth(tree, uniformAsMap, vec)
        better must beEmpty
      }
    }
  }

  "Balltree with random data " should {
    "be correct" in {
      scala.util.Random.setSeed(10)
      def random(n: Int) = (1 to n).map(_ => (scala.util.Random.nextDouble - 0.5) * 2)

      val haystack = (1 to 100000).map(_ => DenseVector(random(3).toArray))
      val haystackAsMap: immutable.IndexedSeq[IdWithFeatures] = haystack.indices.map{i:Int =>IdWithFeatures(i, haystack(i))}

      val tree = new BallTree(haystackAsMap)
      val (better, nearest) = compareToGroundTruth(tree, haystackAsMap, DenseVector(random(3).toArray))
      better must beEmpty
    }
  }

  "Balltree with random data and number of best matches " should {
    "be correct" in {
      scala.util.Random.setSeed(10)
      def random(n: Int) = (1 to n).map(_ => (scala.util.Random.nextDouble - 0.5) * 2)
      val k: Int = 5

      val haystack = (1 to 100000).map(_ => DenseVector(random(3).toArray))

      val haystackAsMap: immutable.IndexedSeq[IdWithFeatures] = haystack.indices.map{i:Int =>IdWithFeatures(i, haystack(i))}

      val needle = DenseVector(random(3).toArray)
      val groundTruth = naiveSearch(haystack, needle)

      val tree = new BallTree(haystackAsMap)

      val (better, nearest) = compareToGroundTruth(tree, haystackAsMap, needle, k)
      better must beEmpty
      nearest must haveSize(k)
    }
  }
  
  "Balltree" should {
    "be serializable" in {
      val uniform: immutable.IndexedSeq[DenseVector[Double]] = for (x <- 1 to 10; y <- 1 to 10; z <- 1 to 10) yield DenseVector((x * 2).toDouble, (y * 2).toDouble, (z * 2).toDouble)
      assume(uniform.size == 1000)
      val uniformAsMap: immutable.IndexedSeq[IdWithFeatures] = uniform.indices.map{i:Int =>IdWithFeatures(i, uniform(i))}
      val tree = new BallTree(uniformAsMap)

      val fos = new FileOutputStream("../a.tmp")
      val oos = new ObjectOutputStream(fos)
      
      oos.writeObject(tree)
      oos.close
     
      fos.close

      val fis = new FileInputStream("../a.tmp")
      val ois = new ObjectInputStream(fis)
      val treeDeserialized : BallTree = ois.readObject().asInstanceOf[BallTree]
      println(treeDeserialized.findMaximumInnerProducts(DenseVector(9.0, 10.0, 11.0)))
      true must beEqualTo(true)
//      oos.writeObject(tree) must not(throwA[IOException])
    }
  }
}
