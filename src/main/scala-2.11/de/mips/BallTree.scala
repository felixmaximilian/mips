package de.mips

import java.io.Serializable

import breeze.linalg.functions.euclideanDistance
import breeze.linalg.{DenseVector, norm}
import de.mips.Node.{Node, LeafNode, InnerNode}
import de.mips.data.{BestMatch, VectorWithExternalId}
import de.mips.geometric.Ball

final case class BallTree(points: IndexedSeq[VectorWithExternalId],leafSize:Int = 50) extends Serializable {

  //using java version of Random() cause the scala version is only serializable since scala version 2.11
  val randomIntGenerator = new java.util.Random()

  val dim = points(0).features.length

  val pointIdx = points.indices
  val root = makeBallTree(pointIdx)

  private def mean(pointIdx: Seq[Int]): DenseVector[Double] = {
    pointIdx.foldLeft(DenseVector.zeros[Double](dim))((sumSoFar: DenseVector[Double], idx: Int) => sumSoFar + points(idx).features).map { scalar: Double => scalar / pointIdx.length }
  }

  private def radius(pointIdx: Seq[Int], point: DenseVector[Double]): Double = {
    pointIdx.map { idx => {
      euclideanDistance(points(idx).features, point)
    }
    }.max
  }

  private def upperBoundMaximumInnerProduct(query: Query, node: Node): Double = {
    query.statistics.boundEvaluations += 1
    (query.point dot node.ball.mu).asInstanceOf[Double] + (node.ball.radius * query.normOfQueryPoint)
  }

  private def linearSearch(query: Query, node: LeafNode): Unit = {
    val bestMatchesCandidates = node.pointIdx.map { idx => BestMatch(idx, (query.point dot points(idx).features).asInstanceOf[Double]) }
    query.bestMatches ++= (bestMatchesCandidates)
    query.statistics.innerProductEvaluations = query.statistics.innerProductEvaluations + node.pointIdx.length
  }

  private def makeBallSplit(pointIdx: Seq[Int]): (Int, Int) = {
    //finding two points in Set that have largest distance
    val randPoint = points(pointIdx(randomIntGenerator.nextInt(pointIdx.length))).features
    //TODO: Check if not using squared euclidean distance is ok
    val pivotPoint1: Int = pointIdx.map { idx: Int => {
      val ed = euclideanDistance(randPoint, points(idx).features)
      (idx, ed * ed)
    }
    }.maxBy(_._2)._1
    val pivotPoint2: Int = pointIdx.map { idx: Int => {
      val ed = euclideanDistance(points(pivotPoint1).features, points(idx).features)
      (idx, ed * ed)
    }
    }.maxBy(_._2)._1
    (pivotPoint1, pivotPoint2)
  }

  private def divideSet(pointIdx: Seq[Int], pivot1: Int, pivot2: Int): (Seq[Int], Seq[Int]) = {
    pointIdx.partition(idx => euclideanDistance(points(idx).features, points(pivot1).features) <= euclideanDistance(points(idx).features, points(pivot2).features))
  }

  private def makeBallTree(pointIdx: Seq[Int]): Node = {
    val mu = mean(pointIdx)
    val ball = Ball(mu, radius(pointIdx, mu))
    if (pointIdx.length <= leafSize) {
      //Leaf Node
      LeafNode(pointIdx, ball)
    } else {
      //split set
      val (pivot1, pivot2) = makeBallSplit(pointIdx)
      val (leftSubSet, rightSubSet) = divideSet(pointIdx, pivot1, pivot2)
      val leftChild = makeBallTree(leftSubSet)
      val rightChild = makeBallTree(rightSubSet)
      InnerNode(ball, leftChild, rightChild)
    }
  }

  private def traverseTree(query: Query, node: Node = root): Unit = {
    if (query.bestMatches.head.value <= upperBoundMaximumInnerProduct(query, node)) {
      //This node has potential
      node match {
        case LeafNode(_, _) => linearSearch(query, node.asInstanceOf[LeafNode])
        case InnerNode(ball, leftChild, rightChild) => {
          val boundLeft = upperBoundMaximumInnerProduct(query, leftChild)
          val boundRight = upperBoundMaximumInnerProduct(query, rightChild)
          if (boundLeft <= boundRight) {
            traverseTree(query, rightChild)
            traverseTree(query, leftChild)
          } else {
            traverseTree(query, leftChild)
            traverseTree(query, rightChild)
          }
        }
        case x => throw new RuntimeException(s"default case in match has been visited for type${x.getClass}: " + x.toString)
      }
    } else {
      //ignoring this subtree
      query.statistics.subTreeIgnores += 1
      return
    }
  }

  def findMultipleMaximumInnerProducts(queries: IndexedSeq[DenseVector[Double]], k: Int = 1): Seq[Seq[BestMatch]] = {
    queries.map { query: DenseVector[Double] => {
      findMaximumInnerProducts(query, k)
    }
    }
  }

  def findMaximumInnerProducts(queryPoint: DenseVector[Double], k: Int = 1): Seq[BestMatch] = {
    val query = new Query(queryPoint, k)
    query.statistics.pointsInTree = pointIdx.length
    traverseTree(query)
    val sortedBestMatches = query.bestMatches.toArray.sorted(Ordering.by((_: BestMatch).value).reverse)
    println(query.statistics)
    //replace internal sequence id with the actual external item id
    sortedBestMatches.transform(bm => BestMatch(points(bm.index).id, bm.value))
  }

  override def toString() = {
    s"Balltree with data size of ${points.length} (${points.take(1)})"
  }
}

private final class Query(val point: DenseVector[Double], val normOfQueryPoint: Double, var bestMatches: BoundedPriorityQueue[BestMatch], var statistics: Statistics) extends Serializable {

  def this(point: DenseVector[Double], bestK: Int) = this(point, norm(point), Query.createQueue(bestK), Statistics())

  override def toString() = {
    s"Query with point ${point}} \n " +
      s"and bestMatches of size of ${bestMatches.size} (bestMatch example: ${bestMatches.take(1)}})"
  }
}

private object Query {
  def createQueue(k: Int) = {
    val bestMatches = new BoundedPriorityQueue[BestMatch](k)(Ordering.by(_.value))
    bestMatches += BestMatch(-1, Double.NegativeInfinity)
  }
}

private final case class Statistics(var pointsInTree:Int = 1, var innerProductEvaluations: Int = 0, var subTreeIgnores: Int = 0, var subtreesVisited: Int = 0, var boundEvaluations: Int = 0)  extends Serializable{
  override def toString() = {
    s"innerProductEvaluations: ${innerProductEvaluations}, \n" +
      s"subTreeIgnores: ${subTreeIgnores}, \n" +
      s"subtreesVisited: ${subtreesVisited}, \n" +
      s"boundEvaluations: ${boundEvaluations}, \n" +
      s"innerProductEvaluationsRelativeToNaiveSearch: ${innerProductEvaluations.toDouble/pointsInTree*100} %"
  }
}