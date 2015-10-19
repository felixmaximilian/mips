import breeze.linalg.functions.euclideanDistance
import breeze.linalg.{DenseVector, norm}
case class IdWithFeatures(id: Int,features: DenseVector[Double])
@SerialVersionUID(100L)
class BallTree(points: IndexedSeq[IdWithFeatures]) extends Serializable {
  case class BestMatch(index: Int, value: Double)

  case class Query(point: DenseVector[Double], var bestMatches: BoundedPriorityQueue[BestMatch])

  case class Ball(mu: DenseVector[Double], radius: Double)


  abstract class Node(val pointIdx: Seq[Int], val ball: Ball) {
  }

  case class LeafNode(override val pointIdx: Seq[Int],
                      override val ball: Ball) extends Node(pointIdx, ball) {

  }

  case class InnerNode(override val pointIdx: Seq[Int],
                       override val ball: Ball,
                       leftChild: Node,
                       rightChild: Node) extends Node(pointIdx, ball)

  val randomIntGenerator = scala.util.Random

  val leafThreshold: Int = 50
  val dim = points(0).features.length

  val root = makeBallTree(points.indices)

  private def mean(pointIdx: Seq[Int]): DenseVector[Double] = {
    pointIdx.foldLeft(DenseVector.zeros[Double](dim))((sumSoFar: DenseVector[Double], idx: Int) => sumSoFar + points(idx).features).map { scalar: Double => scalar / pointIdx.length }
    //    pointIdx.map(points(_)).sum.map(scalar => scalar / indexedSeq.length).toDenseVector
  }

  private def radius(pointIdx: Seq[Int], point: DenseVector[Double]): Double = {
    pointIdx.map { idx => {
      val ed = euclideanDistance(points(idx).features, point)
      //      ed * ed
      ed
    }
    }.max
  }

  private def upperBoundMaximumInnerProduct(query: Query, node: Node): Double = {
    (query.point dot node.ball.mu).asInstanceOf[Double] + (node.ball.radius * norm(query.point))
  }

  def linearSearch(query: Query, node: Node): Unit = {
    val bestMatchesCandidates = node.pointIdx.map { idx => BestMatch(idx, (query.point dot points(idx).features).asInstanceOf[Double]) }
    query.bestMatches ++= (bestMatchesCandidates)
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
    if (pointIdx.length <= leafThreshold) {
      //Leaf Node
      LeafNode(pointIdx, ball)
    } else {
      //split set
      val (pivot1, pivot2) = makeBallSplit(pointIdx)
      val (leftSubSet, rightSubSet) = divideSet(pointIdx, pivot1, pivot2)
      val leftChild = makeBallTree(leftSubSet)
      val rightChild = makeBallTree(rightSubSet)
      InnerNode(pointIdx, ball, leftChild, rightChild)
    }
  }

  private def traverseTree(query: Query, node: Node = root): Unit = {
    if (query.bestMatches.head.value <= upperBoundMaximumInnerProduct(query, node)) {
      //This node has potential
      node match {
        case LeafNode(_, _) => linearSearch(query, node)
        case InnerNode(_, _, leftChild, rightChild) => {
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
      }
    } else
      return
  }

  def findMultipleMaximumInnerProducts(queries: IndexedSeq[DenseVector[Double]], k: Int = 1): Seq[Seq[BestMatch]] = {
    queries.map { query: DenseVector[Double] => {
      findMaximumInnerProducts(query, k)
    }
    }
  }

  def findMaximumInnerProducts(queryPoint: DenseVector[Double], k: Int = 1): Seq[BestMatch] = {
    val bestMatches = new BoundedPriorityQueue[BestMatch](k)(Ordering.by(_.value))
    bestMatches += (BestMatch(-1, Double.NegativeInfinity))
    val query = Query(queryPoint, bestMatches)
    traverseTree(query, root)
    query.bestMatches.toArray.sorted(Ordering.by((_: BestMatch).value).reverse)
  }
}

