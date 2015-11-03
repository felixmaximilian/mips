# Balltree Maximum Inner Product Search

## Introduction 
This project is a Scala implementation of a Balltree data structure for fast maximum inner product search. 

The maximum inner product search problem is given as follows. Given a query point q of dimension d. 
In a set of points p ∈ P of size N and of same dimensionality, find the point p that maximizes the inner product between q and p (<q,p>). 
This problem can also be interpreted as a k-nearest-neighbour search problem where the similarity between points are measured by the inner product instead of the euclidean distance.
This algorithm reduces the complexity of the naive approach (linear search) from O(d N) to average complexity O(d log N).

The implemented algorithm is taken from "Maximum Inner-Product Search using Tree Data-structures" (Parikshit Ram, Alexander G. Gray, 2012).

## Using this Implementation

The balltree is constucted by passing the vectors with their external ids:
```
val pointsWithExternalId: Array(Int,DenseVector[Double]) = ... // data given by you
val points: VectorWithExternalId = pointsWithExternalId.map{case(externalId,featureVector) => VectorWithExternalId(externalId,featureVector)}
val ballTree = BallTree(points)
```
Search for the 5 best matches is triggered by:
```
ballTree.findMaximumInnerProducts(features,5)
```

You can optionally pass the size of the leaf nodes (number of elements contained). The default is set to 50 elements per leaf node. Once the tree is constructed its not meant to be changed (immutable).

### Hints:
This implementation works nice together the latent vectors obtained by spark's mllib matrix factorization algorithm.
Build the tree like above on your spark master, then
```
val model: MatrixFactorizationModel = ...
val userFeatures : (Int,DenseVector[Double]) = model.userFeatures.map{case(userId: Int,features: Array[Double]) => (userId,DenseVector(features)}
val ballTreeBC = sc.broadcast(ballTree)
val recommendationsForUser = userFeatures.map{case(userId: Int,features: DenseVector[Double]) => (userId,ballTree.findMaximumInnerProducts(features,5))}
```
to let the spark executors predict the 5 best matches for each user.

## License

This Software is licensed under Apache License Version 2.0. It uses the Breeze library for numerical processing and includes a copy of a file from the Apache Spark project (BoundedPriorityQueue).

