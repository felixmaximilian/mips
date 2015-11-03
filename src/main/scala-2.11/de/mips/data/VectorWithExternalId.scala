package de.mips.data

import java.io.Serializable

import breeze.linalg.DenseVector

final case class VectorWithExternalId(id: Int, features: DenseVector[Double]) extends Serializable {
  override def toString() = {
    s"IdWithFeatures(${id}},${features}})"
  }
}
