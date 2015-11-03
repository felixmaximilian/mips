package de.mips.geometric

import java.io.Serializable

import breeze.linalg.DenseVector

final case class Ball(mu: DenseVector[Double], radius: Double) extends Serializable
