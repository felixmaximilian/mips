package de.mips.Node

import de.mips.geometric.Ball

final case class LeafNode(pointIdx: Seq[Int],
                          override val ball: Ball) extends Node {
  override def toString() = {
    s"LeafNode with ${ball.toString}} \n " +
      s"and data size of ${pointIdx.length} (example point: ${pointIdx.take(1)}})"
  }
}
