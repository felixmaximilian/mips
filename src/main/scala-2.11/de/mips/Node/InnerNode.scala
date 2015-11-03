package de.mips.Node

import de.mips.geometric.Ball

final case class InnerNode(override val ball: Ball,
                           leftChild: Node,
                           rightChild: Node) extends Node {
  override def toString() = {
    s"InnerNode with ${ball.toString}}."
  }
}
