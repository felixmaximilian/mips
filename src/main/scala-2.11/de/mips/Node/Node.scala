package de.mips.Node

import java.io.Serializable
import de.mips.geometric.Ball

trait Node extends Serializable {
   def ball: Ball
 }
