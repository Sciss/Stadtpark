package de.sciss.stadtpark

import de.sciss.lucre.stm.InMemory

object Test extends App {
  type S  = InMemory

  val c1  = Group.Config[S](Vec(3, 4), "Raspad441.aif", 1.0)
  val g1  = new Group(c1)
}
