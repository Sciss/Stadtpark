package de.sciss.stadtpark

import de.sciss.lucre.stm.Sys

object Group {
  case class Config[S <: Sys[S]](channels: Vec[Int], material: String, loopOverlap: Motion[S]) {
    def numChannels = channels.size
  }
}
class Group[S <: Sys[S]](config: Group.Config[S]) {

}
