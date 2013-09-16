package de.sciss.stadtpark

object Group {
  case class Config(channels: Vec[Int], material: String) {
    def numChannels = channels.size
  }
}
class Group(config: Group.Config) {

}
