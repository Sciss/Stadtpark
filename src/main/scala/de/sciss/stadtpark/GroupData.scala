package de.sciss.stadtpark

/**
  * @param idx            the zero-based index of the group
  * @param channels       the channels of the group, zero-based indices
  * @param material       the material file name
  */
case class GroupData(idx: Int, channels: Vec[Int], material: String) {
  def numChannels = channels.size
}
