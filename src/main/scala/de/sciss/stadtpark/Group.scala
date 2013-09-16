package de.sciss.stadtpark

import de.sciss.synth.proc.ProcGroup
import de.sciss.lucre.stm
import de.sciss.mellite.Document
import de.sciss.mellite.gui.ActionArtifactLocation
import de.sciss.file._
import de.sciss.lucre.synth.Sys

object Group {
  case class Config[S <: Sys[S]](channels: Vec[Int], material: String, loopOverlap: Motion[S]) {
    def numChannels = channels.size
  }

  def apply[S <: Sys[S]](document: Document[S], group: ProcGroup[S], config: Config[S])
                        (implicit tx: S#Tx): Group[S] = {
    implicit val ser = ProcGroup.serializer[S]
    val groupH = tx.newHandle(group)
    new Impl(document, groupH, config)
  }

  private class Impl[S <: Sys[S]](document: Document[S], groupH: stm.Source[S#Tx, ProcGroup[S]],
                                  config: Group.Config[S])
    extends Group[S] {

    def iterate()(implicit tx: S#Tx): Unit = {
      val group = groupH()
      group.nearestEventBefore(Long.MaxValue - 1).fold[Unit] {
        val originF = audioDir / "AlphavilleIlArrive.aif"
        val origin  = Util.resolveAudioFile(document, originF)

      } { stop =>
        println(s"Jo chuck $stop")

      }
    }
  }
}
trait Group[S <: Sys[S]] {
  def iterate()(implicit tx: S#Tx): Unit
}