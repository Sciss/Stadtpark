package de.sciss.stadtpark

import de.sciss.synth.proc._
import de.sciss.lucre.stm
import de.sciss.mellite.{Gain, ProcActions, Document}
import de.sciss.file._
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.span.Span
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.mellite.gui.ActionBounceTimeline
import de.sciss.lucre.stm.Cursor
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import scala.concurrent.{blocking, Await}
import scala.concurrent.duration.Duration
import java.io.File
import de.sciss.synth.io.{SampleFormat, AudioFileSpec}
import de.sciss.synth.io.AudioFileType.AIFF

object Group {
  case class Config[S <: Sys[S]](channels: Vec[Int], material: String, loopOverlap: Motion[S]) {
    def numChannels = channels.size
  }

  // bug in Scala 2.10 - "bad symbolic reference. A signature in Group.class refers to type Modifiable"
  type ProcMod[S <: Sys[S]] = BiGroup.Modifiable[S, Proc[S], Proc.Update[S]]

  def apply[S <: Sys[S]](document: Document[S], group: ProcMod[S] /* ProcGroup.Modifiable[S] */, config: Config[S])
                        (implicit tx: S#Tx, cursor: Cursor[S]): Group[S] = {
    implicit val ser = ProcGroup.Modifiable.serializer[S]
    val groupH = tx.newHandle(group)
    new Impl(document, groupH, config)
  }

  private class Impl[S <: Sys[S]](document: Document[S], groupH: stm.Source[S#Tx, ProcMod[S] /* ProcGroup.Modifiable[S] */],
                                  config: Group.Config[S])(implicit cursor: Cursor[S])
    extends Group[S] {

    def iterate(): Processor[Unit, _] = {
      requireNoTxn()

      // locate the previous iteration's span

      val bounceSpan = cursor.step { implicit tx =>
        val group = groupH()
        group.nearestEventBefore(Long.MaxValue - 1).fold[Span] {
          val originF   = audioDir / "AlphavilleIlArrive.aif"
          val origin    = Util.resolveAudioFile(document, originF)
          val originV   = origin.value
          val time      = sampleRate.toLong
          val spanV     = Span(0L, originV.spec.numFrames)
          val busOption = Option.empty[Expr[S, Int]]
          val (_, proc) = ProcActions.insertAudioRegion(group = group, time = time, track = 0, grapheme = origin,
            selection = spanV, bus = busOption)
          val overLen   = math.min(spanV.length, (config.loopOverlap.step * sampleRate).toLong)
          if (overLen > MinLen) {
            val fadeLen = overLen/4
            if (fadeLen > 0) {
              val fadeOut = FadeSpec.Value(numFrames = fadeLen)
              proc.attributes.put(ProcKeys.attrFadeOut, fadeAttr(fadeOut))
            }
            val timeOver  = time + spanV.length - fadeLen
            val spanVOver = Span(0L, overLen)
            val (_, procOver) = ProcActions.insertAudioRegion(group = group, time = timeOver, track = 1,
              grapheme = origin, selection = spanVOver, bus = busOption)
            if (fadeLen > 0) {
              val fadeIn = FadeSpec.Value(numFrames = fadeLen)
              procOver.attributes.put(ProcKeys.attrFadeIn, fadeAttr(fadeIn))
            }
            spanV union spanVOver
          } else {
            spanV
          }

        } { stop =>
          println(s"Jo chuck $stop")
          ???
        }
      }

      // bounce it

      val serverCfg   = Server.Config()
      val bounceFile  = File.createTempFile("bounce", ".aif", tmpDir)
      val bounceSpec  = AudioFileSpec(AIFF, SampleFormat.Float, numChannels = 1, sampleRate = sampleRate)
      ActionBounceTimeline.specToServerConfig(bounceFile, bounceSpec, serverCfg)
      val bounceGain  = Gain(0f, normalized = false)
      val bounceChans = Vec(0 to 0)
      val bounceCfg   = ActionBounceTimeline.PerformSettings(groupH, serverCfg, bounceGain, bounceSpan, bounceChans)
      val bounce      = ActionBounceTimeline.perform(document, settings = bounceCfg)

      processor[Unit](s"Iteration<${config.material}>") { p =>
        p.await(bounce)
      }
    }
  }
}
trait Group[S <: Sys[S]] {
  def iterate(): Processor[Unit, _]
}