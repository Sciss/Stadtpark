package de.sciss.stadtpark

import de.sciss.synth.proc._
import de.sciss.lucre.stm
import de.sciss.mellite.{Gain, ProcActions, Document}
import de.sciss.file._
import de.sciss.lucre.synth.{InMemory, Server, Sys}
import de.sciss.span.Span
import de.sciss.lucre.expr.Expr
import de.sciss.mellite.gui.ActionBounceTimeline
import de.sciss.lucre.stm.Cursor
import de.sciss.processor.Processor
import java.io.File
import de.sciss.synth.io.{SampleFormat, AudioFileSpec}
import de.sciss.synth.io.AudioFileType.AIFF
import de.sciss.strugatzki.{FeatureCorrelation, FeatureExtraction}
import scala.annotation.tailrec
import de.sciss.serial.{ImmutableSerializer, DataOutput, DataInput, Writable}
import de.sciss.serial

object Group {
  object Config {
    /** Creates a new group iterator configuration.
      *
      * @param idx            the group index
      * @param loopOverlap    the loop overlap at the end of the phrase, in seconds
      * @param windowLen      the segmentation length in seconds
      * @param windowOverlap  the segmentation overlap in seconds
      */
    def apply[S <: Sys[S]](idx: Int,
                           loopOverlap: Motion[S], windowLen: Motion[S],
                           windowOverlap: Motion[S])(implicit tx: S#Tx): Config[S] = {
      new ConfigImpl[S](idx = idx, loopOverlap = loopOverlap, windowLen = windowLen,
        windowOverlap = windowOverlap)
    }

    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Config[S]] = anySer.asInstanceOf[Ser[S]]

    private val anySer = new Ser[InMemory]

    private final val VERSION_COOKIE  = 0x1234

    private final class Ser[S <: Sys[S]] extends serial.Serializer[S#Tx, S#Acc, Config[S]] {
      def write(config: Config[S], out: DataOutput): Unit = config.write(out)

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Config[S] = {
        val version       = in.readShort()
        require (version == VERSION_COOKIE, s"Unexpected version cookie $version")
        val idx           = in.readInt()
        val loopOverlap   = Motion.read(in, access)
        val windowLen     = Motion.read(in, access)
        val windowOverlap = Motion.read(in, access)
        apply(idx = idx, loopOverlap = loopOverlap, windowLen = windowLen,
              windowOverlap = windowOverlap)
      }
    }

    private final class ConfigImpl[S <: Sys[S]](val idx: Int,
                                                val loopOverlap: Motion[S], val windowLen: Motion[S],
                                                val windowOverlap: Motion[S]) extends Config[S] {
      def data = groups(idx)

      def channels    = data.channels
      def numChannels = data.numChannels
      def material    = data.material

      def write(out: DataOutput): Unit = {
        out.writeShort(VERSION_COOKIE)
        out.writeInt(idx)
        loopOverlap.write(out)
        windowLen.write(out)
        windowOverlap.write(out)
      }
    }
  }
  sealed trait Config[S <: Sys[S]] extends Writable {
    def idx: Int
    def data: GroupData
    def loopOverlap   : Motion[S]
    def windowLen     : Motion[S]
    def windowOverlap : Motion[S]

    def channels: Vec[Int]
    def numChannels: Int
    def material: String
  }

  def apply[S <: Sys[S]](document: Document[S], group: ProcMod[S] /* ProcGroup.Modifiable[S] */, config: Config[S])
                        (implicit tx: S#Tx, cursor: Cursor[S]): Group[S] = {
    implicit val ser = ProcGroup.Modifiable.serializer[S]
    val groupH  = tx.newHandle(group)
    val configH = tx.newHandle(config)
    new Impl[S](document, groupH, configH, config.idx)
  }

  private class Impl[S <: Sys[S]](document: Document[S], groupH: stm.Source[S#Tx, ProcMod[S] /* ProcGroup.Modifiable[S] */],
                                  configH: stm.Source[S#Tx, Group.Config[S]], groupIdx: Int)
                                 (implicit cursor: Cursor[S])
    extends Group[S] {

    def iterate(): Processor[Unit, _] = {
      requireNoTxn()

      // locate the previous iteration's span

      val predSpan = cursor.step { implicit tx =>
        val group   = groupH()
        val config  = configH()
        group.nearestEventBefore(Long.MaxValue - 1).fold[Span] {
          val originF   = audioDir / "AlphavilleIlArrive.aif"
          val origin    = Util.resolveAudioFile(document, originF)
          val originV   = origin.value
          val time      = 1.0.secframes
          val spanV     = Span(0L, originV.spec.numFrames)
          val busOption = Option.empty[Expr[S, Int]]
          val (_, proc) = ProcActions.insertAudioRegion(group = group, time = time, track = 0, grapheme = origin,
            selection = spanV, bus = busOption)
          val procOut   = Util.resolveOutProc(document, group, config.channels.head)  // XXX TODO determine correct channels index
          proc ~> procOut
          val overLen   = math.min(spanV.length, config.loopOverlap.step.secframes)
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
            procOver ~> procOut
            if (fadeLen > 0) {
              val fadeIn = FadeSpec.Value(numFrames = fadeLen)
              procOver.attributes.put(ProcKeys.attrFadeIn, fadeAttr(fadeIn))
            }
            spanV.shift(time) union spanVOver.shift(timeOver)
          } else {
            spanV.shift(time)
          }

        } { stop =>
          println(s"Jo chuck $stop")
          ???
        }
      }

      // prepare bounce

      val prdLen = predSpan.length

      processor[Unit](s"Iteration<groupIdx>") { p =>
        // bounce
        val predBnc   = bounce(groupH, predSpan, Vec(0))  // XXX TODO use proper channel indices here
        val predFile  = p.await(predBnc, offset = 0f, weight = 0.1f)
        p.check()

        // create feature file
        val extrCfg           = FeatureExtraction.Config()
        extrCfg.audioInput    = predFile
        extrCfg.featureOutput = predFile.parent / s"${predFile.base}_feat.aif"
        val extrOut           = extrCfg.featureOutput replaceExt "xml"
        extrCfg.metaOutput    = Some(extrOut)
        val extr = FeatureExtraction(extrCfg)
        extr.start()
        p.await(extr, offset = 0.1f, weight = 0.2f)
        
        // create segments
        @tailrec def segmentLoop(config: Group.Config[S], res: Vec[Span] = Vec.empty)(implicit tx: S#Tx): Vec[Span] = {
          val pred  = res.lastOption.getOrElse(Span(0L, 0L))
          val start = math.max(pred.start, pred.stop - config.windowOverlap.step.secframes)
          val stop  = math.min(prdLen, start + math.max(MinLen, config.windowLen.step.secframes))
          val span0 = Span(start, stop)
          val span  = if (prdLen - span0.stop < span0.length/2) Span(start, prdLen) else span0
          val res1  = res :+ span
          if (span.stop < prdLen) segmentLoop(config, res1) else res1
        }

        val segments = cursor.step { implicit tx => segmentLoop(configH()) }
        // segments.foreach(println)

        // prepare database folder
        val matGroupH = cursor.step { implicit tx =>
          val matGroup      = Util.resolveMaterialTimeline(document, groupIdx)
          implicit val ser  = ProcGroup.Modifiable.serializer[S]
          tx.newHandle(matGroup)
        }

        val corrCfg             = FeatureCorrelation.Config()
        corrCfg.metaInput       = extrOut
        //  corrCfg.databaseFolder  =
        // val corr = FeatureCorrelation(corrCfg)
        // corr.start()

      }
    }

    private def bounce(groupH: stm.Source[S#Tx, ProcGroup[S]], span: Span, channels: Vec[Int]): Processor[File, _] = {
      requireNoTxn()
      val serverCfg   = Server.Config()
      val bounceFile  = File.createTempFile("bounce", ".aif", tmpDir)
      val bounceSpec  = AudioFileSpec(AIFF, SampleFormat.Float, numChannels = channels.size, sampleRate = sampleRate)
      ActionBounceTimeline.specToServerConfig(bounceFile, bounceSpec, serverCfg)
      val bounceGain  = Gain(0f, normalized = false)
      val bounceCfg   = ActionBounceTimeline.PerformSettings(groupH, serverCfg, bounceGain, span,
        channels.map(i => i to i))
      ActionBounceTimeline.perform(document, settings = bounceCfg)
    }
  }
}
trait Group[S <: Sys[S]] {
  def iterate(): Processor[Unit, _]
}