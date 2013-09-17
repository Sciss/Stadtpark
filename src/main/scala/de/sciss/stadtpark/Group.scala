package de.sciss.stadtpark

import de.sciss.synth.proc._
import de.sciss.lucre.stm
import de.sciss.mellite.{ProcActions, Gain, Document}
import de.sciss.file._
import de.sciss.lucre.synth.{InMemory, Server, Sys}
import de.sciss.span.Span
import de.sciss.mellite.gui.ActionBounceTimeline
import de.sciss.lucre.stm.Cursor
import de.sciss.processor.Processor
import java.io.File
import de.sciss.synth.io.{SampleFormat, AudioFileSpec}
import de.sciss.synth.io.AudioFileType.AIFF
import de.sciss.strugatzki.{FeatureCorrelation, FeatureExtraction}
import scala.annotation.tailrec
import de.sciss.serial.{DataOutput, DataInput, Writable}
import de.sciss.{numbers, serial}

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

  def apply[S <: Sys[S]](document: Document[S], config: Config[S])(implicit tx: S#Tx, cursor: Cursor[S]): Group[S] = {
    val configH = tx.newHandle(config)
    new Impl[S](document, configH, config.idx)
  }

  private class Impl[S <: Sys[S]](document: Document[S],
                                  configH: stm.Source[S#Tx, Group.Config[S]], groupIdx: Int)
                                 (implicit cursor: Cursor[S])
    extends Group[S] {

    private def findLastStop(group: ProcGroup[S])(implicit tx: S#Tx): Long = {
      // THIS IS BROKEN:
      //      val max       = (Long.MaxValue >> 4) - 1
      //      group.nearestEventBefore(max).getOrElse(max)
      //      println(s"Yo chuck, found $max")

      val sq = group.iterator.map(_._1).collect {
        case sp: Span => sp.stop
      } .toList

      if (sq.isEmpty) sys.error("Timeline is empty")
      sq.max
    }

    private def findLastContiguousSpan(group: ProcGroup[S])(implicit tx: S#Tx): Span = {
      val stop      = findLastStop(group)
      val foo       = group.intersect(Span(stop - MinLen, stop)).toList
      val lastSpans: List[Span] = foo.collect {
        case (sp2: Span, _) => sp2
      }
      if (lastSpans.isEmpty) sys.error(s"Timeline is empty $foo")
      val lastSpan = lastSpans.reduce(_ union _)

      @tailrec def loop(sp: Span): Span = {
        val sq = group.intersect(Span(sp.start - MinLen, sp.start)).collect {
          case (sp2 @ Span(_, _), _) => sp2
        } .toList

        if (sq.isEmpty) sp else {
          val squ = sq.reduce(_ union _)
          val sp2 = sp union squ
          assert (sp2 != sp)
          loop(sp2)
        }
      }

      loop(lastSpan)
    }

    def iterate(): Processor[Unit, _] = {
      requireNoTxn()

      // locate the previous iteration's span

      val (iterGroupH, predSpan) = cursor.step { implicit tx =>
        val group   = Util.resolveIterTimeline(document, groupIdx)
        implicit val ser = ProcGroup.Modifiable.serializer[S]
        val groupH  = tx.newHandle(group)
        val span    = findLastContiguousSpan(group)
        groupH -> span
      }

      // prepare bounce

      val prdLen = predSpan.length

      processor[Unit](s"Iteration<groupIdx>") { p =>
        // bounce
        val predBnc   = bounce(iterGroupH, predSpan, Vec(0))  // XXX TODO use proper channel indices here
        val predFile  = p.await(predBnc, offset = 0f, weight = 0.05f)
        p.check()

        // create template feature file
        val predExCfg           = FeatureExtraction.Config()
        predExCfg.audioInput    = predFile
        predExCfg.featureOutput = predFile.parent / s"${predFile.base}_feat.aif"
        val predExOut           = predExCfg.featureOutput replaceExt "xml"
        predExCfg.metaOutput    = Some(predExOut)
        val predEx = FeatureExtraction(predExCfg)
        predEx.start()
        p.await(predEx, offset = 0.05f, weight = 0.05f)
        
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
        val (matGroupH, matSpan) = cursor.step { implicit tx =>
          val matGroup      = Util.resolveMaterialTimeline(document, groupIdx)
          implicit val ser  = ProcGroup.Modifiable.serializer[S]
          val stop0         = findLastContiguousSpan(matGroup).stop
          val stop          = math.min(maxDatabaseLen.secframes, stop0)
          tx.newHandle(matGroup) -> Span(0L, stop)
        }

        val matBnc  = bounce(matGroupH, matSpan, Vec(0))
        val matFile = p.await(matBnc, offset = 0.1f, weight = 0.05f)

        // create database feature file
        val matExCfg           = FeatureExtraction.Config()
        matExCfg.audioInput    = matFile
        val dbDir              = File.createTempFile("database", ".db", tmpDir)
        dbDir.delete()
        dbDir.mkdir()
        matExCfg.featureOutput = dbDir / s"${matFile.base}_feat.aif"
        val matExOut           = matExCfg.featureOutput replaceExt "xml"
        matExCfg.metaOutput    = Some(matExOut)
        val matEx = FeatureExtraction(matExCfg)
        matEx.start()
        p.await(matEx, offset = 0.15f, weight = 0.05f)

        import numbers.Implicits._

        // find matches
        val numSegm     = segments.size
        val matchWeight = 0.4f / numSegm
        // println(s"predSpan $predSpan, numSegm $numSegm")
        val matches     = segments.zipWithIndex.map { case (segm, segmIdx) =>
          val corrCfg             = FeatureCorrelation.Config()
          corrCfg.metaInput       = predExOut
          corrCfg.databaseFolder  = dbDir
          corrCfg.normalize       = false // XXX TODO: perhaps enable (need to copy norm feat file)
          corrCfg.punchIn         = FeatureCorrelation.Punch(segm, temporalWeight = 0.5f) // XXX TODO: make weight configurable
          corrCfg.maxBoost        = 20.dbamp
          corrCfg.minPunch        = corrCfg.punchIn.span.length
          corrCfg.maxPunch        = corrCfg.minPunch
          corrCfg.numMatches      = 1
          corrCfg.numPerFile      = corrCfg.numMatches

          val corr = FeatureCorrelation(corrCfg)
          corr.start()
          p.await(corr, offset = 0.2f + segmIdx * matchWeight, weight = matchWeight).apply(0)
        }

        // println()
        // matches.foreach(println)

        // place matches
        val placeOff = predSpan.stop + 1.0.secframes
        cursor.step { implicit tx =>
          val group   = iterGroupH()
          val config  = configH()
          val procOut = Util.resolveOutProc(document, group, config.channels.head)  // XXX TODO: channel
          (segments zip matches).zipWithIndex.foreach { case ((segm, m), idx) =>
            // Match(sim: Float, file: File, punch: Span, boostIn: Float, boostOut: Float)
            val gain  = (m.boostIn * m.boostOut).sqrt
            val time  = segm.start + placeOff
            val audio = Util.resolveAudioFile(document, m.file)
            val span  = m.punch
            val (_, proc) = ProcActions.insertAudioRegion(group, time = time, track = idx % 2, grapheme = audio,
              selection = span, bus = None)
            if (gain != 1) ProcActions.adjustGain(proc, gain)
            // XXX TODO: fade in and out
            proc ~> procOut
          }
        }
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