package de.sciss.stadtpark

import de.sciss.file._
import de.sciss.lucre.synth.Sys
import de.sciss.mellite._
import de.sciss.synth.proc._
import de.sciss.mellite.gui.ActionArtifactLocation
import de.sciss.lucre.synth.expr._
import de.sciss.synth.io.AudioFile
import de.sciss.lucre.expr.Expr
import de.sciss.synth.SynthGraph
import de.sciss.span.Span
import de.sciss.synth.proc.graph.scan
import de.sciss.synth.proc.ExprImplicits
import scala.Some

object Util {
  def findLocation[S <: Sys[S]](document: Document[S], dir: File)(implicit tx: S#Tx): Option[Artifact.Location[S]] =
    document.collectElements {
      case e: Element.ArtifactLocation[S] => e.entity
    } .headOption

  def resolveLocation[S <: Sys[S]](document: Document[S], dir: File)(implicit tx: S#Tx): Artifact.Location[S] =
    findLocation(document, dir).getOrElse {
      val folder = resolveFolder(document, "Locations")
      ActionArtifactLocation.create(dir, dir.name, folder).entity
    }

  def findAudioFile[S <: Sys[S]](document: Document[S], file: File)(implicit tx: S#Tx): Option[Grapheme.Elem.Audio[S]] =
    document.collectElements {
      case e: Element.AudioGrapheme[S] => e.entity
    } .headOption

  def resolveAudioFile[S <: Sys[S]](document: Document[S], file: File)(implicit tx: S#Tx): Grapheme.Elem.Audio[S] =
    findAudioFile(document, file).getOrElse {
      val dir     = file.parent
      val loc     = resolveLocation(document, dir).modifiableOption.get
      val imp     = ExprImplicits[S]
      import imp._
      val offset  = Longs  .newVar[S](0L)
      val gain    = Doubles.newVar[S](0.0)
      val spec    = AudioFile.readSpec(file)
      val artif   = loc.add(file)
      val graph   = Grapheme.Elem.Audio(artif, spec, offset, gain)
      val elem    = Element.AudioGrapheme(file.base, graph)
      val folder  = resolveFolder(document, "Audio Files")
      folder.addLast(elem)
      elem.entity
    }

  def findFolder[S <: Sys[S]](document: Document[S], name: String)(implicit tx: S#Tx): Option[Folder[S]] =
    document.collectElements {
      case e: Element.Folder[S] if e.name.value == name => e.entity
    } .headOption

  def resolveFolder[S <: Sys[S]](document: Document[S], name: String)(implicit tx: S#Tx): Folder[S] =
    findFolder(document, name).getOrElse {
      val elem  = Element.Folder(name, Folder[S])
      document.elements.addLast(elem)
      elem.entity
    }

  private def busName(idx: Int) = s"Bus-${idx+1}"

  def findBus[S <: Sys[S]](document: Document[S], idx: Int)(implicit tx: S#Tx): Option[Expr[S, Int]] = {
    val name = busName(idx)
    document.collectElements {
      case e: Element.Int[S] if e.name.value == name => e.entity
    } .headOption
  }

  def resolveBus[S <: Sys[S]](document: Document[S], idx: Int)(implicit tx: S#Tx): Expr[S, Int] =
    findBus(document, idx).getOrElse {
      val init    = Ints.newVar[S](Ints.newConst(idx))
      val elem    = Element.Int(busName(idx), init)
      val folder  = resolveFolder(document, "Buses")
      folder.addLast(elem)
      elem.entity
    }

  //  private val outGraphName = "Mono-Out"
  //
  //  def findOutGraph[S <: Sys[S]](document: Document[S])(implicit tx: S#Tx): Option[SynthGraph] =
  //    document.collectElements {
  //      case e: Element.Code[S] if e.name.value == outGraphName => e.entity.value
  //    } .collectFirst {
  //      case sg: Code.SynthGraph => sg.execute()
  //    }
  //
  //  def resolveOutGraph[S <: Sys[S]](document: Document[S])(implicit tx: S#Tx): SynthGraph = ...

  private def outProcName(idx: Int) = s"Out-${idx+1}"

  def findOutProc[S <: Sys[S]](document: Document[S], group: ProcGroup[S], idx: Int)
                              (implicit tx: S#Tx): Option[Proc[S]] = {
    val name = outProcName(idx)
    group.intersect(0L).collect {
      case (Span.All, timeds) => timeds.flatMap { timed =>
        val proc = timed.value
        proc.attributes[Attribute.String](ProcKeys.attrName).flatMap { nameAttr =>
          if (nameAttr.value == name) Some(proc) else None
        }
      }
    } .toIndexedSeq.flatten.headOption
  }

  def resolveOutProc[S <: Sys[S]](document: Document[S], group: ProcMod[S], idx: Int)
                                 (implicit tx: S#Tx): Proc[S] =
    findOutProc(document, group, idx).getOrElse {
      val p     = Proc[S]
      val name  = outProcName(idx)
      p.attributes.put(ProcKeys.attrName, Attribute.String(Strings.newVar(Strings.newConst(name))))
      val bus   = resolveBus(document, idx)
      p.attributes.put(ProcKeys.attrBus, Attribute.Int(bus))
      val gr   = SynthGraph {
        import de.sciss.synth._
        import ugen._
        val in    = scan.InFix(ProcKeys.scanMainIn, 1)
        val mute  = graph.attribute(ProcKeys.attrMute).ir(0)
        val gain  = graph.attribute(ProcKeys.attrGain).ir(1)
        val bus   = graph.attribute(ProcKeys.attrBus ).ir(0)
        val amp   = gain * (1 - mute)
        val sig   = in * amp
        Out.ar(bus, sig)
      }
      p.graph()   = SynthGraphs.newConst[S](gr)
      val spanEx  = SpanLikes.newVar[S](SpanLikes.newConst(Span.All))
      group.add(spanEx, p)
      p
    }
}