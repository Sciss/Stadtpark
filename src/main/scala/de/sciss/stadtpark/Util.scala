package de.sciss.stadtpark

import de.sciss.file._
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.{Document, Element}
import de.sciss.synth.proc.{ExprImplicits, Grapheme, Artifact}
import de.sciss.mellite.gui.ActionArtifactLocation
import de.sciss.lucre.synth.expr.{Doubles, Longs}
import de.sciss.synth.io.AudioFile

object Util {
  def findAudioFile[S <: Sys[S]](document: Document[S], file: File)(implicit tx: S#Tx): Option[Grapheme.Elem.Audio[S]] =
    document.collectElements {
      case e: Element.AudioGrapheme[S] => e.entity
    } .headOption

  def findLocation[S <: Sys[S]](document: Document[S], dir: File)(implicit tx: S#Tx): Option[Artifact.Location[S]] =
    document.collectElements {
      case e: Element.ArtifactLocation[S] => e.entity
    } .headOption

  def resolveAudioFile[S <: Sys[S]](document: Document[S], file: File)(implicit tx: S#Tx): Grapheme.Elem.Audio[S] =
    findAudioFile(document, file).getOrElse {
      val dir = file.parent
      val loc = findLocation(document, dir).flatMap(_.modifiableOption).getOrElse {
        ActionArtifactLocation.create(dir, dir.name, document.elements).entity.modifiableOption.get
      }
      val imp     = ExprImplicits[S]
      import imp._
      val offset  = Longs  .newVar[S](0L)
      val gain    = Doubles.newVar[S](0.0)
      val spec    = AudioFile.readSpec(file)
      val artif   = loc.add(file)
      val graph   = Grapheme.Elem.Audio(artif, spec, offset, gain)
      val elem    = Element.AudioGrapheme(file.base, graph)
      document.elements.addLast(elem)
      elem.entity
    }
}