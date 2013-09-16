package de.sciss.stadtpark

import java.awt.EventQueue
import de.sciss.mellite.{Document, EphemeralDocument, ConfluentDocument, Mellite}
import de.sciss.mellite.gui.{TimelineView, DocumentViewHandler, MenuBar}
import de.sciss.desktop.Menu
import de.sciss.lucre.stm.Cursor
import de.sciss.lucre.synth.Sys

object Application extends App with Runnable {
  //  type S  = Confluent
  //

  EventQueue.invokeLater(this)

  def run(): Unit = {
    import Menu._
    MenuBar.instance.add(
      Group("stadtpark", "Stadtpark").add(
        Item("iterate")("Iterate") {
          iterate()
        }
      )
    )
    Mellite.main(args)
  }

  def iterate(): Unit =
    Mellite.documentHandler.activeDocument match {
      case Some(cd: ConfluentDocument) => iterate1(cd)(cd.cursors.cursor)
      case Some(ed: EphemeralDocument) => iterate1(ed)(ed.cursor)
      case Some(other) => println(s"Unsupported document type $other")
      case _ =>
    }

  private def iterate1[S <: Sys[S]](doc: Document[S])(implicit cursor: Cursor[S]): Unit = {
    val wins  = DocumentViewHandler.instance(doc).toList
    val tlvOpt = wins.collectFirst {
      case tf: TimelineView[S] => tf
    }
    tlvOpt.foreach { tlv =>
      val g1Opt = cursor.step { implicit tx =>
        tlv.group.entity.modifiableOption.map { group =>
          val c1  = Group.Config[S](Vec(3, 4), "Raspad441.aif", loopOverlap = 0.5)
          Group(doc, group, c1)
        }
      }
      g1Opt.foreach { g1 =>
        val proc = g1.iterate()
        proc.monitor()
      }
    }
  }
}
