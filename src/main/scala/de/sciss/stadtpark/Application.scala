package de.sciss.stadtpark

import java.awt.EventQueue
import de.sciss.mellite.{Document, EphemeralDocument, ConfluentDocument, Mellite}
import de.sciss.mellite.gui.MenuBar
import de.sciss.desktop.{KeyStrokes, Menu}
import de.sciss.lucre.stm.Cursor
import de.sciss.lucre.synth.Sys
import java.awt.event.KeyEvent

object Application extends App with Runnable {
  EventQueue.invokeLater(this)

  def run(): Unit = {
    import Menu._
    import KeyStrokes._
    import KeyEvent._
    MenuBar.instance.add(
      Group("stadtpark", "Stadtpark").add(
        Item("iterate")("Iterate" -> (menu2 + VK_I)) {
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
    //    val wins  = DocumentViewHandler.instance(doc).toList
    //    val tlvOpt = wins.collectFirst {
    //      case tf: TimelineView[S] => tf
    //    }
    //    tlvOpt.foreach { tlv =>

    val g1 = cursor.step { implicit tx =>
      implicit val rand = TxnRandom() // (0L)
      val c1 = Group.Config[S](idx = 0, loopOverlap = 0.5,
        windowLen = Motion.linrand[S](0.2, 0.5), windowOverlap = Motion.linrand[S](0.05, 0.2))
      Group(doc, c1)
    }
    val proc = g1.iterate()
    proc.monitor()
  }
}
