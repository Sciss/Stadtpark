package de.sciss

import de.sciss.file._
import de.sciss.synth.proc._
import de.sciss.lucre.synth.Sys
import scala.concurrent.stm.Txn
import scala.concurrent.ExecutionContext
import de.sciss.processor.Processor
import scala.util.{Success, Failure}
import de.sciss.lucre.bitemp.BiGroup
import scala.util.Success
import scala.util.Failure

package object stadtpark {
  // bug in Scala 2.10 - "bad symbolic reference. A signature in Group.class refers to type Modifiable"
  type ProcMod[S <: Sys[S]] = BiGroup.Modifiable[S, Proc[S], Proc.Update[S]]

  val baseDir     = userHome / "Desktop" / "Forum"
  val tmpDir      = baseDir / "tmp"
  val materialDir = baseDir / "material"
  val audioDir    = baseDir / "audio_work"
  val originFile  = audioDir / "AlphavilleIlArrive.aif"

  type Vec[+A]  = collection.immutable.IndexedSeq[A]
  val  Vec      = collection.immutable.IndexedSeq

  val sampleRate  = 44100.0
  val MinLen      = 32

  val maxDatabaseLen  = 20 * 60.0 // 20 minutes

  // type Tx       = concurrent.stm.InTxn

  val groups      = Vec(
    GroupData(idx = 0, channels = Vec(0), material = "Raspad441.aif")
  )

  def fadeAttr[S <: Sys[S]](init: FadeSpec.Value)(implicit tx: S#Tx): Attribute.FadeSpec[S] =
    Attribute.FadeSpec(FadeSpec.Elem.newVar(FadeSpec.Elem.newConst(init)))

  def requireNoTxn(): Unit = require(Txn.findCurrent.isEmpty, "Must not be called inside transaction")

  implicit val executionContext = ExecutionContext.global

  implicit class RichProcessor(proc: Processor[Any, _]) {
    def monitor(): Unit = {
      var lastProg = 0
      proc.addListener {
        case prog @ Processor.Progress(_, _) =>
          val p = prog.toInt/3
          while (lastProg < p) {
            print('#')
            lastProg += 1
          }
      }

      proc.onComplete {
        case Failure(Processor.Aborted()) =>
          println(s" Aborted $proc")

        case Failure(err) =>
          println(s" Failure: $proc")
          err.printStackTrace()

        case Success(_) =>
          println(s" Success: $proc")
      }
    }
  }

  def processor[A](name: => String)(fun: Processor2[A] => A): Processor[A, _] = {
    val p = new Processor2[A] {
      protected def body(): A = fun(this)
      override def toString = name
    }
    p.start()
    p
  }

  implicit class RichProc[S <: Sys[S]](proc: Proc[S]) {
    def ~>(that: Proc[S])(implicit tx: S#Tx): Unit = {
      val source  = proc.scans.get(ProcKeys.scanMainOut).getOrElse(proc.scans.add(ProcKeys.scanMainOut))
      val sink    = that.scans.get(ProcKeys.scanMainIn ).getOrElse(that.scans.add(ProcKeys.scanMainIn ))
      if (!source.sinks.toList.contains(Scan.Link.Scan(sink))) {
        source.addSink(sink)
      }
    }
  }

  implicit class RichDouble(d: Double) {
    /** Translates the input number from seconds to sample frames. */
    def secframes: Long = (d * sampleRate).toLong
  }
}