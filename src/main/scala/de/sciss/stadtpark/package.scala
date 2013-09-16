package de.sciss

import de.sciss.file._
import de.sciss.synth.proc.{Attribute, FadeSpec}
import de.sciss.lucre.synth.Sys
import scala.concurrent.stm.Txn
import scala.concurrent.ExecutionContext
import de.sciss.processor.Processor
import scala.util.{Success, Failure}
import de.sciss.processor.impl.ProcessorImpl

package object stadtpark {
  val baseDir     = userHome / "Desktop" / "Forum"
  val tmpDir      = baseDir / "tmp"
  val materialDir = baseDir / "material"
  val audioDir    = baseDir / "audio_work"

  type Vec[+A]  = collection.immutable.IndexedSeq[A]
  val  Vec      = collection.immutable.IndexedSeq

  val sampleRate  = 44100.0
  val MinLen      = 32

  // type Tx       = concurrent.stm.InTxn

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
          while (p < lastProg) {
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
}