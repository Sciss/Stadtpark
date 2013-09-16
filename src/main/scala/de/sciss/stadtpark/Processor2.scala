package de.sciss.stadtpark

import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.Processor
import scala.concurrent.{blocking, Await}
import scala.concurrent.duration.Duration

trait Processor2[A] extends ProcessorImpl[A, Any] {
  def await[B](that: Processor[B, _], offset: Float = 0f, weight: Float = 1f): B = {
    that.addListener {
      case Processor.Progress(_, p) => progress(p * weight + offset)
    }
    blocking {
      Await.result(that, Duration.Inf)
    }
  }
}
