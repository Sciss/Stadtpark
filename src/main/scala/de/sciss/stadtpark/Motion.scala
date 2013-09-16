package de.sciss.stadtpark

import de.sciss.numbers
import de.sciss.lucre.stm.{Mutable, Sys}
import de.sciss.lucre.confluent.TxnRandom
import de.sciss.serial.DataOutput
import language.implicitConversions

// todo: this should actually just go into synth-expr (Motion[S] = Expr[S, Double])
object Motion {
  implicit def constant[S <: Sys[S]](value: Double): Motion[S] = Constant(value)

  def linrand[S <: Sys[S]](lo: Double, hi: Double)(implicit random: TxnRandom[S#Tx]): Motion[S] = LinRand(lo, hi)

  def exprand[S <: Sys[S]](lo: Double, hi: Double)(implicit random: TxnRandom[S#Tx]): Motion[S] = ExpRand(lo, hi)

  def sine[S <: Sys[S]](lo: Double, hi: Double, period: Int)(implicit tx: S#Tx): Motion[S] = {
    val id    = tx.newID()
    val phase = tx.newVar[Double](id, 0.0)
    new Sine(id, phase, lo, hi, period)
  }

  def walk[S <: Sys[S]](lo: Double, hi: Double, maxStep: Double)
                       (implicit random: TxnRandom[S#Tx], tx: S#Tx): Motion[S] = {
    val id      = tx.newID()
    val current = tx.newVar[Double](id, Double.NaN)
    new Walk(id, current, lo, hi, maxStep)
  }

  def linlin[S <: Sys[S]](in: Motion[S], inLo: Double, inHi: Double, outLo: Double, outHi: Double): Motion[S] =
    LinLin(in, inLo, inHi, outLo, outHi)

  def linexp[S <: Sys[S]](in: Motion[S], inLo: Double, inHi: Double, outLo: Double, outHi: Double): Motion[S] =
    LinExp(in, inLo, inHi, outLo, outHi)

  def coin[S <: Sys[S]](prob: Double, a: Motion[S], b: Motion[S])
                       (implicit random: TxnRandom[S#Tx]): Motion[S] = Coin(prob, a, b)

  private final case class Constant[S <: Sys[S]](value: Double) extends Motion[S] {
    def step(implicit tx: S#Tx): Double = value
  }

  private final case class LinRand[S <: Sys[S]](lo: Double, hi: Double)(implicit random: TxnRandom[S#Tx])
    extends Motion[S] {

    val range = hi - lo

    def step(implicit tx: S#Tx): Double = random.nextDouble() * range + lo
  }

  private final case class ExpRand[S <: Sys[S]](lo: Double, hi: Double)(implicit random: TxnRandom[S#Tx])
    extends Motion[S] {

    val factor = math.log(hi / lo)

    def step(implicit tx: S#Tx): Double = math.exp(random.nextDouble() * factor) * lo
  }

  private final class Walk[S <: Sys[S]](val id: S#ID, current: S#Var[Double], lo: Double, hi: Double, maxStep: Double)
                                       (implicit random: TxnRandom[S#Tx])
    extends Motion[S] with Mutable.Impl[S] {

    val maxStep2  = maxStep * 2
    // val current   = Ref(Double.NaN)

    def step(implicit tx: S#Tx): Double = {
      val c = current()
      val r = random.nextDouble()
      val v = if (c.isNaN) {
        r * (hi - lo) + lo
      } else {
        math.max(lo, math.min(hi, c + (r * maxStep2 - maxStep)))
      }
      current() = v
      v
    }

    protected def disposeData()(implicit tx: S#Tx) = ()

    protected def writeData(out: DataOutput) = ()
  }

  private final class Sine[S <: Sys[S]](val id: S#ID, phase: S#Var[Double], lo: Double, hi: Double, period: Int)
    extends Motion[S] with Mutable.Impl[S] {

    // val phase   = Ref(0)
    val mul     = (hi - lo) / 2
    val add     = mul + lo
    val factor  = math.Pi * 2 / period

    def step(implicit tx: S#Tx): Double = {
      val p   = phase()
      phase() = (p + 1) % period
      math.sin(p * factor) * mul + add
    }

    protected def disposeData()(implicit tx: S#Tx) = ()

    protected def writeData(out: DataOutput) = ()
  }

  private final case class Coin[S <: Sys[S]](prob: Double, a: Motion[S], b: Motion[S])
                                            (implicit random: TxnRandom[S#Tx])
    extends Motion[S] {

    def step(implicit tx: S#Tx): Double = {
      val m = if (random.nextDouble() >= prob) a else b
      m.step
    }
  }

  private final case class LinLin[S <: Sys[S]](in: Motion[S], inLo: Double, inHi: Double,
                                               outLo: Double, outHi: Double)
    extends Motion[S] {

    def step(implicit tx: S#Tx): Double = {
      import numbers.Implicits._
      in.step.linlin(inLo, inHi, outLo, outHi)
    }
  }

  private final case class LinExp[S <: Sys[S]](in: Motion[S], inLo: Double, inHi: Double,
                                               outLo: Double, outHi: Double)
    extends Motion[S] {

    def step(implicit tx: S#Tx): Double = {
      import numbers.Implicits._
      in.step.linexp(inLo, inHi, outLo, outHi)
    }
  }
}
trait Motion[S <: Sys[S]] {
  def step(implicit tx: S#Tx): Double
}