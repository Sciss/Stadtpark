package de.sciss.stadtpark

import de.sciss.numbers
import de.sciss.lucre.stm.{Identifiable, Mutable, Sys}
import de.sciss.serial.{Writable, DataInput, DataOutput}
import language.implicitConversions
import scala.annotation.switch

// todo: this should actually just go into synth-expr (Motion[S] = Expr[S, Double])
object Motion {
  implicit def constant[S <: Sys[S]](value: Double): Motion[S] = Constant(value)

  def linrand[S <: Sys[S]](lo: Double, hi: Double)(implicit random: TxnRandom.Writable[S]): Motion[S] =
    LinRand(lo, hi, random)

  def exprand[S <: Sys[S]](lo: Double, hi: Double)(implicit random: TxnRandom.Writable[S]): Motion[S] =
    ExpRand(lo, hi, random)

  def sine[S <: Sys[S]](lo: Double, hi: Double, period: Int)(implicit tx: S#Tx): Motion[S] = {
    val id    = tx.newID()
    val phase = tx.newVar[Double](id, 0.0)
    new Sine(id, phase, lo, hi, period)
  }

  def walk[S <: Sys[S]](lo: Double, hi: Double, maxStep: Double)
                       (implicit random: TxnRandom.Writable[S], tx: S#Tx): Motion[S] = {
    val id      = tx.newID()
    val current = tx.newVar[Double](id, Double.NaN)
    new Walk(id, current, lo, hi, maxStep, random)
  }

  def linlin[S <: Sys[S]](in: Motion[S], inLo: Double, inHi: Double, outLo: Double, outHi: Double): Motion[S] =
    LinLin(in, inLo, inHi, outLo, outHi)

  def linexp[S <: Sys[S]](in: Motion[S], inLo: Double, inHi: Double, outLo: Double, outHi: Double): Motion[S] =
    LinExp(in, inLo, inHi, outLo, outHi)

  def coin[S <: Sys[S]](prob: Double, a: Motion[S], b: Motion[S])
                       (implicit random: TxnRandom.Writable[S]): Motion[S] = Coin(prob, a, b, random)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Motion[S] = {
    val cookie = in.readShort()
    (cookie: @switch) match {
      case Constant.cookie => Constant.readIdentified(in)
      case LinRand .cookie => LinRand .readIdentified(in, access)
      case ExpRand .cookie => ExpRand .readIdentified(in, access)
      case Walk    .cookie => Walk    .readIdentified(in, access)
      case Sine    .cookie => Sine    .readIdentified(in, access)
      case Coin    .cookie => Coin    .readIdentified(in, access)
      case LinLin  .cookie => LinLin  .readIdentified(in, access)
      case LinExp  .cookie => LinExp  .readIdentified(in, access)
    }
  }

  private sealed trait Impl[S <: Sys[S]] extends Motion[S] {
    def cookie: Int

    def write(out: DataOutput): Unit = {
      out.writeShort(cookie)
      writeData(out)
    }

    protected def writeData(out: DataOutput): Unit
  }

  private sealed trait MutableImpl[S <: Sys[S]] extends Impl[S] with Identifiable[S#ID] {
    override def equals(that: Any): Boolean =
       that match {
         case value: MutableImpl[_] => id == value.id
         case _ => super.equals(that)
       }

     override def hashCode = id.hashCode()
  }

  private object Constant {
    final val cookie = 0
    def readIdentified[S <: Sys[S]](in: DataInput): Constant[S] = Constant(in.readDouble())
  }
  private final case class Constant[S <: Sys[S]](value: Double) extends Impl[S] {
    def step(implicit tx: S#Tx): Double = value

    def cookie = Constant.cookie

    protected def writeData(out: DataOutput): Unit = out.writeDouble(value)
  }

  private object LinRand {
    final val cookie = 1
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): LinRand[S] = {
      val lo      = in.readDouble()
      val hi      = in.readDouble()
      val random  = TxnRandom.read[S](in, access)
      LinRand[S](lo, hi, random)
    }
  }
  private final case class LinRand[S <: Sys[S]](lo: Double, hi: Double, random: TxnRandom.Writable[S])
    extends Impl[S] {

    val range = hi - lo

    def step(implicit tx: S#Tx): Double = random.nextDouble() * range + lo

    def cookie = LinRand.cookie

    protected def writeData(out: DataOutput): Unit = {
      out.writeDouble(lo)
      out.writeDouble(hi)
      random.write(out)
    }
  }

  private object ExpRand {
    final val cookie = 2
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ExpRand[S] = {
      val lo      = in.readDouble()
      val hi      = in.readDouble()
      val random  = TxnRandom.read(in, access)
      ExpRand[S](lo, hi, random)
    }
  }
  private final case class ExpRand[S <: Sys[S]](lo: Double, hi: Double, random: TxnRandom.Writable[S])
    extends Impl[S] {

    val factor = math.log(hi / lo)

    def step(implicit tx: S#Tx): Double = math.exp(random.nextDouble() * factor) * lo

    def cookie = ExpRand.cookie

    protected def writeData(out: DataOutput): Unit = {
      out.writeDouble(lo)
      out.writeDouble(hi)
      random.write(out)
    }
  }

  private object Walk {
    final val cookie = 3
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Walk[S] = {
      val id      = tx.readID(in, access)
      val current = tx.readVar[Double](id, in)
      val lo      = in.readDouble()
      val hi      = in.readDouble()
      val maxStep = in.readDouble()
      val random  = TxnRandom.read(in, access)
      new Walk[S](id, current, lo, hi, maxStep, random)
    }
  }

  private final class Walk[S <: Sys[S]](val id: S#ID, current: S#Var[Double], lo: Double, hi: Double, maxStep: Double,
                                        random: TxnRandom.Writable[S])
    extends MutableImpl[S] {

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

    def cookie = Walk.cookie

    protected def writeData(out: DataOutput): Unit = {
      current.write(out)
      out.writeDouble(lo)
      out.writeDouble(hi)
      out.writeDouble(maxStep)
      random.write(out)
    }
  }

  private object Sine {
    final val cookie = 4
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Sine[S] = {
      val id      = tx.readID(in, access)
      val phase   = tx.readVar[Double](id, in)
      val lo      = in.readDouble()
      val hi      = in.readDouble()
      val period  = in.readInt()
      new Sine[S](id, phase, lo, hi, period)
    }
  }

  private final class Sine[S <: Sys[S]](val id: S#ID, phase: S#Var[Double], lo: Double, hi: Double, period: Int)
    extends MutableImpl[S] {

    // val phase   = Ref(0)
    val mul     = (hi - lo) / 2
    val add     = mul + lo
    val factor  = math.Pi * 2 / period

    def step(implicit tx: S#Tx): Double = {
      val p   = phase()
      phase() = (p + 1) % period
      math.sin(p * factor) * mul + add
    }

    def cookie = Sine.cookie

    protected def writeData(out: DataOutput): Unit = {
      phase.write(out)
      out.writeDouble(lo)
      out.writeDouble(hi)
      out.writeInt(period)
    }
  }

  private object Coin {
    final val cookie = 5
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Coin[S] = {
      val prob    = in.readDouble()
      val a       = Motion.read(in, access)
      val b       = Motion.read(in, access)
      val random  = TxnRandom.read(in, access)
      new Coin[S](prob, a, b, random)
    }
  }

  private final case class Coin[S <: Sys[S]](prob: Double, a: Motion[S], b: Motion[S], random: TxnRandom.Writable[S])
    extends Impl[S] {

    def step(implicit tx: S#Tx): Double = {
      val m = if (random.nextDouble() >= prob) a else b
      m.step
    }

    def cookie = Coin.cookie

    protected def writeData(out: DataOutput): Unit = {
      out.writeDouble(prob)
      a.write(out)
      b.write(out)
      random.write(out)
    }
  }

  private object LinLin {
    final val cookie = 6
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): LinLin[S] = {
      val inM     = Motion.read(in, access)
      val inLo    = in.readDouble()
      val inHi    = in.readDouble()
      val outLo   = in.readDouble()
      val outHi   = in.readDouble()
      LinLin[S](inM, inLo, inHi, outLo, outHi)
    }
  }

  private final case class LinLin[S <: Sys[S]](in: Motion[S], inLo: Double, inHi: Double,
                                               outLo: Double, outHi: Double)
    extends Impl[S] {

    def step(implicit tx: S#Tx): Double = {
      import numbers.Implicits._
      in.step.linlin(inLo, inHi, outLo, outHi)
    }

    def cookie = LinLin.cookie

    protected def writeData(out: DataOutput): Unit = {
      in.write(out)
      out.writeDouble(inLo)
      out.writeDouble(inHi)
      out.writeDouble(outLo)
      out.writeDouble(outHi)
    }
  }

  private object LinExp {
    final val cookie = 7
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): LinExp[S] = {
      val inM     = Motion.read(in, access)
      val inLo    = in.readDouble()
      val inHi    = in.readDouble()
      val outLo   = in.readDouble()
      val outHi   = in.readDouble()
      LinExp[S](inM, inLo, inHi, outLo, outHi)
    }
  }

  private final case class LinExp[S <: Sys[S]](in: Motion[S], inLo: Double, inHi: Double,
                                               outLo: Double, outHi: Double)
    extends Impl[S] {

    def step(implicit tx: S#Tx): Double = {
      import numbers.Implicits._
      in.step.linexp(inLo, inHi, outLo, outHi)
    }

    def cookie = LinExp.cookie

    protected def writeData(out: DataOutput): Unit = {
      in.write(out)
      out.writeDouble(inLo)
      out.writeDouble(inHi)
      out.writeDouble(outLo)
      out.writeDouble(outHi)
    }
  }
}
trait Motion[S <: Sys[S]] extends Writable {
  def step(implicit tx: S#Tx): Double
}