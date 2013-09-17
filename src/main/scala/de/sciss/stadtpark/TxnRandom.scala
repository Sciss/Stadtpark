package de.sciss.stadtpark

import concurrent.stm.{Ref, InTxn}
import java.util.concurrent.atomic.AtomicLong
import de.sciss.lucre.stm
import de.sciss.serial.{DataInput, DataOutput}

/**
 * Like java's random, but within a transactional cell
 */
object TxnRandom {
  private val multiplier  = 0x5DEECE66DL
  private val mask        = (1L << 48) - 1
  private val addend      = 11L

  /**
   * Scrambles a seed value for initializing the underlying long variable.
   * Callers who use the `wrap` method may use this to initially fill the
   * wrapped variabled based on a given seed.
   */
  def initialScramble(seed: Long): Long = (seed ^ multiplier) & mask

  private def calcSeedUniquifier(): Long = {
    while (true) {
      val current = seedUniquifier.get()
      val next = current * 181783497276652981L
      if (seedUniquifier.compareAndSet(current, next)) return next
    }
    sys.error("Never here")
  }

  private val seedUniquifier = new AtomicLong(8682522807148012L)

  def plain():           TxnRandom[InTxn] = plain(calcSeedUniquifier() ^ System.nanoTime())
  def plain(seed: Long): TxnRandom[InTxn] = new PlainImpl(Ref(initialScramble(seed)))

  def apply[S <: stm.Sys[S]]()(implicit tx: S#Tx): Writable[S] =
    apply(calcSeedUniquifier() ^ System.nanoTime())

  def apply[S <: stm.Sys[S]](seed: Long)(implicit tx: S#Tx): Writable[S] = {
    val id      = tx.newID()
    val seedRef = tx.newLongVar(id, initialScramble(seed))
    new SysImpl[S](id, seedRef)
  }

  // def wrap[Txn](peer: stm.Var[Txn, Long]): TxnRandom[Txn] = new SysImpl[Txn](peer)

  def read[S <: stm.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Writable[S] = {
    val id      = tx.readID(in, access)
    val seedRef = tx.readLongVar(id, in)
    new SysImpl[S](id, seedRef)
  }

  private sealed trait Impl[Txn] extends TxnRandom[Txn] {
    protected def refSet(seed: Long)(implicit tx: Txn): Unit
    protected def refGet(implicit tx: Txn): Long

    def nextBoolean()(implicit tx: Txn): Boolean = next(1) != 0

    def nextDouble()(implicit tx: Txn): Double =
      ((next(26).toLong << 27) + next(27)) / (1L << 53).toDouble

    def nextFloat()(implicit tx: Txn): Float = next(24) / (1 << 24).toFloat

    def nextInt()(implicit tx: Txn): Int = next(32)

    def nextInt(n: Int)(implicit tx: Txn): Int = {
      require(n > 0, "n must be positive")

      if ((n & -n) == n) {
        // n is a power of 2
        return ((n * next(31).toLong) >> 31).toInt
      }

      do {
        val bits = next(31)
        val res = bits % n
        if (bits - res + n >= 1) return res
      } while (true)

      sys.error("Never here")
    }

    def nextLong()(implicit tx: Txn): Long = (next(32).toLong << 32) + next(32)

    def setSeed(seed: Long)(implicit tx: Txn): Unit = refSet(initialScramble(seed))

    private def next(bits: Int)(implicit tx: Txn): Int = {
      val oldSeed = refGet
      val nextSeed = (oldSeed * multiplier + addend) & mask
      refSet(nextSeed)
      (nextSeed >>> (48 - bits)).toInt
    }
  }

  private final class PlainImpl(seedRef: Ref[Long]) extends Impl[InTxn] {
    protected def refSet(value: Long)(implicit tx: InTxn): Unit = seedRef() = value

    protected def refGet(implicit tx: InTxn): Long = seedRef()
  }

  private final class SysImpl[S <: stm.Sys[S]](val id: S#ID, seedRef: S#Var[Long]) extends Impl[S#Tx]
    with Writable[S] with stm.Mutable.Impl[S] {

    protected def refSet(value: Long)(implicit tx: S#Tx): Unit = seedRef() = value

    protected def refGet(implicit tx: S#Tx): Long = seedRef()

    protected def disposeData()(implicit tx: S#Tx): Unit = seedRef.dispose()

    protected def writeData(out: DataOutput): Unit = seedRef.write(out)
  }

  sealed trait Writable[S <: stm.Sys[S]] extends TxnRandom[S#Tx] with stm.Mutable[S#ID, S#Tx]
}

trait TxnRandom[-Txn] {
  def nextBoolean()(implicit tx: Txn): Boolean
  def nextDouble() (implicit tx: Txn): Double
  def nextFloat()  (implicit tx: Txn): Float
  def nextInt()    (implicit tx: Txn): Int
  def nextInt(n: Int)(implicit tx: Txn): Int
  def nextLong()   (implicit tx: Txn): Long

  def setSeed(seed: Long)(implicit tx: Txn): Unit
}