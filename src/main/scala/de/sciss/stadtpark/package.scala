package de.sciss

import de.sciss.file._

package object stadtpark {
  val baseDir     = userHome / "Desktop" / "Forum"
  val tmpDir      = baseDir / "tmp"
  val materialDir = baseDir / "material"
  val audioDir    = baseDir / "audio_work"

  type Vec[+A]  = collection.immutable.IndexedSeq[A]
  val  Vec      = collection.immutable.IndexedSeq

  // type Tx       = concurrent.stm.InTxn
}
