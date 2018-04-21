// See LICENSE for license details.

package chisel3.core

import scala.language.experimental.macros
import chisel3.internal.Builder.pushOp
import chisel3.internal.firrtl.PrimOp._
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo.{DeprecatedSourceInfo, SourceInfo, SourceInfoTransform, SourceInfoWhiteboxTransform}
import chisel3.internal.{Builder, chiselRuntimeDeprecated, throwException}


/** Exists to unify common interfaces of [[Bits]] and [[Reset]]
  * Workaround because macros cannot override abstract methods
  */
private[chisel3] sealed trait ToBoolable extends Element {

  /** Casts this object to a [[Bool]]
    *
    * @note Width must be known and equal to 1
    */
  final def toBool(): Bool = macro SourceInfoWhiteboxTransform.noArg

  def do_toBool(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool
}


trait Reset extends Element with ToBoolable

/** A data type for values represented by a single bitvector. Provides basic
  * bitwise operations.
  */
//scalastyle:off number.of.methods
abstract class Bits(width: Width, override val litArg: Option[LitArg])
  extends Element(width) with ToBoolable {
  // TODO: perhaps make this concrete?
  // Arguments for: self-checking code (can't do arithmetic on bits)
  // Arguments against: generates down to a FIRRTL UInt anyways

  // Only used for in a few cases, hopefully to be removed
  private[core] def cloneTypeWidth(width: Width): this.type

  def cloneType: this.type = cloneTypeWidth(width)

  final def tail(n: Int): UInt = macro SourceInfoTransform.nArg
  final def head(n: Int): UInt = macro SourceInfoTransform.nArg

  def do_tail(n: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = {
    val w = width match {
      case KnownWidth(x) =>
        require(x >= n, s"Can't tail($n) for width $x < $n")
        Width(x - n)
      case UnknownWidth() => Width()
    }
    binop(sourceInfo, UInt(width = w), TailOp, n)
  }


  def do_head(n: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = {
    width match {
      case KnownWidth(x) => require(x >= n, s"Can't head($n) for width $x < $n")
      case UnknownWidth() =>
    }
    binop(sourceInfo, UInt(Width(n)), HeadOp, n)
  }

  /** Returns the specified bit on this wire as a [[Bool]], statically
    * addressed.
    */
  final def apply(x: BigInt): Bool = macro SourceInfoTransform.xArg

  final def do_apply(x: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = {
    if (x < 0) {
      Builder.error(s"Negative bit indices are illegal (got $x)")
    }
    if (isLit()) {
      (((litValue() >> x.toInt) & 1) == 1).asBool
    } else {

      requireIsHardware(this, "bits to be indexed")
      pushOp(DefPrim(sourceInfo, Bool(), BitsExtractOp, this.ref, ILit(x), ILit(x)))
    }
  }

  /** Returns the specified bit on this wire as a [[Bool]], statically
    * addressed.
    *
    * @note convenience method allowing direct use of Ints without implicits
    */
  final def apply(x: Int): Bool = macro SourceInfoTransform.xArg

  final def do_apply(x: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = apply(BigInt(x))


  /** Returns a subset of bits on this wire from `hi` to `lo` (inclusive),
    * statically addressed.
    *
    * @example
    * {{{
    * myBits = 0x5 = 0b101
    * myBits(1,0) => 0b01  // extracts the two least significant bits
    * }}}
    */
  final def apply(x: Int, y: Int): UInt = macro SourceInfoTransform.xyArg

  final def do_apply(x: Int, y: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = {
    if (x < y || y < 0) {
      Builder.error(s"Invalid bit range ($x,$y)")
    }
    val w = x - y + 1
    if (isLit()) {
      ((litValue >> y) & ((BigInt(1) << w) - 1)).asUInt(w.W)
    } else {
      requireIsHardware(this, "bits to be sliced")
      pushOp(DefPrim(sourceInfo, UInt(Width(w)), BitsExtractOp, this.ref, ILit(x), ILit(y)))
    }
  }

  // REVIEW TODO: again, is this necessary? Or just have this and use implicits?
  final def apply(x: BigInt, y: BigInt): UInt = macro SourceInfoTransform.xyArg

  final def do_apply(x: BigInt, y: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    apply(x.toInt, y.toInt)

  private[core] def unop[T <: Data](sourceInfo: SourceInfo, dest: T, op: PrimOp): T = {
    requireIsHardware(this, "bits operated on")
    pushOp(DefPrim(sourceInfo, dest, op, this.ref))
  }
  private[core] def binop[T <: Data](sourceInfo: SourceInfo, dest: T, op: PrimOp, other: BigInt): T = {
    requireIsHardware(this, "bits operated on")
    pushOp(DefPrim(sourceInfo, dest, op, this.ref, ILit(other)))
  }
  private[core] def binop[T <: Data](sourceInfo: SourceInfo, dest: T, op: PrimOp, other: Bits): T = {
    requireIsHardware(this, "bits operated on")
    requireIsHardware(other, "bits operated on")
    pushOp(DefPrim(sourceInfo, dest, op, this.ref, other.ref))
  }
  private[core] def compop(sourceInfo: SourceInfo, op: PrimOp, other: Bits): Bool = {
    requireIsHardware(this, "bits operated on")
    requireIsHardware(other, "bits operated on")
    pushOp(DefPrim(sourceInfo, Bool(), op, this.ref, other.ref))
  }
  private[core] def redop(sourceInfo: SourceInfo, op: PrimOp): Bool = {
    requireIsHardware(this, "bits operated on")
    pushOp(DefPrim(sourceInfo, Bool(), op, this.ref))
  }

  /** Returns this wire zero padded up to the specified width.
    *
    * @note for SInts only, this does sign extension
    */
  final def pad(that: Int): this.type = macro SourceInfoTransform.thatArg

  def do_pad(that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): this.type =
    binop(sourceInfo, cloneTypeWidth(this.width max Width(that)), PadOp, that)

  /** Returns this wire bitwise-inverted. */
//  final def unary_~ (): Bits = macro SourceInfoWhiteboxTransform.noArg

//  def do_unary_~ (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bits


//  /** Shift left operation */
//  // REVIEW TODO: redundant
//  // REVIEW TODO: should these return this.type or Bits?
//  final def << (that: BigInt): Bits = macro SourceInfoWhiteboxTransform.thatArg
//
//  def do_<< (that: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bits
//


//  def do_<< (that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bits


//  def do_<< (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bits

//  /** Shift right operation */
//  // REVIEW TODO: redundant
//  final def >> (that: BigInt): Bits = macro SourceInfoWhiteboxTransform.thatArg

//  def do_>> (that: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bits

//  def do_>> (that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bits


//  def do_>> (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bits

  /** Returns the contents of this wire as a [[Vec]] of [[Bool]]s.
    */
  final def toBools(): Seq[Bool] = macro SourceInfoTransform.noArg

  def do_toBools(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Seq[Bool] =
    Seq.tabulate(this.getWidth)(i => this(i))


  /** Returns the specified bit on this wire as a [[Bool]], dynamically
    * addressed.
    */
  final def apply(x: UInt): Bool = macro SourceInfoTransform.xArg

  final def do_apply(x: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = {
    val theBits = this.asUInt() >> x
    theBits(0)
  }

  /** Reinterpret cast to a SInt.
    *
    * @note value not guaranteed to be preserved: for example, an UInt of width
    * 3 and value 7 (0b111) would become a SInt with value -1
    */
  final def asSInt(): SInt = macro SourceInfoTransform.noArg

  def do_asSInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt

  /** Reinterpret cast as a FixedPoint.
    *
    * @note value not guaranteed to be preserved: for example, an UInt of width
    * 3 and value 7 (0b111) would become a FixedInt with value -1, the interpretation
    * of the number is also affected by the specified binary point.  Caution advised
    */
  final def asFixedPoint(that: BinaryPoint): FixedPoint = macro SourceInfoTransform.thatArg

  def do_asFixedPoint(that: BinaryPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint = {
    throwException(s"Cannot call .asFixedPoint on $this")
  }

//  /** Reinterpret cast to Bits. */
//  @chiselRuntimeDeprecated
//  @deprecated("Use asUInt, which does the same thing but returns a more concrete type", "chisel3")
//  final def asBits(implicit compileOptions: CompileOptions): Bits = {
//    implicit val sourceInfo = DeprecatedSourceInfo
//    do_asUInt
//  }

//  @chiselRuntimeDeprecated
//  @deprecated("Use asSInt, which makes the reinterpret cast more explicit", "chisel3")
//  final def toSInt(implicit compileOptions: CompileOptions): SInt = {
//    implicit val sourceInfo = DeprecatedSourceInfo
//    do_asSInt
//  }
//
//  @chiselRuntimeDeprecated
//  @deprecated("Use asUInt, which makes the reinterpret cast more explicit", "chisel3")
//  final def toUInt(implicit compileOptions: CompileOptions): UInt = {
//    implicit val sourceInfo = DeprecatedSourceInfo
//    do_asUInt
//  }

  final def do_toBool(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = {
    width match {
      case KnownWidth(1) => this(0)
      case _ => throwException(s"can't covert UInt<$width> to Bool")
    }
  }

  /** Default print as [[Decimal]] */
  final def toPrintable: Printable = Decimal(this)

  protected final def validateShiftAmount(x: Int): Int = {
    if (x < 0)
      Builder.error(s"Negative shift amounts are illegal (got $x)")
    x
  }
}

object Bits extends UIntFactory