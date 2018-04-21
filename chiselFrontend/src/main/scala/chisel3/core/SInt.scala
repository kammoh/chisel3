// See LICENSE for license details.

package chisel3.core

import scala.language.experimental.macros
import chisel3.internal.Builder.{pushCommand, pushOp}
import chisel3.internal.{chiselRuntimeDeprecated, throwException}
import chisel3.internal.firrtl.PrimOp._
import chisel3.internal.firrtl.{BinaryPoint, Connect, DefInvalid, DefPrim, ILit, KnownBinaryPoint, KnownSIntRange,
  NumericBound, Range, SLit, Width}
import chisel3.internal.sourceinfo.{SourceInfo, SourceInfoTransform}


//scalastyle:off method.name

/** Element is a leaf data type: it cannot contain other Data objects. Example
  * uses are for representing primitive data types, like integers and bits.
  */
abstract class Element(private[chisel3] val width: Width) extends Data {
  private[chisel3] override def bind(target: Binding, parentDirection: SpecifiedDirection) {
    binding = target
    val resolvedDirection = SpecifiedDirection.fromParent(parentDirection, specifiedDirection)
    direction = resolvedDirection match {
      case SpecifiedDirection.Unspecified | SpecifiedDirection.Flip => ActualDirection.Unspecified
      case SpecifiedDirection.Output => ActualDirection.Output
      case SpecifiedDirection.Input => ActualDirection.Input
    }
  }

  private[chisel3] final def allElements: Seq[Element] = Seq(this)
  def widthKnown: Boolean = width.known
  def name: String = getRef.name

  private[core] def legacyConnect(that: Data)(implicit sourceInfo: SourceInfo): Unit = {
    // If the source is a DontCare, generate a DefInvalid for the sink,
    //  otherwise, issue a Connect.
    if (that == DontCare) {
      pushCommand(DefInvalid(sourceInfo, this.lref))
    } else {
      pushCommand(Connect(sourceInfo, this.lref, that.ref))
    }
  }


  /** Returns this wire concatenated with `other`, where this wire forms the
    * most significant part and `other` forms the least significant part.
    *
    * The width of the output is sum of the inputs.
    */
  final def ## (that: Bits): UInt = macro SourceInfoTransform.thatArg

  def do_## (that: Bits)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = {
    val w = this.width + that.width
    pushOp(DefPrim(sourceInfo, UInt(w), ConcatOp, this.ref, that.ref))
  }
}


sealed class SInt private[core] (width: Width, lit: Option[SLit] = None)
  extends Bits(width, lit) with Num[SInt] {

  private[core] override def typeEquivalent(that: Data): Boolean =
    this.getClass == that.getClass && this.width == that.width  // TODO: should this be true for unspecified widths?

  private[core] override def cloneTypeWidth(w: Width): this.type =
    new SInt(w).asInstanceOf[this.type]

  final def unary_- (): SInt = macro SourceInfoTransform.noArg
  final def unary_-% (): SInt = macro SourceInfoTransform.noArg

  def unary_- (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt = 0.S - this
  def unary_-% (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt = 0.S -% this

  /** add (default - no growth) operator */
  override def do_+ (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    this +% that
  /** subtract (default - no growth) operator */
  override def do_- (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    this -% that
  override def do_* (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, SInt(this.width + that.width), TimesOp, that)
  override def do_/ (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, SInt(this.width), DivideOp, that)
  override def do_% (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, SInt(this.width), RemOp, that)

  final def * (that: UInt): SInt = macro SourceInfoTransform.thatArg
  def do_* (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt = {
    val thatToSInt = that.zext()
    val result = binop(sourceInfo, SInt(this.width + thatToSInt.width), TimesOp, thatToSInt)
    result.tail(1).asSInt
  }

  /** add (width +1) operator */
  final def +& (that: SInt): SInt = macro SourceInfoTransform.thatArg
  /** add (no growth) operator */
  final def +% (that: SInt): SInt = macro SourceInfoTransform.thatArg
  /** subtract (width +1) operator */
  final def -& (that: SInt): SInt = macro SourceInfoTransform.thatArg
  /** subtract (no growth) operator */
  final def -% (that: SInt): SInt = macro SourceInfoTransform.thatArg

  def do_+& (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, SInt((this.width max that.width) + 1), AddOp, that)
  def do_+% (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    (this +& that).tail(1).asSInt
  def do_-& (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, SInt((this.width max that.width) + 1), SubOp, that)
  def do_-% (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    (this -& that).tail(1).asSInt

  final def & (that: SInt): SInt = macro SourceInfoTransform.thatArg
  final def | (that: SInt): SInt = macro SourceInfoTransform.thatArg
  final def ^ (that: SInt): SInt = macro SourceInfoTransform.thatArg

  def do_& (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, UInt(this.width max that.width), BitAndOp, that).asSInt
  def do_| (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, UInt(this.width max that.width), BitOrOp, that).asSInt
  def do_^ (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, UInt(this.width max that.width), BitXorOp, that).asSInt

  /** Returns this wire bitwise-inverted. */
  def do_unary_~ (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    unop(sourceInfo, UInt(width = width), BitNotOp).asSInt

  override def do_< (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, LessOp, that)
  override def do_> (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, GreaterOp, that)
  override def do_<= (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, LessEqOp, that)
  override def do_>= (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, GreaterEqOp, that)

  @chiselRuntimeDeprecated
  @deprecated("Use '=/=', which avoids potential precedence problems", "chisel3")
  final def != (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = this =/= that
  final def =/= (that: SInt): Bool = macro SourceInfoTransform.thatArg
  final def === (that: SInt): Bool = macro SourceInfoTransform.thatArg

  def do_=/= (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, NotEqualOp, that)
  def do_=== (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, EqualOp, that)

  //  final def abs(): UInt = macro SourceInfoTransform.noArg

  def do_abs(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt = {
    Mux(this < 0.S, (-this), this)
  }

  override def do_<< (that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, SInt(this.width + that), ShiftLeftOp, validateShiftAmount(that))
  override def do_<< (that: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    this << that.toInt
  override def do_<< (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, SInt(this.width.dynamicShiftLeft(that.width)), DynamicShiftLeftOp, that)
  override def do_>> (that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, SInt(this.width.shiftRight(that)), ShiftRightOp, validateShiftAmount(that))
  override def do_>> (that: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    this >> that.toInt
  override def do_>> (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    binop(sourceInfo, SInt(this.width), DynamicShiftRightOp, that)

  override def do_asUInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = pushOp(DefPrim(sourceInfo, UInt(this.width), AsUIntOp, ref))
  override def do_asSInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt = this
  override def do_asFixedPoint(binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint = {
    binaryPoint match {
      case KnownBinaryPoint(value) =>
        val iLit = ILit(value)
        pushOp(DefPrim(sourceInfo, FixedPoint(width, binaryPoint), AsFixedPointOp, ref, iLit))
      case _ =>
        throwException(s"cannot call $this.asFixedPoint(binaryPoint=$binaryPoint), you must specify a known binaryPoint")
    }
  }

  private[core] override def connectFromBits(that: Bits)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) {
    this := that.asSInt
  }
}

trait SIntFactory {
  /** Create an SInt type with inferred width. */
  def apply(): SInt = apply(Width())
  /** Create a SInt type or port with fixed width. */
  def apply(width: Width): SInt = new SInt(width)

  /** Create a SInt with the specified range */
  def apply(range: Range): SInt = {
    apply(range.getWidth)
  }
  /** Create a SInt with the specified range */
  def apply(range: (NumericBound[Int], NumericBound[Int])): SInt = {
    apply(KnownSIntRange(range._1, range._2))
  }

  /** Create an SInt literal with specified width. */
  protected[chisel3] def Lit(value: BigInt, width: Width): SInt = {
    val lit = SLit(value, width)
    val result = new SInt(lit.width, Some(lit))
    // Bind result to being an Literal
    result.bind(LitBinding())
    result
  }
}

object SInt extends SIntFactory
