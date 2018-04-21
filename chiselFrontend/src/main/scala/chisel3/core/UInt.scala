package chisel3.core


import chisel3.internal.Builder.pushOp
import chisel3.internal.firrtl.PrimOp._
import chisel3.internal.firrtl.{BinaryPoint, DefPrim, ILit, KnownBinaryPoint, KnownUIntRange, NumericBound, Range, ULit, Width}
import chisel3.internal.sourceinfo.{SourceInfo, SourceInfoTransform, UIntTransform}
import chisel3.internal.{chiselRuntimeDeprecated, throwException}

import scala.language.experimental.macros


/** A data type for unsigned integers, represented as a binary bitvector.
  * Defines arithmetic operations between other integer types.
  */
sealed class UInt private[core] (width: Width, lit: Option[ULit] = None)
  extends Bits(width, lit) with Num[UInt] {

  private[core] override def typeEquivalent(that: Data): Boolean =
    that.isInstanceOf[UInt] && this.width == that.width

  private[core] override def cloneTypeWidth(w: Width): this.type =
    new UInt(w).asInstanceOf[this.type]

  // TODO: refactor to share documentation with Num or add independent scaladoc
  final def unary_- (): UInt = macro SourceInfoTransform.noArg
  final def unary_-% (): UInt = macro SourceInfoTransform.noArg

  def do_unary_- (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) : UInt = 0.U - this
  def do_unary_-% (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = 0.U -% this

  override def do_+ (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = this +% that
  override def do_- (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = this -% that
  override def do_/ (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt(this.width), DivideOp, that)
  override def do_% (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt(this.width), RemOp, that)
  override def do_* (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt(this.width + that.width), TimesOp, that)

  final def * (that: SInt): SInt = macro SourceInfoTransform.thatArg
  def do_* (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt = that * this

  final def +& (that: UInt): UInt = macro SourceInfoTransform.thatArg
  final def +% (that: UInt): UInt = macro SourceInfoTransform.thatArg
  final def -& (that: UInt): UInt = macro SourceInfoTransform.thatArg
  final def -% (that: UInt): UInt = macro SourceInfoTransform.thatArg

  def do_+& (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt((this.width max that.width) + 1), AddOp, that)
  def do_+% (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    (this +& that).tail(1)
  def do_-& (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, SInt((this.width max that.width) + 1), SubOp, that).asUInt
  def do_-% (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    (this -& that).tail(1)

  final def & (that: UInt): UInt = macro SourceInfoTransform.thatArg
  final def | (that: UInt): UInt = macro SourceInfoTransform.thatArg
  final def ^ (that: UInt): UInt = macro SourceInfoTransform.thatArg

  //  override def abs: UInt = macro SourceInfoTransform.noArg
  def do_abs(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = this

  def do_& (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt(this.width max that.width), BitAndOp, that)
  def do_| (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt(this.width max that.width), BitOrOp, that)
  def do_^ (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt(this.width max that.width), BitXorOp, that)


  /** Returns this wire bitwise-inverted. */
  def do_unary_~ (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    unop(sourceInfo, UInt(width = width), BitNotOp)

  // REVIEW TODO: Can this be defined on Bits?
  final def orR(): Bool = macro SourceInfoTransform.noArg
  final def andR(): Bool = macro SourceInfoTransform.noArg
  final def xorR(): Bool = macro SourceInfoTransform.noArg

  def do_orR(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = this =/= 0.U
  def do_andR(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = ~this === 0.U
  def do_xorR(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = redop(sourceInfo, XorReduceOp)

  override def do_< (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, LessOp, that)
  override def do_> (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, GreaterOp, that)
  override def do_<= (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, LessEqOp, that)
  override def do_>= (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, GreaterEqOp, that)

  @chiselRuntimeDeprecated
  @deprecated("Use '=/=', which avoids potential precedence problems", "chisel3")
  final def != (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = this =/= that
  final def =/= (that: UInt): Bool = macro SourceInfoTransform.thatArg
  final def === (that: UInt): Bool = macro SourceInfoTransform.thatArg

  def do_=/= (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, NotEqualOp, that)
  def do_=== (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, EqualOp, that)

  final def unary_! () : Bool = macro SourceInfoTransform.noArg

  def do_unary_! (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) : Bool = this === 0.U(1.W)

  override def do_<< (that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt(this.width + that), ShiftLeftOp, validateShiftAmount(that))
  override def do_<< (that: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    this << that.toInt
  override def do_<< (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt(this.width.dynamicShiftLeft(that.width)), DynamicShiftLeftOp, that)
  override def do_>> (that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt(this.width.shiftRight(that)), ShiftRightOp, validateShiftAmount(that))
  override def do_>> (that: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    this >> that.toInt
  override def do_>> (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    binop(sourceInfo, UInt(this.width), DynamicShiftRightOp, that)

  final def bitSet(off: UInt, dat: Bool): UInt = macro UIntTransform.bitset

  def do_bitSet(off: UInt, dat: Bool)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = {
    val bit = 1.U(1.W) << off
    Mux(dat, this | bit, ~(~this | bit))
  }

  /** Returns this UInt as a [[SInt]] with an additional zero in the MSB.
    */
  // TODO: this eventually will be renamed as toSInt, once the existing toSInt
  // completes its deprecation phase.
  final def zext(): SInt = macro SourceInfoTransform.noArg
  def do_zext(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    pushOp(DefPrim(sourceInfo, SInt(width + 1), ConvertOp, ref))

  /** Returns this UInt as a [[SInt]], without changing width or bit value. The
    * SInt is not guaranteed to have the same value (for example, if the MSB is
    * high, it will be interpreted as a negative value).
    */
  override def do_asSInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt =
    pushOp(DefPrim(sourceInfo, SInt(width), AsSIntOp, ref))
  override def do_asUInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = this
  override def do_asFixedPoint(binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint = {
    binaryPoint match {
      case KnownBinaryPoint(value) =>
        val iLit = ILit(value)
        pushOp(DefPrim(sourceInfo, FixedPoint(width, binaryPoint), AsFixedPointOp, ref, iLit))
      case _ =>
        throwException(s"cannot call $this.asFixedPoint(binaryPoint=$binaryPoint), you must specify a known binaryPoint")
    }
  }

  private[core] override def connectFromBits(that: Bits)(implicit sourceInfo: SourceInfo,
                                                         compileOptions: CompileOptions): Unit = {
    this := that.asUInt
  }
}

// This is currently a factory because both Bits and UInt inherit it.
trait UIntFactory {
  /** Create a UInt type with inferred width. */
  def apply(): UInt = apply(Width())
  /** Create a UInt port with specified width. */
  def apply(width: Width): UInt = new UInt(width)

  /** Create a UInt literal with specified width. */
  protected[chisel3] def Lit(value: BigInt, width: Width): UInt = {
    val lit = ULit(value, width)
    val result = new UInt(lit.width, Some(lit))
    // Bind result to being an Literal
    result.bind(LitBinding())
    result
  }

  /** Create a UInt with the specified range */
  def apply(range: Range): UInt = {
    apply(range.getWidth)
  }
  /** Create a UInt with the specified range */
  def apply(range: (NumericBound[Int], NumericBound[Int])): UInt = {
    apply(KnownUIntRange(range._1, range._2))
  }
}

object UInt extends UIntFactory


// REVIEW TODO: Why does this extend UInt and not Bits? Does defining airth
// operations on a Bool make sense?
/** A data type for booleans, defined as a single bit indicating true or false.
  */
sealed class Bool(lit: Option[ULit] = None) extends UInt(1.W, lit) with Reset {
  private[core] override def cloneTypeWidth(w: Width): this.type = {
    require(!w.known || w.get == 1)
    new Bool().asInstanceOf[this.type]
  }

  // REVIEW TODO: Why does this need to exist and have different conventions
  // than Bits?
  final def & (that: Bool): Bool = macro SourceInfoTransform.thatArg
  final def | (that: Bool): Bool = macro SourceInfoTransform.thatArg
  final def ^ (that: Bool): Bool = macro SourceInfoTransform.thatArg

  def do_& (that: Bool)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool =
    binop(sourceInfo, Bool(), BitAndOp, that)
  def do_| (that: Bool)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool =
    binop(sourceInfo, Bool(), BitOrOp, that)
  def do_^ (that: Bool)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool =
    binop(sourceInfo, Bool(), BitXorOp, that)

  /** Returns this wire bitwise-inverted. */
  override def do_unary_~ (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool =
    unop(sourceInfo, Bool(), BitNotOp)

  /** Outputs the logical OR of two Bools.
    */
  def || (that: Bool): Bool = macro SourceInfoTransform.thatArg

  def do_|| (that: Bool)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = this | that

  /** Outputs the logical AND of two Bools.
    */
  def && (that: Bool): Bool = macro SourceInfoTransform.thatArg

  def do_&& (that: Bool)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = this & that

  /** Reinterprets this Bool as a Clock.  */
  def asClock(): Clock = macro SourceInfoTransform.noArg

  def do_asClock(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Clock = pushOp(DefPrim(sourceInfo, Clock(), AsClockOp, ref))
}



trait BoolFactory {
  /** Creates an empty Bool.
    */
  def apply(): Bool = new Bool()

  /** Creates Bool literal.
    */
  protected[chisel3] def Lit(x: Boolean): Bool = {
    val result = new Bool(Some(ULit(if (x) 1 else 0, Width(1))))
    // Bind result to being an Literal
    result.bind(LitBinding())
    result
  }
}

object Bool extends BoolFactory