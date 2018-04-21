// See LICENSE for license details.

package chisel3.core

import scala.language.experimental.macros
import collection.mutable

import chisel3.internal._
import chisel3.internal.Builder.{pushCommand, pushOp}
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo.{SourceInfo, DeprecatedSourceInfo, SourceInfoTransform, SourceInfoWhiteboxTransform,
  UIntTransform}
import chisel3.internal.firrtl.PrimOp._

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


//scalastyle:off number.of.methods
/**
  * A sealed class representing a fixed point number that has a bit width and a binary point
  * The width and binary point may be inferred.
  *
  * IMPORTANT: The API provided here is experimental and may change in the future.
  *
  * @param width       bit width of the fixed point number
  * @param binaryPoint the position of the binary point with respect to the right most bit of the width
  *                    currently this should be positive but it is hoped to soon support negative points
  *                    and thus use this field as a simple exponent
  * @param lit
  */
sealed class FixedPoint private (width: Width, val binaryPoint: BinaryPoint, lit: Option[FPLit] = None)
    extends Bits(width, lit) with Num[FixedPoint] {
  private[core] override def typeEquivalent(that: Data): Boolean = that match {
    case that: FixedPoint => this.width == that.width && this.binaryPoint == that.binaryPoint  // TODO: should this be true for unspecified widths?
    case _ => false
  }

  private[core] override def cloneTypeWidth(w: Width): this.type =
    new FixedPoint(w, binaryPoint).asInstanceOf[this.type]

  override def connect (that: Data)(implicit sourceInfo: SourceInfo, connectCompileOptions: CompileOptions): Unit = that match {
    case _: FixedPoint|DontCare => super.connect(that)
    case _ => this badConnect that
  }

  final def unary_- (): FixedPoint = macro SourceInfoTransform.noArg
  final def unary_-% (): FixedPoint = macro SourceInfoTransform.noArg

  def unary_- (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint = FixedPoint.fromBigInt(0) - this
  def unary_-% (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint = FixedPoint.fromBigInt(0) -% this

  /** add (default - no growth) operator */
  override def do_+ (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    this +% that
  /** subtract (default - no growth) operator */
  override def do_- (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    this -% that
  override def do_* (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    binop(sourceInfo, FixedPoint(this.width + that.width, this.binaryPoint + that.binaryPoint), TimesOp, that)
  override def do_/ (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    throwException(s"division is illegal on FixedPoint types")
  override def do_% (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    throwException(s"mod is illegal on FixedPoint types")

  final def * (that: UInt): FixedPoint = macro SourceInfoTransform.thatArg
  def do_* (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    binop(sourceInfo, FixedPoint(this.width + that.width, binaryPoint), TimesOp, that)

  final def * (that: SInt): FixedPoint = macro SourceInfoTransform.thatArg
  def do_* (that: SInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    binop(sourceInfo, FixedPoint(this.width + that.width, binaryPoint), TimesOp, that)

  /** add (width +1) operator */
  final def +& (that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg
  /** add (no growth) operator */
  final def +% (that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg
  /** subtract (width +1) operator */
  final def -& (that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg
  /** subtract (no growth) operator */
  final def -% (that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg

  def do_+& (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint = {
    (this.width, that.width, this.binaryPoint, that.binaryPoint) match {
      case (KnownWidth(thisWidth), KnownWidth(thatWidth), KnownBinaryPoint(thisBP), KnownBinaryPoint(thatBP)) =>
        val thisIntWidth = thisWidth - thisBP
        val thatIntWidth = thatWidth - thatBP
        val newBinaryPoint = thisBP max thatBP
        val newWidth = (thisIntWidth max thatIntWidth) + newBinaryPoint + 1
        binop(sourceInfo, FixedPoint(newWidth.W, newBinaryPoint.BP), AddOp, that)
      case _ =>
        val newBinaryPoint = this.binaryPoint max that.binaryPoint
        binop(sourceInfo, FixedPoint(UnknownWidth(), newBinaryPoint), AddOp, that)
    }
  }

  def do_+% (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    (this +& that).tail(1).asFixedPoint(this.binaryPoint max that.binaryPoint)
  def do_-& (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint = {
    (this.width, that.width, this.binaryPoint, that.binaryPoint) match {
      case (KnownWidth(thisWidth), KnownWidth(thatWidth), KnownBinaryPoint(thisBP), KnownBinaryPoint(thatBP)) =>
        val thisIntWidth = thisWidth - thisBP
        val thatIntWidth = thatWidth - thatBP
        val newBinaryPoint = thisBP max thatBP
        val newWidth = (thisIntWidth max thatIntWidth) + newBinaryPoint + 1
        binop(sourceInfo, FixedPoint(newWidth.W, newBinaryPoint.BP), SubOp, that)
      case _ =>
        val newBinaryPoint = this.binaryPoint max that.binaryPoint
        binop(sourceInfo, FixedPoint(UnknownWidth(), newBinaryPoint), SubOp, that)
    }
  }

  def do_-% (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    (this -& that).tail(1).asFixedPoint(this.binaryPoint max that.binaryPoint)

  final def & (that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg
  final def | (that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg
  final def ^ (that: FixedPoint): FixedPoint = macro SourceInfoTransform.thatArg

  def do_& (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    throwException(s"And is illegal between $this and $that")
  def do_| (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    throwException(s"Or is illegal between $this and $that")
  def do_^ (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    throwException(s"Xor is illegal between $this and $that")

  final def setBinaryPoint(that: Int): FixedPoint = macro SourceInfoTransform.thatArg

  def do_setBinaryPoint(that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint = this.binaryPoint match {
    case KnownBinaryPoint(value) =>
      binop(sourceInfo, FixedPoint(this.width + (that - value), KnownBinaryPoint(that)), SetBinaryPoint, that)
    case _ =>
      binop(sourceInfo, FixedPoint(UnknownWidth(), KnownBinaryPoint(that)), SetBinaryPoint, that)
  }

  /** Returns this wire bitwise-inverted. */
  def do_unary_~ (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    throwException(s"Not is illegal on $this")

  // TODO(chick): Consider comparison with UInt and SInt
  override def do_< (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, LessOp, that)
  override def do_> (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, GreaterOp, that)
  override def do_<= (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, LessEqOp, that)
  override def do_>= (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, GreaterEqOp, that)

  final def != (that: FixedPoint): Bool = macro SourceInfoTransform.thatArg
  final def =/= (that: FixedPoint): Bool = macro SourceInfoTransform.thatArg
  final def === (that: FixedPoint): Bool = macro SourceInfoTransform.thatArg

  def do_!= (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, NotEqualOp, that)
  def do_=/= (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, NotEqualOp, that)
  def do_=== (that: FixedPoint)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool = compop(sourceInfo, EqualOp, that)

  def do_abs(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint = {
    // TODO: remove this once we have CompileOptions threaded through the macro system.
    import chisel3.core.ExplicitCompileOptions.NotStrict
    Mux(this < 0.F(0.BP), 0.F(0.BP) - this, this)
  }

  override def do_<< (that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    binop(sourceInfo, FixedPoint(this.width + that, this.binaryPoint), ShiftLeftOp, validateShiftAmount(that))
  override def do_<< (that: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    (this << that.toInt).asFixedPoint(this.binaryPoint)
  override def do_<< (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    binop(sourceInfo, FixedPoint(this.width.dynamicShiftLeft(that.width), this.binaryPoint), DynamicShiftLeftOp, that)
  override def do_>> (that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    binop(sourceInfo, FixedPoint(this.width.shiftRight(that), this.binaryPoint), ShiftRightOp, validateShiftAmount(that))
  override def do_>> (that: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    (this >> that.toInt).asFixedPoint(this.binaryPoint)
  override def do_>> (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): FixedPoint =
    binop(sourceInfo, FixedPoint(this.width, this.binaryPoint), DynamicShiftRightOp, that)

  override def do_asUInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt = pushOp(DefPrim(sourceInfo, UInt(this.width), AsUIntOp, ref))
  override def do_asSInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): SInt = pushOp(DefPrim(sourceInfo, SInt(this.width), AsSIntOp, ref))

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
    // TODO: redefine as just asFixedPoint on that, where FixedPoint.asFixedPoint just works.
    this := (that match {
      case fp: FixedPoint => fp.asSInt.asFixedPoint(this.binaryPoint)
      case _ => that.asFixedPoint(this.binaryPoint)
    })
  }
  //TODO(chick): Consider "convert" as an arithmetic conversion to UInt/SInt
}

/** Use PrivateObject to force users to specify width and binaryPoint by name
 */
sealed trait PrivateType
private case object PrivateObject extends PrivateType

/**
  * Factory and convenience methods for the FixedPoint class
  * IMPORTANT: The API provided here is experimental and may change in the future.
  */
object FixedPoint {
  /** Create an FixedPoint type with inferred width. */
  def apply(): FixedPoint = apply(Width(), BinaryPoint())

  /** Create an FixedPoint type or port with fixed width. */
  @chiselRuntimeDeprecated
  @deprecated("Use FixedPoint(width: Width, binaryPoint: BinaryPoint) example FixedPoint(16.W, 8.BP)", "chisel3")
  def apply(width: Int, binaryPoint: Int): FixedPoint = apply(Width(width), BinaryPoint(binaryPoint))

  /** Create an FixedPoint type or port with fixed width. */
  def apply(width: Width, binaryPoint: BinaryPoint): FixedPoint = new FixedPoint(width, binaryPoint)

  /** Create an FixedPoint literal with inferred width from BigInt.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  def fromBigInt(value: BigInt, width: Width, binaryPoint: BinaryPoint): FixedPoint = {
    apply(value, width, binaryPoint)
  }
  /** Create an FixedPoint literal with inferred width from BigInt.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  def fromBigInt(value: BigInt, binaryPoint: BinaryPoint = 0.BP): FixedPoint = {
    apply(value, Width(), binaryPoint)
  }
  /** Create an FixedPoint literal with inferred width from BigInt.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  def fromBigInt(value: BigInt, width: Int, binaryPoint: Int): FixedPoint =
    if(width == -1) {
      apply(value, Width(), BinaryPoint(binaryPoint))
    }
    else {
      apply(value, Width(width), BinaryPoint(binaryPoint))
    }
  /** Create an FixedPoint literal with inferred width from Double.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  @chiselRuntimeDeprecated
  @deprecated("use fromDouble(value: Double, width: Width, binaryPoint: BinaryPoint)", "chisel3")
  def fromDouble(value: Double, dummy: PrivateType = PrivateObject,
                 width: Int = -1, binaryPoint: Int = 0): FixedPoint = {
    fromBigInt(
      toBigInt(value, binaryPoint), width = width, binaryPoint = binaryPoint
    )
  }
  /** Create an FixedPoint literal with inferred width from Double.
    * Use PrivateObject to force users to specify width and binaryPoint by name
    */
  def fromDouble(value: Double, width: Width, binaryPoint: BinaryPoint): FixedPoint = {
    fromBigInt(
      toBigInt(value, binaryPoint.get), width = width, binaryPoint = binaryPoint
    )
  }

  /** Create an FixedPoint port with specified width and binary position. */
  def apply(value: BigInt, width: Width, binaryPoint: BinaryPoint): FixedPoint = {
    val lit = FPLit(value, width, binaryPoint)
    val newLiteral = new FixedPoint(lit.width, lit.binaryPoint, Some(lit))
    newLiteral.bind(LitBinding())
    newLiteral
  }

  /**
    * How to create a bigint from a double with a specific binaryPoint
    * @param x               a double value
    * @param binaryPoint     a binaryPoint that you would like to use
    * @return
    */
  def toBigInt(x: Double, binaryPoint    : Int): BigInt = {
    val multiplier = math.pow(2,binaryPoint    )
    val result = BigInt(math.round(x * multiplier))
    result
  }

  /**
    * converts a bigInt with the given binaryPoint into the double representation
    * @param i            a bigint
    * @param binaryPoint  the implied binaryPoint of @i
    * @return
    */
  def toDouble(i: BigInt, binaryPoint    : Int): Double = {
    val multiplier = math.pow(2,binaryPoint)
    val result = i.toDouble / multiplier
    result
  }

}

/** Data type for representing bidirectional bitvectors of a given width
  *
  * Analog support is limited to allowing wiring up of Verilog BlackBoxes with bidirectional (inout)
  * pins. There is currently no support for reading or writing of Analog types within Chisel code.
  *
  * Given that Analog is bidirectional, it is illegal to assign a direction to any Analog type. It
  * is legal to "flip" the direction (since Analog can be a member of aggregate types) which has no
  * effect.
  *
  * Analog types are generally connected using the bidirectional [[attach]] mechanism, but also
  * support limited bulkconnect `<>`. Analog types are only allowed to be bulk connected *once* in a
  * given module. This is to prevent any surprising consequences of last connect semantics.
  *
  * @note This API is experimental and subject to change
  */
final class Analog private (width: Width) extends Element(width) {
  require(width.known, "Since Analog is only for use in BlackBoxes, width must be known")

  private[core] override def typeEquivalent(that: Data): Boolean =
    that.isInstanceOf[Analog] && this.width == that.width

  def cloneType: this.type = new Analog(width).asInstanceOf[this.type]

  // Used to enforce single bulk connect of Analog types, multi-attach is still okay
  // Note that this really means 1 bulk connect per Module because a port can
  //   be connected in the parent module as well
  private[core] val biConnectLocs = mutable.Map.empty[UserModule, SourceInfo]

  // Define setter/getter pairing
  // Analog can only be bound to Ports and Wires (and Unbound)
  private[chisel3] override def bind(target: Binding, parentDirection: SpecifiedDirection) {
    SpecifiedDirection.fromParent(parentDirection, specifiedDirection) match {
      case SpecifiedDirection.Unspecified | SpecifiedDirection.Flip =>
      case x => throwException(s"Analog may not have explicit direction, got '$x'")
    }
    val targetTopBinding = target match {
      case target: TopBinding => target
      case ChildBinding(parent) => parent.topBinding
    }

    // Analog counts as different directions based on binding context
    targetTopBinding match {
      case WireBinding(_) => direction = ActualDirection.Unspecified  // internal wire
      case PortBinding(_) => direction = ActualDirection.Bidirectional(ActualDirection.Default)
      case x => throwException(s"Analog can only be Ports and Wires, not '$x'")
    }
    binding = target
  }

  override def do_asUInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): UInt =
    throwException("Analog does not support asUInt")

  private[core] override def connectFromBits(that: Bits)(implicit sourceInfo: SourceInfo,
      compileOptions: CompileOptions): Unit = {
    throwException("Analog does not support connectFromBits")
  }

  final def toPrintable: Printable = PString("Analog")
}
/** Object that provides factory methods for [[Analog]] objects
  *
  * @note This API is experimental and subject to change
  */
object Analog {
  def apply(width: Width): Analog = new Analog(width)
}
