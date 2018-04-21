// See LICENSE for license details.

package chisel3.core

import scala.language.experimental.macros
import chisel3.internal.sourceinfo.{SourceInfo, SourceInfoTransform, SourceInfoWhiteboxTransform}

// REVIEW TODO: Further discussion needed on what Num actually is.
/** Abstract trait defining operations available on numeric-like wire data
  * types.
  */
trait Num[T <: Data] {
  self: Num[T] =>
  // def << (b: T): T
  // def >> (b: T): T
  //def unary_-(): T

  /** Returns this wire statically left shifted by the specified amount,
    * inserting zeros into the least significant bits.
    *
    * The width of the output is `other` larger than the input.
    */
  final def << (that: Int): T = macro SourceInfoWhiteboxTransform.thatArg

  /** Returns this wire dynamically left shifted by the specified amount,
    * inserting zeros into the least significant bits.
    *
    * The width of the output is `pow(2, width(other))` larger than the input.
    */
  final def << (that: UInt): T = macro SourceInfoWhiteboxTransform.thatArg


  def do_<< (that: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T


  def do_<< (that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T


  def do_<< (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T


  /** Returns this wire statically right shifted by the specified amount,
    * inserting zeros into the most significant bits.
    *
    * The width of the output is the same as the input.
    */
  final def >> (that: Int): T = macro SourceInfoWhiteboxTransform.thatArg


  /** Returns this wire dynamically right shifted by the specified amount,
    * inserting zeros into the most significant bits.
    *
    * The width of the output is the same as the input.
    */
  final def >> (that: UInt): T = macro SourceInfoWhiteboxTransform.thatArg

  def do_>> (that: BigInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T

  def do_>> (that: Int)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T

  def do_>> (that: UInt)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T


  // REVIEW TODO: double check ops conventions against FIRRTL

  /** Outputs the sum of `this` and `b`. The resulting width is the max of the
    * operands plus 1 (should not overflow).
    */
  final def + (that: T): T = macro SourceInfoTransform.thatArg

  def do_+ (that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T

  /** Outputs the product of `this` and `b`. The resulting width is the sum of
    * the operands.
    *
    * @note can generate a single-cycle multiplier, which can result in
    * significant cycle time and area costs
    */
  final def * (that: T): T = macro SourceInfoTransform.thatArg

  def do_* (that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T

  /** Outputs the quotient of `this` and `b`.
    *
    * TODO: full rules
    */
  final def / (that: T): T = macro SourceInfoTransform.thatArg

  def do_/ (that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T

  final def % (that: T): T = macro SourceInfoTransform.thatArg

  def do_% (that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T

  /** Outputs the difference of `this` and `b`. The resulting width is the max
    *  of the operands plus 1 (should not overflow).
    */
  final def - (that: T): T = macro SourceInfoTransform.thatArg

  def do_- (that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T

  /** Outputs true if `this` < `b`.
    */
  final def < (that: T): Bool = macro SourceInfoTransform.thatArg

  def do_< (that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool

  /** Outputs true if `this` <= `b`.
    */
  final def <= (that: T): Bool = macro SourceInfoTransform.thatArg

  def do_<= (that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool

  /** Outputs true if `this` > `b`.
    */
  final def > (that: T): Bool = macro SourceInfoTransform.thatArg

  def do_> (that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool

  /** Outputs true if `this` >= `b`.
    */
  final def >= (that: T): Bool = macro SourceInfoTransform.thatArg

  def do_>= (that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Bool

  /** Outputs the absolute value of `this`. The resulting width is the unchanged */
  final def abs(): T = macro SourceInfoTransform.noArg
  def do_abs(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T

  /** Outputs the minimum of `this` and `b`. The resulting width is the max of
    * the operands.
    */
  final def min(that: T): T = macro SourceInfoTransform.thatArg

  def do_min(that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T =
    Mux(this < that, this.asInstanceOf[T], that)

  /** Outputs the maximum of `this` and `b`. The resulting width is the max of
    * the operands.
    */
  final def max(that: T): T = macro SourceInfoTransform.thatArg

  def do_max(that: T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T =
    Mux(this < that, that, this.asInstanceOf[T])

  final def unary_~ (): T = macro SourceInfoWhiteboxTransform.noArg

}
