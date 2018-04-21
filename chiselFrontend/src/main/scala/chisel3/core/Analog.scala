// See LICENSE for license details.

package chisel3.core

import scala.language.experimental.macros
import collection.mutable

import chisel3.internal._
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo.SourceInfo


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

  def toPrintable: Printable = PString("Analog")
}
/** Object that provides factory methods for [[Analog]] objects
  *
  * @note This API is experimental and subject to change
  */
object Analog {
  def apply(width: Width): Analog = new Analog(width)
}
