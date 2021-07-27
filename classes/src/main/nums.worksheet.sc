import java.lang.annotation.Native
import java.lang.annotation.Native
abstract class Nat:
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat

object Zero extends Nat: 
  def isZero: Boolean = true
  def predecessor: Nat = ???
  def successor: Nat = Succ(this)
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if that.isZero then this else ???

class Succ(n: Nat) extends Nat:
  def isZero: Boolean = false
  def predecessor: Nat = n
  def successor: Nat = Succ(this)
  def + (that: Nat): Nat = Succ(n + that)
  def - (that: Nat): Nat = if that.isZero then this else n - that.predecessor
  override def toString = s"Succ($n)"

val one = Succ(Zero)