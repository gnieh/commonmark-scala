/* Copyright (c) 2016 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package commonmark

import scala.util.control.NonFatal
import scala.util.matching.Regex
import Regex.Match

/** Abstract representation of an input to be parsed.
 *  It provides methods to easily access current
 *  character and its context, as well as methods
 *  to move forward in this input. No backtracking is
 *  possible once a character was consumed.
 *
 *  It is possible to create local buffer of the input starting at the current position,
 *  allowing to either commit or rollback the consumed input.
 *  It is handy when it is not easy to determine a priori whether
 *  the current input matches a construct using a regular expression.
 */
class StringScanner(input: String) {

  private val size = input.size

  private var pointers = List(0)

  private def pointer = pointers.head

  private def pointer_=(p: Int): Unit = {
    val _ :: rest = pointers
    pointers = p :: rest
  }

  /** Returns the current character without consuming it.
   *
   *  @group NonConsuming
   */
  def peek: Option[Char] =
    if (pointer >= size)
      None
    else
      Some(input(pointer))

  /** Returns the last consumed character.
   *
   *  @group NonConsuming
   */
  def previous: Option[Char] =
    if (pointer == 0)
      None
    else
      Some(input(pointer - 1))

  /** Consumes the first character.
   *
   *  @group Consuming
   */
  def next(): Char =
    if (hasNext) {
      pointer += 1
      input(pointer - 1)
    } else {
      throw new NoSuchElementException("EOS reached")
    }

  /** Indicates whehter there is further characters in the input at the current position.
   *
   *  @group NonConsuming
   */
  def hasNext: Boolean =
    pointer < size

  /* Checks that the current character is the given one
   * and consumes it.
   *
   * @group Consuming
    */
  def accept(c: Char): Unit =
    if (hasNext && input(pointer) == c)
      pointer += 1
    else
      throw new NoSuchElementException(f"Character $c expected")

  /* Checks that the input at the current position satsifies the regular expression
   * and consumes the matched prefix.
   * Returns the matched group in case of success (by default, the entire match).
   *
   * @group Consuming
    */
  def accept(re: Regex, group: Int = 0): String =
    re.findFirstMatchIn(input.substring(pointer)) match {
      case Some(m) if m.start == 0 =>
        pointer += m.end
        m.group(group)
      case _ =>
        throw new NoSuchElementException(f"Input does not respect expect regular expression $re")
    }

  /* Indicates whether the input at the current position satsifies the regular expression
   * without consuming it.
   *
   * @group NonConsuming
    */
  def satisfy(re: Regex): Boolean =
    re.findFirstMatchIn(input.substring(pointer)) match {
      case Some(m) if m.start == 0 =>
        true
      case _ =>
        false
    }

  /** Consumes input from the current position
   *  until the given regular expression matches (inclusive) and returns the (possibly empty)
   *  matching string.
   *  If input ends before the closing regular expression matches, then returns `None` and consumes nothing.
   *
   *  @group Consuming
   */
  def consumeTill(re: Regex): Option[String] =
    re.findFirstMatchIn(input.substring(pointer)) match {
      case Some(m) =>
        val res = input.substring(pointer, m.start + pointer)
        pointer = m.end
        Some(res)
      case None =>
        None
    }

  def buffered[T](f: => Action[T]): Option[T] = try {
    pointers ::= pointer
    f match {
      case Commit(v) =>
        // discard previous pointer
        val p :: _ :: rest = pointers
        // repush committed opinter
        pointers = p :: rest
        // return result
        Some(v)
      case Rollback =>
        // just discard this pointer
        pointers = pointers.tail
        None
    }
  } catch {
    case NonFatal(e) =>
      pointers = pointers.tail
      throw e
  }

}

/** An action to execute at the end of a buffered tratement.
 *  Either commit or rollback.
 */
sealed trait Action[+T]
object Action {

  def apply[T](o: Option[T]): Action[T] =
    o match {
      case Some(v) => Commit(v)
      case None    => Rollback
    }

}
final case class Commit[T](value: T) extends Action[T]
case object Rollback extends Action[Nothing]
