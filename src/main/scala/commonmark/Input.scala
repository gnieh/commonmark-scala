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

import scala.util.matching.Regex

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
 *  However be careful, for stream input, the more you buffer data, the more memory is potentially used.
 */
trait Input {

  /** Returns the current character without consuming it.
   *
   *  @group NonConsuming
   */
  def peek: Option[Char]

  /** Returns the last consumed character.
   *
   *  @group NonConsuming
   */
  def previous: Option[Char]

  /** Consumes the first character.
   *
   *  @group Consuming
   */
  def next(): Char

  /** Indicates whehter there is further characters in the input at the current position.
   *
   *  @group NonConsuming
   */
  def hasNext: Boolean

  /* Checks that the current character is the given one
   * and consumes it.
   *
   * @group Consuming
    */
  def accept(c: Char): Unit

  /* Checks that the input at the current position satsifies the regular expression
   * and consumes the matched prefix.
   * Returns the matched group in case of success (by default, the entire match).
   *
   * @group Consuming
    */
  def accept(re: Regex, group: Int = 0): String

  /* Indicates whether the input at the current position satsifies the regular expression
   * without consuming it.
   *
   * @group NonConsuming
    */
  def satisfy(re: Regex): Boolean

  /** Consumes input from the current position
   *  until the given regular expression matches and returns the (possibly empty)
   *  matching string.
   *  If input ends before the closing regular expression matches, then returns `None` and consumes nothing.
   *  If `inclusive` is `true`, then also consumes the matched closing string
   *  (without returning it in any case).
   *
   *  @group Consuming
   */
  def consumeTill(re: Regex, inclusive: Boolean): Option[String]

  def buffered[T](f: => Action[T]): Option[T]

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
