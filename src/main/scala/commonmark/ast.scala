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
package commonmark.ast

import java.net.URI

final case class Doc(blocks: Seq[Block])

/** @see [[http://spec.commonmark.org/0.27/#leaf-blocks]]
 *  @see [[http://spec.commonmark.org/0.27/#container-blocks]]
 */
sealed trait Block

/** @group LeafBlock
 *  @see [[http://spec.commonmark.org/0.27/#thematic-breaks]]
 */
case object ThematicBreak extends Block

/** @group LeafBlock
 *  @see [[http://spec.commonmark.org/0.27/#atx-headings]]
 *  @see [[http://spec.commonmark.org/0.27/#setext-headings]]
 */
final case class Heading(level: HeadingLevel.Value, value: Seq[Inline]) extends Block

object HeadingLevel extends Enumeration {
  val One, Two, Three, Four, Five, Six = Value
}

/** @group LeafBlock
 *  @see [[http://spec.commonmark.org/0.27/#indented-code-blocks]]
 *  @see [[http://spec.commonmark.org/0.27/#code-fences]]
 */
final case class CodeBlock(language: Option[String], content: String) extends Block

/** @group LeafBlock
 *  @see [[http://spec.commonmark.org/0.27/#html-blocks]]
 */
final case class HtmlBlock(content: String) extends Block

/** @group LeafBlock
 *  @see [[http://spec.commonmark.org/0.27/#link-reference-definitions]]
 */
final case class LinkReferenceDefinition(label: String, destination: URI, title: Option[String]) extends Block

/** @group LeafBlock
 *  @see [[http://spec.commonmark.org/0.27/#paragraphs]]
 */
final case class Paragraph(content: Seq[Inline]) extends Block

/** @group ContainerBlock
 *  @see [[http://spec.commonmark.org/0.27/#block-quotes]]
 */
final case class Quote(blocks: Seq[Block]) extends Block

/** @group ContainerBlock
 *  @see [[http://spec.commonmark.org/0.27/#list-marker]]
 *  @see [[http://spec.commonmark.org/0.27/#list]]
 */
final case class Itemize(tpe: ListType, tight: Boolean, items: Seq[Seq[Block]]) extends Block

sealed trait ListType

final case class Bullet(marker: BulletMarker.Value) extends ListType

object BulletMarker extends Enumeration {
  val Minus, Plus, Star = Value
}

final case class Ordered(start: Int, delimiter: Delimiter.Value) extends ListType

object Delimiter extends Enumeration {
  val Dot, Paren = Value
}

/** @see [[http://spec.commonmark.org/0.27/#inlines]] */
sealed trait Inline {
  /** Returns the raw text of this inline without structure. */
  def toText: String
}

/** @see [[http://spec.commonmark.org/0.27/#code-spans]] */
final case class Code(value: String) extends Inline {
  val toText = value
}

/** @see [[http://spec.commonmark.org/0.27/#emphasis-and-strong-emphasis]] */
final case class Emphasis(strong: Boolean, content: Seq[Inline]) extends Inline {
  def toText = content.foldLeft(new StringBuilder)((acc, in) => acc.append(in.toText)).toString
}

/** @see [[http://spec.commonmark.org/0.27/#links]]
 *  @see [[http://spec.commonmark.org/0.27/#autolinks]]
 */
final case class Link(text: Seq[Inline], destination: URI, title: Option[String]) extends Inline {
  def toText = text.foldLeft(new StringBuilder)((acc, in) => acc.append(in.toText)).toString
}

/** @see [[http://spec.commonmark.org/0.27/#images]] */
final case class Image(description: Seq[Inline], destination: URI, title: Option[String]) extends Inline {
  def toText = description.foldLeft(new StringBuilder)((acc, in) => acc.append(in.toText)).toString
}

/** @see [[http://spec.commonmark.org/0.27/#raw-html]] */
final case class RawHtml(content: String) extends Inline {
  val toText = content
}

/** @see [[http://spec.commonmark.org/0.27/#hard-line-breaks]] */
case object Hardbreak extends Inline {
  val toText = "\n"
}

/** @see [[http://spec.commonmark.org/0.27/#soft-line-breaks]] */
case object SoftBreak extends Inline {
  val toText = " "
}

/** @see [[http://spec.commonmark.org/0.27/#textual-content]] */
final case class Text(value: String) extends Inline {
  val toText = value
}

object Text {
  def apply(c: Char): Text = Text(c.toString)
}
