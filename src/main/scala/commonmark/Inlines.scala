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

import ast._

import org.unbescape.html.HtmlEscape

import scala.annotation.tailrec

trait Inlines {

  // backslash escapes

  private val escapable = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

  def parseBackslash(input: Input, block: Seq[Inline]): Seq[Inline] = {
    input.accept('\\')
    input.peek match {
      case Some('\r') | Some('\n') =>
        // hard break, consume it and following leading spaces
        input.accept("""(\r\n|\r|\n) *""".r)
        block.append(Hardbreak)
      case Some(c) if escapable.contains(c) =>
        input.next()
        block.append(Text(c))
      case _ =>
        block.append(Text('\\'))
    }
  }

  // entities

  private val entity = "(#x[a-f0-9]{1,8}|#[0-9]{1,8}|[a-z][a-z0-9]{1,31});".r

  def parseAmp(input: Input, block: Seq[Inline]): Seq[Inline] = {
    input.accept('&')
    if (input.satisfy(entity)) {
      val raw = input.accept(entity, group = 1)
      val unescaped = HtmlEscape.unescapeHtml(raw)
      if (raw == unescaped) {
        // not an entity
        // escape the '&' and add the rest as is
        block.append(Text(HtmlEscape.escapeHtml5(raw)))
      } else {
        block.append(Text(unescaped))
      }
    } else {
      // just return the raw '&'
      block.append(Text('&'))
    }
  }

  // backticks

  private val backticks = "(?<!`)`+(?!`)".r

  def parseBacktick(input: Input, block: Seq[Inline]): Seq[Inline] = {
    val opener = input.accept(backticks)
    input.consumeTill(f"(?<!`)$opener(?!`)".r, true) match {
      case Some(code) =>
        // collapse internal spaces and append the inline code
        block.append(Code(code.replaceAll("[ \t\r\n\f\u000b]{2,}", " ")))
      case None =>
        // append literal backticks
        block.append(Text(opener))
    }
  }

  // autolinks and XML tags

  private val absoluteUri = """([A-Za-z][A-Za-z0-9.+-]{1,31}:[^<>\x00-\x20]*)>""".r

  private val email = """([a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)>""".r

  private val htmlTag = {
    val tagName = "(?:[a-zA-Z][a-zA-Z0-9-]*)"

    val attributeName = "[a-zA-Z_-][a-zA-T0-9_.:-]"

    val attributeValue = """[^\x00-\x20"'=<>`]+|'[^']*'|"[^"]*""""

    val attributeValueSpec = f"\\s*=\\s*$attributeValue"

    val attribute = f"(?:\\s+$attributeName(?:$attributeValueSpec)?)"

    val openTag = f"$tagName$attribute*\\s*/?>"

    val closingTag = f"/$tagName\\s*>"

    val comment = "!---->|!--(?:-?[^>-])(?:-?[^-])*-->"

    val pi = """\?.*?\?>"""

    val decl = """![A-Z]+\s+[^>]*>"""

    val cdata = """!\[CDATA\[.*?\]\]>"""

    f"$openTag|$closingTag|$comment|$pi|$decl|$cdata".r
  }

  def parseLt(input: Input, block: Seq[Inline]): Seq[Inline] = {
    input.accept('<')
    if (input.satisfy(email)) {
      // this is an email
      val mail = input.accept(email, group = 1)
      block.append(Link(Seq(Text(mail)), f"mailto:$mail", None))
    } else if (input.satisfy(absoluteUri)) {
      // this is an absolute URI
      val uri = input.accept(absoluteUri, group = 1)
      block.append(Link(Seq(Text(uri)), uri, None))
    } else if (input.satisfy(htmlTag)) {
      // this is a raw html tag
      val tag = input.accept(htmlTag)
      block.append(RawHtml(f"<$tag"))
    } else {
      // just a '<'
      block.append(Text("&lt;"))
    }
  }

  // hardbreak and softbreak

  private val lineEnding = "(?:\r\n|\r|\n) *".r

  def parseSpace(input: Input, block: Seq[Inline]): Seq[Inline] = {
    val spaces = input.accept(" +".r)
    val isLineEnd = input.satisfy(lineEnding)
    if (spaces.size >= 2 && isLineEnd) {
      // this is a hardbreak
      input.accept(lineEnding)
      block.append(Hardbreak)
    } else if (isLineEnd) {
      // this is a softbreak
      input.accept(lineEnding)
      block.append(SoftBreak)
    } else {
      // raw spaces
      block.append(Text(spaces))
    }
  }

  def parseNewLine(input: Input, block: Seq[Inline]): Seq[Inline] = {
    input.accept(lineEnding)
    block.append(SoftBreak)
  }

  // link, images and emphasis

  private val unicodeWhitespace = """\p{IsWhite_Space}+"""

  private val unicodePunctuation = """[!"#$%&'()*+,\-./:;<=>?@\[\]\^_`{|}~\p{gc=Pc}\p{gc=Pd}\p{gc=Pe}\p{gc=Pf}\p{gc=Pi}\p{gc=Po}\p{gc=Ps}]"""

  private sealed trait Delimiter {
    def deactivate: Delimiter
    def inlines: Seq[Inline]
  }
  private case class EmphDelimiter(char: Char, length: Int, canOpen: Boolean, canClose: Boolean) extends Delimiter {
    val deactivate = this
    def inlines = Seq(Text(char.toString * length))
  }
  private case class LinkOpener(isImage: Boolean, active: Boolean, inlines: Seq[Inline]) extends Delimiter {
    def deactivate = copy(active = isImage)
  }

  private case class DelimiterStack(delimiter: Delimiter) {
    private var previous: Option[DelimiterStack] = None
    private var next: Option[DelimiterStack] = None

    def append(d: Delimiter): DelimiterStack = {
      val n = DelimiterStack(d)
      next = Some(n)
      n.previous = Some(this)
      n
    }

  }

  def parseBang(input: Input, block: Seq[Inline]): Seq[Inline] = {
    input.accept('!')
    input.peek match {
      case Some('[') =>
        // this is probably an image, try to parse it
        parseDelimiters(input, block, DelimiterStack(LinkOpener(true, true, Seq(Text("![")))))
      case Some(_) | None =>
        // ok, just a bang
        block.append(Text("!"))
    }
  }

  def parseBracket(input: Input, block: Seq[Inline]): Seq[Inline] = {
    input.accept('[')
    parseDelimiters(input, block, DelimiterStack(LinkOpener(false, true, Seq(Text("[")))))
  }

  def parseStar(input: Input, block: Seq[Inline]): Seq[Inline] = {
    val prev = input.previous
    val delim = input.accept("\\*+".r)
    val next = input.peek

    val prevIsWhitespace = prev.map(_.toString.matches(unicodeWhitespace)).getOrElse(false)
    val prevIsPunctuation = prev.map(_.toString.matches(unicodePunctuation)).getOrElse(false)
    val nextIsWhitespace = next.map(_.toString.matches(unicodeWhitespace)).getOrElse(false)
    val nextIsPunctuation = next.map(_.toString.matches(unicodePunctuation)).getOrElse(false)

    val leftFlanking = !nextIsWhitespace && (!nextIsPunctuation || (prevIsWhitespace || prevIsPunctuation))
    val rightFlanking = !prevIsWhitespace && (!prevIsPunctuation || (nextIsWhitespace || nextIsPunctuation))

    val canOpen = leftFlanking
    val canClose = rightFlanking
    parseDelimiters(input, block, DelimiterStack(EmphDelimiter('*', delim.size, canOpen, canClose)))
  }

  def parseUnderscore(input: Input, block: Seq[Inline]): Seq[Inline] = {
    val prev = input.previous
    val delim = input.accept("_+".r)
    val next = input.peek

    val prevIsWhitespace = prev.map(_.toString.matches(unicodeWhitespace)).getOrElse(false)
    val prevIsPunctuation = prev.map(_.toString.matches(unicodePunctuation)).getOrElse(false)
    val nextIsWhitespace = next.map(_.toString.matches(unicodeWhitespace)).getOrElse(false)
    val nextIsPunctuation = next.map(_.toString.matches(unicodePunctuation)).getOrElse(false)

    val leftFlanking = !nextIsWhitespace && (!nextIsPunctuation || (prevIsWhitespace || prevIsPunctuation))
    val rightFlanking = !prevIsWhitespace && (!prevIsPunctuation || (nextIsWhitespace || nextIsPunctuation))

    val canOpen = leftFlanking && (!rightFlanking || prevIsPunctuation)
    val canClose = rightFlanking && (!leftFlanking || nextIsPunctuation)
    parseDelimiters(input, block, DelimiterStack(EmphDelimiter('_', delim.size, canOpen, canClose)))
  }

  def parseDelimiters(input: Input, block: Seq[Inline], stack: DelimiterStack): Seq[Inline] = {
    input.peek match {
      case Some(']') =>
        lookForLinkOrImage(input: Input, block: Seq[Inline], stack: DelimiterStack)
    }
  }

  def lookForLinkOrImage(input: Input, block: Seq[Inline], stack: DelimiterStack): Seq[Inline] = {

    ???
  }

  // entry point

  private val text = "[^ \r\n`\\[\\!<&*_]+".r

  def parseInline(input: Input, block: Seq[Inline]): Option[Seq[Inline]] =
    input.peek.map {
      case '\\' =>
        parseBackslash(input, block)
      case '&' =>
        parseAmp(input, block)
      case '`' =>
        parseBacktick(input, block)
      case '<' =>
        parseLt(input, block)
      case ' ' =>
        parseSpace(input, block)
      case '\r' | '\n' =>
        parseNewLine(input, block)
      case '!' =>
        parseBang(input, block)
      case '[' =>
        parseBracket(input, block)
      case '*' =>
        parseStar(input, block)
      case '_' =>
        parseUnderscore(input, block)
      case _ =>
        // consume normal text
        val txt = input.accept(text)
        block.append(Text(txt))
    }

  def parseInlines(input: Input, block: Seq[Inline]): Seq[Inline] = {
    @tailrec
    def loop(block: Seq[Inline]): Seq[Inline] =
      parseInline(input, block) match {
        case Some(block) => loop(block)
        case None        => block
      }
    loop(block)
  }

}
