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

import java.net.{
  URLEncoder,
  URLDecoder
}
import java.text.Normalizer
import java.util.Locale

import scala.annotation.tailrec

trait Inlines {

  // backslash escapes

  private val escapable = """[!"#$%&'()*+,\-./:;<=>?@\[\]\^_`{|}~]"""

  private val escaped = escapable.r

  private def parseBackslash(input: StringScanner, block: Seq[Inline]): Seq[Inline] = {
    input.accept('\\')
    input.peek match {
      case Some('\r') | Some('\n') =>
        // hard break, consume it and following leading spaces
        input.accept("""(\r\n|\r|\n) *""".r)
        block :+ Hardbreak
      case Some(c) if input.satisfy(escaped) =>
        val esc = input.accept(escaped)
        block :+ Text(esc)
      case _ =>
        block :+ Text("\\")
    }
  }

  // entities

  private val entity = "&(?:#x[a-f0-9]{1,8}|#[0-9]{1,8}|[a-z][a-z0-9]{1,31});".r

  private def parseAmp(input: StringScanner, block: Seq[Inline]): Seq[Inline] = {
    if (input.satisfy(entity)) {
      val raw = input.accept(entity)
      val unescaped = HtmlEscape.unescapeHtml(raw)
      if (raw == unescaped) {
        // not an entity
        // escape the '&' and add the rest as is
        block :+ Text(HtmlEscape.escapeHtml5(raw))
      } else {
        block :+ Text(unescaped)
      }
    } else {
      // just return the raw '&'
      block :+ Text('&')
    }
  }

  // backticks

  private val backticks = "(?<!`)`+(?!`)".r

  private def parseBacktick(input: StringScanner, block: Seq[Inline]): Seq[Inline] = {
    val opener = input.accept(backticks)
    input.consumeTill(f"(?<!`)$opener(?!`)".r) match {
      case Some(code) =>
        // collapse internal spaces and append the inline code
        block :+ Code(code.replaceAll("[ \t\r\n\f\u000b]{2,}", " "))
      case None =>
        // append literal backticks
        block :+ Text(opener)
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

  private def parseLt(input: StringScanner, block: Seq[Inline]): Seq[Inline] = {
    input.accept('<')
    if (input.satisfy(email)) {
      // this is an email
      val mail = input.accept(email, group = 1)
      block :+ Link(Seq(Text(mail)), f"mailto:$mail", None)
    } else if (input.satisfy(absoluteUri)) {
      // this is an absolute URI
      val uri = input.accept(absoluteUri, group = 1)
      block :+ Link(Seq(Text(uri)), uri, None)
    } else if (input.satisfy(htmlTag)) {
      // this is a raw html tag
      val tag = input.accept(htmlTag)
      block :+ RawHtml(f"<$tag")
    } else {
      // just a '<'
      block :+ Text("&lt;")
    }
  }

  // hardbreak and softbreak

  private val lineEnding = "(?:\r\n|\r|\n) *".r

  private def parseSpace(input: StringScanner, block: Seq[Inline]): Seq[Inline] = {
    val spaces = input.accept(" +".r)
    val isLineEnd = input.satisfy(lineEnding)
    if (spaces.size >= 2 && isLineEnd) {
      // this is a hardbreak
      input.accept(lineEnding)
      block :+ Hardbreak
    } else if (isLineEnd) {
      // this is a softbreak
      input.accept(lineEnding)
      block :+ SoftBreak
    } else {
      // raw spaces
      block :+ Text(spaces)
    }
  }

  private def parseNewLine(input: StringScanner, block: Seq[Inline]): Seq[Inline] = {
    input.accept(lineEnding)
    block :+ SoftBreak
  }

  // link, images and emphasis

  private def encodeUri(input: String): String =
    URLEncoder.encode(input, "UTF-8")
      .replaceAll("\\+", "%20")
      .replaceAll("%21", "!")
      .replaceAll("%27", "'")
      .replaceAll("%28", "(")
      .replaceAll("%29", ")")
      .replaceAll("%7E", "~")

  private def decodeUri(input: String): String =
    URLDecoder.decode(input.replaceAll("%20", "\\+")
      .replaceAll("!", "%21")
      .replaceAll("'", "%27")
      .replaceAll("(", "%28")
      .replaceAll(")", "%29")
      .replaceAll("~", "%7E"), "UTF-8")

  private val unicodeWhitespace = """\p{IsWhite_Space}+"""

  private val unicodePunctuation = """[!"#$%&'()*+,\-./:;<=>?@\[\]\^_`{|}~\p{gc=Pc}\p{gc=Pd}\p{gc=Pe}\p{gc=Pf}\p{gc=Pi}\p{gc=Po}\p{gc=Ps}]"""

  private val linkLabel = f"^\\[((?:[^\\\\\\[\\]]|\\\\$escapable|\\\\){999})\\]".r

  private def normalizeLinkLabel(label: String): String =
    Normalizer.normalize(label, Normalizer.Form.NFC).toLowerCase(Locale.ENGLISH).replaceAll("\\s{2,}", " ")

  private sealed abstract class Delimiter {

    var previous: Option[Delimiter] = None
    var next: Option[Delimiter] = None

    def findLinkOpener: Option[LinkOpener]

    def findPotentialCloserFrom: Option[EmphDelim]

    // deactivate all the link openers before this node (inclusive) in the stack
    def deactivateMeAndPrevious: Unit = {
      deactivate
      previous.foreach(_.deactivateMeAndPrevious)
    }

    // deactivate all the link openers before this node (exclusive) in the stack
    def deactivatePrevious: Unit =
      previous.foreach(_.deactivateMeAndPrevious)

    var node: Text
    def deactivate: Unit
  }

  private case class EmphDelim(char: Char, length: Int, canOpen: Boolean, canClose: Boolean, var node: Text) extends Delimiter {
    def findLinkOpener =
      previous.flatMap(_.findLinkOpener)
    def findPotentialCloserFrom =
      if (canClose) Some(this) else next.flatMap(_.findPotentialCloserFrom)
    def findPotentialOpenerFrom(bottom: Option[Delimiter]): Option[EmphDelim] = {
      def loop(d: Delimiter): Option[EmphDelim] =
        d match {
          case e @ EmphDelim(`char`, olength, true, ocanClose, _) if (!ocanClose && !canOpen) || olength + length % 3 != 0 => Some(e)
          case _ => if (Some(d) == bottom) None else d.previous.flatMap(loop)
        }
      previous.flatMap(loop)
    }
    val deactivate = ()
  }
  private case class LinkOpener(isImage: Boolean, var active: Boolean, var node: Text) extends Delimiter {
    val findLinkOpener = Some(this)
    def findPotentialCloserFrom = next.flatMap(_.findPotentialCloserFrom)

    def deactivate =
      active = isImage
  }

  private class DelimiterStack {

    private var _top: Option[Delimiter] = None
    private var _bottom: Option[Delimiter] = None

    def top = _top
    def bottom = _bottom

    def push(d: Delimiter): Unit =
      _top match {
        case None =>
          // stack is empty
          val sd = Some(d)
          _top = sd
          _bottom = sd
        case Some(t) =>
          val sd = Some(d)
          t.next = sd
          d.previous = _top
          _top = sd
      }

    def remove(node: Delimiter): Unit =
      (node.previous, node.next) match {
        case (sp @ Some(p), sn @ Some(n)) =>
          // in the middle of the stack
          p.next = sn
          n.previous = sp
        case (sp @ Some(p), None) =>
          // on the top
          p.next = None
          _top = sp
        case (None, sn @ Some(n)) =>
          // in the bottom
          n.previous = None
          _bottom = sn
        case (None, None) =>
          // single element, empty everythin
          _top = None
          _bottom = None
      }

    def removeBetween(top: Delimiter, bottom: Delimiter): Unit = {
      bottom.next = Some(top)
      top.previous = Some(bottom)
    }

    def removeAbove(node: Delimiter): Unit = {
      _top = Some(node)
      node.next = None
    }

    def clear(): Unit = {
      _top = None
      _bottom = None
    }

  }

  private def parseBang(input: StringScanner, block: Seq[Inline], stack: DelimiterStack): Seq[Inline] = {
    input.accept('!')
    input.peek match {
      case Some('[') =>
        // this is probably an image, try to parse it
        val node = Text("![")
        stack.push(LinkOpener(true, true, node))
        block :+ node
      case Some(_) | None =>
        // ok, just a bang
        block :+ Text("!")
    }
  }

  private def parseOpeningBracket(input: StringScanner, block: Seq[Inline], stack: DelimiterStack): Seq[Inline] = {
    input.accept('[')
    val node = Text("[")
    stack.push(LinkOpener(false, true, node))
    block :+ node
  }

  private def parseStar(input: StringScanner, block: Seq[Inline], stack: DelimiterStack): Seq[Inline] = {
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

    val node = Text(delim)
    stack.push(EmphDelim('*', delim.size, canOpen, canClose, node))
    block :+ node
  }

  private def parseUnderscore(input: StringScanner, block: Seq[Inline], stack: DelimiterStack): Seq[Inline] = {
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

    val node = Text(delim)
    stack.push(EmphDelim('_', delim.size, canOpen, canClose, node))
    block :+ node
  }

  private def lookForLinkOrImage(input: StringScanner, references: Map[String, LinkReferenceDefinition], block: Seq[Inline], stack: DelimiterStack): Seq[Inline] = {
    stack.top.flatMap(_.findLinkOpener) match {
      case None =>
        // link opener not found, return the raw "]" as text
        block :+ Text("]")
      case Some(node @ LinkOpener(_, false, _)) =>
        // deactivated link opener found
        // remove it from the stack
        stack.remove(node)
        block :+ Text("]")
      case Some(node @ LinkOpener(isImage, true, _)) =>
        parseLinkOrImage(input, references, block, isImage, node, stack)
      case Some(_) =>
        // error should never happen
        throw new Exception("THIS IS A BUG! Please report with stack trace")
    }
  }

  private def parseLinkOrImage(input: StringScanner, references: Map[String, LinkReferenceDefinition], block: Seq[Inline], isImage: Boolean, opener: Delimiter, stack: DelimiterStack): Seq[Inline] =
    input.peek match {
      case Some('(') =>
        // try to parse an inline link or image
        val inlined =
          input.buffered {
            // consume the opening parenthesis
            input.next()
            parseOptionalWhitespace(input)
            val linkOrImage =
              for {
                dest <- parseLinkDestination(input)
                () = parseOptionalWhitespace(input)
                title <- parseOptionalTitle(input)
                () = parseOptionalWhitespace(input)
                ')' <- input.peek
              } yield {
                input.next()
                // create the link with all inlines appearing after the opener
                val (block1, inlines) = block.span(_ ne opener.node)
                // head of inlines is the opener, drop it
                val inlines1 = inlines.tail
                // apply processEmphasis on these inlines
                val inlines2 = processEmphasis(inlines1, Some(opener), stack)
                // if this is not an image deactivate previous to avoid link in link
                val link =
                  if (isImage) {
                    Image(inlines2, dest, title)
                  } else {
                    opener.deactivatePrevious
                    Link(inlines2, dest, title)
                  }
                block1 :+ link
              }
            Action(linkOrImage)
          }
        inlined match {
          case Some(block) =>
            block
          case None =>
            input.next()
            // try a shortcut reference
            val (block1, inlines) = block.span(_ ne opener.node)
            val inlines1 = inlines.tail
            val label = normalizeLinkLabel(inlines1.map(_.toText).mkString)
            references.get(label) match {
              case Some(LinkReferenceDefinition(_, dest, title)) =>
                val inlines2 = processEmphasis(inlines1, Some(opener), stack)
                val link =
                  if (isImage) {
                    Image(inlines2, dest, title)
                  } else {
                    opener.deactivatePrevious
                    Link(inlines2, dest, title)
                  }
                block1 :+ link
              case None =>
                stack.remove(opener)
                block :+ Text("]")
            }
        }
      case Some('[') =>
        // try to parse a reference link or image
        val reference =
          input.buffered {
            val label = normalizeLinkLabel(input.accept(linkLabel, group = 1))
            if (label.isEmpty) {
              // collapsed
              val (block1, inlines) = block.span(_ ne opener.node)
              val inlines1 = inlines.tail
              val label1 = normalizeLinkLabel(inlines1.map(_.toText).mkString)
              references.get(label1) match {
                case Some(LinkReferenceDefinition(_, dest, title)) =>
                  val inlines2 = processEmphasis(inlines1, Some(opener), stack)
                  val link =
                    if (isImage) {
                      Image(inlines2, dest, title)
                    } else {
                      opener.deactivatePrevious
                      Link(inlines2, dest, title)
                    }
                  Commit(block1 :+ link)
                case None =>
                  Rollback
              }
            } else {
              // full
              references.get(label) match {
                case Some(LinkReferenceDefinition(_, dest, title)) =>
                  val (block1, inlines) = block.span(_ ne opener.node)
                  val inlines1 = inlines.tail
                  val inlines2 = processEmphasis(inlines1, Some(opener), stack)
                  val link =
                    if (isImage) {
                      Image(inlines2, dest, title)
                    } else {
                      opener.deactivatePrevious
                      Link(inlines2, dest, title)
                    }
                  Commit(block1 :+ link)
                case None =>
                  Rollback
              }
            }
          }
        reference match {
          case Some(block) =>
            block
          case None =>
            stack.remove(opener)
            block :+ Text("]")
        }
      case _ =>
        // remove delimiter from stack and return raw "]"
        stack.remove(opener)
        block :+ Text("]")
    }

  private def processEmphasis(inlines: Seq[Inline], bottom: Option[Delimiter], stack: DelimiterStack): Seq[Inline] =
    bottom.map(_.next).getOrElse(stack.bottom) match {
      case Some(currentPosition) =>
        def loop(inlines: Seq[Inline], currentPosition: Delimiter, starOpenerBottom: Option[Delimiter], underscoreOpenerBottom: Option[Delimiter]): Seq[Inline] = currentPosition.findPotentialCloserFrom match {
          case Some(closer) =>
            val bot = if (closer.char == '*') starOpenerBottom else underscoreOpenerBottom
            closer.findPotentialOpenerFrom(bot) match {
              case Some(opener) =>
                val strong = closer.length >= 2 && opener.length >= 2
                // take all between opener and closer in the inlines and emphasize them
                val (beforeOpener, afterOpener) = inlines.span(_ ne opener.node)
                val (beforeCloser, afterCloser) = afterOpener.span(_ ne closer.node)
                (beforeCloser, afterCloser) match {
                  case (Seq(_, content @ _*), Seq(_, rest @ _*)) =>
                    if (strong) {
                      // remove two characters from opener and closer
                      opener.node = Text(opener.node.value.substring(2))
                      closer.node = Text(opener.node.value.substring(2))
                    } else {
                      // remove one character from opener and closer
                      opener.node = Text(opener.node.value.substring(1))
                      closer.node = Text(opener.node.value.substring(1))
                    }
                    // remove delimiters between open and closer in the stack
                    stack.removeBetween(closer, opener)
                    (opener.node.value.size, closer.node.value.size) match {
                      case (0, 0) =>
                        // remove both from inlines they are empty
                        val currentPosition1 = closer.next
                        currentPosition1 match {
                          case Some(pos) =>
                            loop(beforeOpener ++ (Emphasis(strong, content) +: rest), pos, starOpenerBottom, underscoreOpenerBottom)
                          case None =>
                            // remove all delimiters above bottom in the stack
                            bottom match {
                              case Some(b) => stack.removeAbove(b)
                              case None    => stack.clear
                            }
                            beforeOpener ++ (Emphasis(strong, content) +: rest)
                        }
                      case (0, _) =>
                        // remove opener
                        loop(beforeOpener ++ (Emphasis(strong, content) +: closer.node +: rest), currentPosition, starOpenerBottom, underscoreOpenerBottom)
                      case (_, 0) =>
                        // remove closer from inlines they are empty
                        val currentPosition1 = closer.next
                        currentPosition1 match {
                          case Some(pos) =>
                            loop(beforeOpener ++ (opener.node +: Emphasis(strong, content) +: rest), pos, starOpenerBottom, underscoreOpenerBottom)
                          case None =>
                            // remove all delimiters above bottom in the stack
                            bottom match {
                              case Some(b) => stack.removeAbove(b)
                              case None    => stack.clear
                            }
                            beforeOpener ++ (opener.node +: Emphasis(strong, content) +: rest)
                        }
                      case (_, _) =>
                        // keep both opener and closer
                        loop(beforeOpener ++ (opener.node +: Emphasis(strong, content) +: closer.node +: rest), currentPosition, starOpenerBottom, underscoreOpenerBottom)
                    }
                  case (_, _) =>
                    throw new Exception("THIS IS A BUG! Please report with a stack trace")
                }
              case None =>
                if (!closer.canOpen)
                  stack.remove(closer)
                currentPosition.next match {
                  case Some(pos) =>
                    if (closer.char == '*')
                      loop(inlines, pos, currentPosition.previous, underscoreOpenerBottom)
                    else
                      loop(inlines, pos, starOpenerBottom, currentPosition.previous)
                  case None =>
                    // done
                    // remove all delimiters above bottom in the stack
                    bottom match {
                      case Some(b) => stack.removeAbove(b)
                      case None    => stack.clear
                    }
                    inlines
                }
            }
          case None =>
            // done
            // remove all delimiters above bottom in the stack
            bottom match {
              case Some(b) => stack.removeAbove(b)
              case None    => stack.clear
            }
            inlines
        }
        loop(inlines, currentPosition, bottom, bottom)
      case None =>
        // stack is empty, return inlines as is
        inlines
    }

  private val whitespaceChar = " \t\r\n\f\u000b"
  private val optionalWhitespace = f"(?:[$whitespaceChar])*".r

  private def parseOptionalWhitespace(input: StringScanner): Unit =
    input.accept(optionalWhitespace)

  private val linkDestinationBraces = f"^(?:[<]([^ <>\t\n\\\\\\x00]|\\\\$escapable|\\\\)*[>])".r

  private def parseLinkDestination(input: StringScanner): Option[String] =
    if (input.satisfy(linkDestinationBraces)) {
      val url = input.accept(linkDestinationBraces, group = 1)
      Some(encodeUri(decodeUri(url)))
    } else {
      @tailrec
      def loop(openedParen: Int, acc: StringBuilder): Option[String] =
        input.peek match {
          case None =>
            None
          case Some('\\') =>
            acc.append(input.next())
            if (input.hasNext)
              acc.append(input.next())
            loop(openedParen, acc)
          case Some('(') =>
            acc.append(input.next())
            loop(openedParen + 1, acc)
          case Some(')') =>
            if (openedParen < 1) {
              // this is the end
              Some(acc.toString)
            } else {
              loop(openedParen - 1, acc.append(input.next()))
            }
          case Some(c) if whitespaceChar.contains(c) =>
            Some(acc.toString)
          case Some(_) =>
            loop(openedParen, acc.append(input.next()))
        }
      loop(0, new StringBuilder).map(url => encodeUri(decodeUri(url)))
    }

  private val titleDoubleQuotes = f""""(\\\\$escapable|[^"\\x00])*"""".r
  private val titleSimpleQuotes = f"""'(\\\\$escapable|[^'\\x00])*'""".r
  private val titleParentheses = f"""\\((\\\\$escapable|[^)\\x00])*\\)""".r

  private def parseOptionalTitle(input: StringScanner): Option[Option[String]] =
    input.peek match {
      case Some('"') =>
        Some(Some(input.accept(titleDoubleQuotes, group = 1)))
      case Some('\'') =>
        Some(Some(input.accept(titleSimpleQuotes, group = 1)))
      case Some('(') =>
        Some(Some(input.accept(titleParentheses, group = 1)))
      case Some(_) =>
        Some(None)
      case None =>
        None
    }

  // entry point

  private val text = "[^ \r\n`\\[\\]\\!<&*_]+".r

  private def parseInline(input: StringScanner, references: Map[String, LinkReferenceDefinition], block: Seq[Inline], stack: DelimiterStack): Option[Seq[Inline]] =
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
        parseBang(input, block, stack)
      case '[' =>
        parseOpeningBracket(input, block, stack)
      case ']' =>
        lookForLinkOrImage(input, references, block, stack)
      case '*' =>
        parseStar(input, block, stack)
      case '_' =>
        parseUnderscore(input, block, stack)
      case _ =>
        // consume normal text
        val txt = input.accept(text)
        block :+ Text(txt)
    }

  def parseInlines(input: StringScanner, references: Map[String, LinkReferenceDefinition]): Seq[Inline] = {
    @tailrec
    def loop(block: Seq[Inline], stack: DelimiterStack): Seq[Inline] =
      parseInline(input, references, block, stack) match {
        case Some(block) => loop(block, stack)
        case None        => block
      }
    loop(Seq.empty[Inline], new DelimiterStack)
  }

}
