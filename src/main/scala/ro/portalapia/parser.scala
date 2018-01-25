package ro.portalapia

import fastparse.all
import fastparse.all.{AnyChar, _}
import fastparse.core.{Logger, Parsed, Parser}

import scala.collection.mutable

object parser {

//  val logged = mutable.Buffer.empty[String]
//  implicit val logger = Logger(logged.append(_))

  case class T(hs: Seq[H], skipToValues: Int)

  object T {
    def apply(skipToValues: Int, hs: H*): T = T(hs, skipToValues)
  }

  case class H(name: String, cond: (String) => Boolean = _.toDouble > 0, coef: Double = 0.00, kg: Double = 0, repExactly: Int = 1, isLast: Boolean = false, skip: Boolean = false) {
    lazy val sName = sourcecode.Name(name)
  }

  case class Col(h: H, v: Option[String] = None) {
    def valueAsText: String = v.getOrElse(0).toString
  }

  private lazy val anyChar = P(AnyChar)(sourcecode.Name(""))
  private lazy val notBracket: all.Parser[Unit] = P(CharsWhile(_ != '['))(sourcecode.Name("notBracket"))
  private lazy val pAlphaNum: Parser[String, Char, String] = P(CharIn('0' to '9', Seq('\u0000', ' '), 'a' to 'z', 'A' to 'Z').rep(1).!.map(_.replace("\u0000", "")))(sourcecode.Name("alpha-num"))

  def pTables(bytes: Array[Byte], ts: Seq[T], pdfContent: String): Either[String, (Array[Byte], Seq[Col])] = {
    val cols = mutable.ListBuffer[Col]()
    val first: Either[String, Int] = pTable(ts.head, cols).parse(pdfContent).fold(onFailure, onSuccess)
    val r = ts.tail.foldLeft(first) { (sOrF, t) =>
      sOrF match {
        case Right(index) =>
          pTable(t, cols).parse(pdfContent, index + 1).fold(onFailure, onSuccess)
        case l@_ => l
      }
    }
//    println(logged.mkString("\n"))
    r.map(_ => (bytes, cols.toList))
  }

  private def pTable(t: T, cols: mutable.ListBuffer[Col]): Parser[Any, Char, String] = {
    (notBracket ~ ("[]" | (pHeaders(t.hs) ~ pValues(t, cols)) | anyChar)).rep
  }

  private def pHeaders(headers: Seq[parser.H]): Parser[Unit, Char, String] = {
    headers.tail.foldLeft(pHeader(headers.head))((p, h) => p ~ pHeader(h))
  }

  private def pHeader(h: H): Parser[Unit, Char, String] = {
    (h.name.foldLeft(P("[(")(h.sName))((p, c) => p ~ anyChar ~ c.toString) ~ (0 until h.repExactly).foldLeft(notBracket)((p, _) => p ~ "[]" ~ notBracket))
//      .log()
  }

  private def pValues(t: T, cols: mutable.ListBuffer[Col]): Parser[Any, Char, String] = {
    (("[]" | "[(") ~ notBracket).rep(exactly = t.skipToValues) ~
    t.hs.foldLeft(P[Any]("[")(sourcecode.Name("Values")))((p, h) =>
      if(h.skip)
        p
      else
      p ~ (((("[(" | "(") ~ anyChar ~ pAlphaNum) ~ skipLines(h.isLast, h.repExactly)) | ("[]" | "]") ~ skipLines(h.isLast, h.repExactly - 1)).map {
      case v: String =>
        cols += Col(h, Some(v))
      case _ =>
        cols += Col(h, None)
    })
//      .log()
  }

  private def skipLines(isLast: Boolean, repExactly: Int) = {
    if (isLast)
      notBracket
    else
      (0 until repExactly).foldLeft(notBracket)((p, _) => p ~ "[]" ~ notBracket)
  }

  private def onFailure(p: Parser[_, Char, String], index: Int, log: Parsed.Failure.Extra[Char, String]): Either[String, Int] = {
    Left(log.traced.fullStack.mkString("\n"))
  }

  private def onSuccess: (Any, Int) => Either[String, Int] = (_, index) => Right(index)

  def pAlfalfa(pdfContent: String): Either[String, Double] = {
    var total = 0.0
    val p1 = "[(\u00009\u00007\u00004\u00008)]"
    val p2 = "[(\u00009\u00007\u00004\u00007)]"
    val emptyCell = (notBracket ~ "[]").rep(exactly = 2)
    val double = notBracket ~ "[(" ~ CharIn('0' to '9', "\u0000,").rep(1).!.map(s => total += s.replace("\u0000", "").replace(",", ".").toDouble)
    val r = P(notBracket ~ ("[]" | ((p1 | p2) ~ emptyCell ~ double)) | anyChar).rep
      .parse(pdfContent)
      .fold((_, _, log) => Left(log.traced.fullStack.mkString("\n")), (_, _) => Right(total))
//    println(logged.mkString("\n"))
    r
  }
}
