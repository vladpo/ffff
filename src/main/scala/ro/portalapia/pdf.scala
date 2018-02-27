package ro.portalapia

import java.io.{ByteArrayOutputStream, File, FileOutputStream}
import java.util.Date

import cats.effect.IO
import com.itextpdf.io.source.RandomAccessSourceFactory
import com.itextpdf.kernel.font.{PdfFont, PdfFontFactory}
import com.itextpdf.kernel.geom.PageSize.A4
import com.itextpdf.kernel.geom.Rectangle
import com.itextpdf.kernel.pdf._
import com.itextpdf.layout.{Canvas, Document}
import com.itextpdf.layout.element._
import com.itextpdf.layout.property.TabAlignment.{CENTER => TAB_CENTER}
import com.itextpdf.layout.property.TabAlignment.{LEFT => TAB_LEFT}
import com.itextpdf.layout.property.TextAlignment.{CENTER => TEXT_CENTER, RIGHT => TEXT_RIGHT}
import ro.portalapia.parser.{Col, H, T, pAlfalfa, pTables}
import com.github.nscala_time.time.Imports._
import monocle.Lens
import monocle.macros.GenLens

object pdf {

  case class Farmer(id: String, sn: Option[String] = None, n: Option[String] = None, company: Option[String] = None, county: Option[String] = None, town: Option[String] = None, village: Option[String] = None, street: Option[String] = None, streetNb: Option[String] = None) {
    override def toString: String = {
      def spaced(ms: Option[String]): String = ms.map(_ + " ").getOrElse("")
      fullName() + " " + id + "\n" + spaced(county) + spaced(town) + spaced(village) + spaced(street) + spaced(streetNb)
    }
    def fullName(capitalize: Boolean = false, sep: String = " "): String = {
      def capSplit(s: String): String = s.split(" ").map(_.toLowerCase.capitalize).mkString(" ")
      val fullName = if(capitalize)
        sn.fold(n.fold("")(sep + capSplit(_)))(sep + capSplit(_) + n.fold("")(sep + capSplit(_))).replace(" ", sep)
      else
        sn.fold(n.fold("")(sep + _))(sep + _ + n.fold("")(sep + _)).replace(" ", sep)
      if (fullName.isEmpty)
        sep + company.getOrElse("").replace(" ", sep)
      else
        fullName
    }
  }

  case class M(name: String, days: Int, fedFactor: Int = 1, alfalfaAvgPerHectar: Float = 0.0f, year: Int = DateTime.now.year.get) {
    def isApril(): Boolean = name == "Apr."

  }

  case class Consumption(perMonth: Map[String, Double], hectares: Double, totalAlfalfa: Double, remainingAlfalfa: Double, storedAlfalfa: Double)

  object Consumption {
    def perMonth: Lens[Consumption, Map[String, Double]] = GenLens[Consumption](_.perMonth)

    def alfalfa: Lens[Consumption, Double] = GenLens[Consumption](_.remainingAlfalfa)
  }

  private val nameSurnameLabel = "Numele \u015fi prenumele"
  private val signLabel = "Semn\u0103tura"

  private lazy val months: Vector[M] = Vector(
    M("Mai", daysOfMonth(5), fedFactor = 0, alfalfaAvgPerHectar = 2.0f, year = prevYear), M("Iun.", daysOfMonth(6), fedFactor = 0, year = prevYear), M("Iul.", daysOfMonth(7), fedFactor = 0, alfalfaAvgPerHectar = 1.2f, year = prevYear),
    M("Aug.", daysOfMonth(8), fedFactor = 0, year = prevYear), M("Sep.", daysOfMonth(9), fedFactor = 0, alfalfaAvgPerHectar = 0.8f, year = prevYear), M("Oct.", daysOfMonth(10), fedFactor = 0, year = prevYear),
    M("Nov.", daysOfMonth(11), year = prevYear), M("Dec.", daysOfMonth(12), year = prevYear), M("Ian.", daysOfMonth(1)), M("Feb.", daysOfMonth(2)), M("Mar.", daysOfMonth(3)),
    M("Apr.", daysOfMonth(4), fedFactor = 0))

  private lazy val prevYear = DateTime.now.year.get - 1

  private lazy val computeMonths: Vector[M] = {
    if (4 < DateTime.now.month.get) {
      months.dropRight(13 + 4 - DateTime.now.month.get) :+ months(DateTime.now.month.get - 5).copy(days = DateTime.now.day.get)
    } else {
      months.dropRight(5 - DateTime.now.month.get) :+ months(11 - 4 + DateTime.now.month.get).copy(days = DateTime.now.day.get)
    }
  }

  private lazy val legend = Seq("*) Calculul efectivului mediu furajat din ferm\u0103 se completeaz\u0103 lunar \u015fi anual pe fiecare specie \u015fi categorie de animale.",
    "\n**) (col. 5) efectiv la sf\u00e2r\u015fitul perioadei reprezint\u0103 \u015fi efectiv la \u00eenceputul lunii/perioadei urm\u0103toare",
    "\n(col. 6) Zile animale furajate lunar = (col. 2 X nr. zile lun\u0103) + (col. 3. X 0,5 X nr. zile lun\u0103) - (col. 4 X 0,5 X nr. zile lun\u0103)",
    "\n(col. 6) Zile animale furajate an/total perioad\u0103 = (col. 2 X 365 sau nr. zile perioad\u0103) + (col. 3 X 0,5 X 365 sau nr. zile perioad\u0103) -",
    "\n(col. 4 X 0,5 X 365 sau nr. zile perioad\u0103)",
    "\n(col. 7) Efectivul mediu furajat lunar = (col. 6) \u00f7 nr. zile lun\u0103",
    "\n(col. 7) Efectivul mediu furajat an/total perioad\u0103 = (col. 6) \u00f7 365 zile (sau nr. zile perioad\u0103).",
    "\n(col. 8) = col. (7) X coef. de transformare \u00een UVM al categoriei de animale respective din anexa nr. 5 la Ordinul nr. 619/2015, cu modificarile \u015fi complet\u0103rile ulterioare",
    "\n(col. 9) consum lucern\u0103/soia/f\u00e2n lunar = (col. 8) X cantitatea de lucern\u0103/soia consumat\u0103 pe UVM stabilit\u0103 prin ra\u0163ie de c\u0103tre fermier X nr. zile lun\u0103",
    "\n(col. 9) consum lucern\u0103/soia/f\u00e2n an/total perioad\u0103 = (col. 8) X cantitatea de lucern\u0103/soia consumat\u0103 pe UVM stabilit\u0103 prin ra\u0163ie de",
    "\nc\u0103tre fermier X 365 zile (sau nr. zile perioad\u0103). Cantitatea calculat\u0103 nu poate dep\u0103\u015fi cantitatea maxim\u0103/UVM/an prev\u0103zut\u0103 \u00een anexa nr. 5 la Ordinul nr. 619/2015.")

  private val ignore: (String) => Boolean = _ => false
  private val SURNAME = "01. Nume"
  private val NAME = "02. Prenume"
  private val COMPANY = "07. Denumire exploata"
  private val COUNTY = "15. Jude"
  private val LOCALITY = "16. Localitate"
  private val TOWN = "16.1 Ora"
  private val VILLAGE = "16.2 Sat"
  private val STREET = "17. Strada"
  private val STREET_NB = "18. Nr."
  private val ZIP_CODE = "19. Cod po"
  private val BUILDING = "20. Bl"
  private val STAIRCASE = "21. Sc."
  private val APARTMENT = "22. Ap."

  private def daysOfMonth(monthIndex: Int): Int = {
      DateTime.now.month(monthIndex).dayOfMonth.maxValue
  }

  def fillAnnex(farmerId: String, zootechnic: scala.List[Col], bs: Array[Byte], storedAlfalfa: Double, filePath: Option[String]): Either[String, Array[Byte]] = {
    val pdfDoc = asPdfDoc(bs)
    (farmer(bs, farmerId, pdfDoc), alfalfa(pdfDoc)) match {
      case (Right(farmer), Right(hectares)) =>
        val totalAlfalfa = computeMonths.foldLeft(0.0)((t, m) => t + hectares * m.alfalfaAvgPerHectar)
        buildPdf(filePath, farmer, zootechnic, Consumption(Map(), hectares, totalAlfalfa, totalAlfalfa - storedAlfalfa, storedAlfalfa))
      case (Left(err), _) => Left(err)
      case (_, Left(err)) => Left(err)
    }
  }

  private def asPdfDoc(bs: Array[Byte]): PdfDocument = new PdfDocument(new PdfReader(new RandomAccessSourceFactory().createSource(bs), new ReaderProperties()))

  private def farmer(bytes: Array[Byte], farmerId: String, pdfDoc: PdfDocument): Either[String, Farmer] = {
    getPdfContent(pdfDoc, 1).flatMap { content =>
      pTables(bytes, Seq(
        T(0, H(SURNAME, repExactly = 4), H(NAME, repExactly = 4, isLast = true)),
        T(0, H(COMPANY, isLast = true)),
        T(0, H(COUNTY, repExactly = 4), H(LOCALITY, repExactly = 7, skip = true), H(TOWN, repExactly = 4),
          H(VILLAGE, repExactly = 4, isLast = true)),
        T(0, H(STREET, repExactly = 4), H(STREET_NB, repExactly = 4), H(ZIP_CODE, repExactly = 4), H(BUILDING, repExactly = 4),
          H(STAIRCASE, repExactly = 4), H(APARTMENT, repExactly = 4, isLast = true))), content)
    }.fold(err => Left(err), cs => Right(toFarmer(farmerId, cs._2)))
  }

  private def toFarmer(farmerId: String, cols: Seq[Col]): Farmer = {
    cols.foldLeft(Farmer(farmerId)) { (farmer, c) =>
      c.h.name match {
        case SURNAME => farmer.copy(sn = c.v)
        case NAME => farmer.copy(n = c.v)
        case COMPANY => farmer.copy(company = c.v)
        case COUNTY => farmer.copy(county = c.v)
        case VILLAGE => farmer.copy(village = c.v)
        case TOWN => farmer.copy(town = c.v)
        case STREET => farmer.copy(street = c.v)
        case STREET_NB => farmer.copy(streetNb = c.v)
        case _ => farmer
      }
    }
  }

  def zootechnic(errorOrBytes: Either[String, Array[Byte]]): IO[Either[String, (Array[Byte], Seq[Col])]] = errorOrBytes match {
    case Right(bytes) =>
      IO(asPdfDoc(bytes)).attempt.map {
        case Right(pdfDocument) =>
          getPdfContent(pdfDocument, 2).flatMap(content =>
            pTables(bytes, Seq(
              T(2,
                H("Bovine > 2 ani", coef = 1.00, kg = 6), H("Bovine 6 luni - 2 ani", coef = 0.6, kg = 3.6), H("Bovine < 6 luni", coef = 0.4, kg = 2.4),
                H("Ecvidee > 6 luni", coef = 1.00, kg = 6), H("Ecvidee < 6 luni", ignore), H("Ovine", coef = 0.15, kg = 0.898, isLast = true)),
              T(3, H("Caprine", coef = 0.15, kg = 0.898), H("Scroafe rep. > 50kg", ignore), H("Alte porcine", ignore), H("Gaini ou", ignore), H("Alte pasari curte", ignore, isLast = true))
            ),
              content
            ))
        case Left(err) => Left(err.getLocalizedMessage)
      }
    case Left(err) => IO(Left(err))
  }

  private def alfalfa(pdfDoc: PdfDocument): Either[String, Double] = {
    (getPdfContent(pdfDoc, 3), getPdfContent(pdfDoc, 4)) match {
      case (Right(c1), Right(c2)) => pAlfalfa(c1 + c2)
      case (Left(err), _) => Left(err)
      case (_, Left(err)) => Left(err)
    }
  }

  private def getPdfContent(pdfDoc: PdfDocument, page: Int): Either[String, String] = {
    pdfDoc.getPage(page).getPdfObject.get(PdfName.Contents) match {
      case stream: PdfStream => Right(new String(stream.getBytes(true), "windows-1250"))
      case _ => Left(s"""Eroare la parsarea paginii $page""")
    }
  }

  private def buildPdf(filePath: Option[String], farmer: Farmer, cs: scala.List[Col], init: Consumption): Either[String, Array[Byte]] = {
    val normal: PdfFont = PdfFontFactory.createFont("DejaVuSansCondensed.ttf", "Cp1250", true)
    val bold: PdfFont = PdfFontFactory.createFont("DejaVuSansCondensed-Bold.ttf", "Cp1250", true)
    val oblique: PdfFont = PdfFontFactory.createFont("DejaVuSansCondensed-Oblique.ttf", "Cp1250", true)
    val os = if (filePath.isDefined) new FileOutputStream(new File(filePath.get + farmer.fullName(sep = "_") + ".pdf")) else new ByteArrayOutputStream()
    val pdfDoc = new PdfDocument(new PdfWriter(os))
    val doc = new Document(pdfDoc, A4)
    val kgAvg: Option[Double] = calcKgAvg(cs, init)
    doc.setLeftMargin(48)
    val validCols = cs.filter(c => c.v.exists(c.h.cond))
    val consumption = validCols.foldLeft(init) { (consumption, c) =>
      if (consumption.remainingAlfalfa >= calcMinConsumption(c, kgAvg)) {
        doc.add(p(farmer.toString, font = normal))
        doc.add(
          p("Anexa Nr. 5", font = bold).setBold().setTextAlignment(TEXT_RIGHT)
            .add(t("\n(Anexa nr.5", font = oblique))
            .add(t("1", font = oblique).setTextRise(5).setFontSize(5))
            .add(t("  la Ordinul nr. 619/2015)", font = oblique).setItalic())
        )
        doc.add(lineBreak(3, font = normal))
        doc.add(centeredText(doc, "Calculul efectivului mediu furajat din ferm\u0103 \u015fi produc\u0163ia de lucern\u0103/soia/f\u00e2n*)", font = bold))
        doc.add(p(c.h.name, font = bold).setTextAlignment(TEXT_CENTER).setBold())
        doc.add(lineBreak(1, font = normal))
        val (newConsumption, table) = animalFedTable(c, normal, bold, consumption, kgAvg)
        doc.add(table)
        doc.add(legend.foldLeft(p(font = normal))((p, s) => p.add(t(s, fontSize = 8, font = normal))))
        if (validCols.last != c && newConsumption.remainingAlfalfa >= calcMinConsumption(c, kgAvg)) {
          doc.add(new AreaBreak())
        }
        newConsumption
      } else consumption
    }
    doc.add(new AreaBreak())
    doc.add(p(farmer.toString, font = normal))
    doc.add(centeredText(doc, "Produc\u0163ia de lucern\u0103/soia/f\u00e2n \u015fi calculul utiliz\u0103rii acesteia", font = bold))
    doc.add(alfalfaProductionTable(cs, consumption, normal, bold))
    doc.add(p("(col.5***) se calculeaz\u0103 prin \u00eensumarea consumului de lucern\u0103/soia total pe UVM prev\u0103zut la col.9 din tabelul anterior", fontSize = 8, normal).setMarginLeft(91))
    doc.add(lineBreak(2, font = normal))
    doc.add(centeredText(doc, nameSurnameLabel + spaces(farmer)._1 + signLabel, font = normal))
    doc.add(centeredText(doc, farmer.fullName(capitalize = true) + spaces(farmer)._2 + IndexedSeq.fill(2 * signLabel.length)(".").mkString, font = normal))
    doc.add(p(DateTimeFormat.forPattern("dd.MM.yyyy").print(signDate()), font = normal))
    try {
      doc.close()
      if (filePath.isDefined)
        Right(Array[Byte]())
      else {
        val out = os.asInstanceOf[ByteArrayOutputStream].toByteArray
        os.close()
        Right(out)
      }
    }catch {
      case e: Exception => Left(e.getMessage)
    }
  }

  private def signDate(): DateTime  = if (DateTime.now.isAfter(DateTime.now.day(2).month(4))) DateTime.now.day(2).month(4) else DateTime.now

  private def spaces(farmer: Farmer): (String, String) = {
    val t = if (farmer.fullName().length > nameSurnameLabel.length)
      (farmer.fullName().length, 2 * farmer.fullName().length - nameSurnameLabel.length)
    else
      (2 * nameSurnameLabel.length - farmer.fullName().length, nameSurnameLabel.length)
    (IndexedSeq.fill(t._1)(" ").mkString, IndexedSeq.fill(t._2)(" ").mkString)
  }

  private def calcKgAvg(cs: Seq[Col], initConsumption: Consumption): Option[Double] = {
    None
//    cs.foldLeft((initConsumption, 0.0)) { (t, c) =>
//      if (t._1.remainingAlfalfa >= calcMinConsumption(c) && c.v.exists(c.h.cond)) {
//        computeMonths.foldLeft(t) { (tt, m) =>
//          val totalPerMonth = tt._2 + consumptionPerMonth(c, m)
//          println("tt._1.remainingAlfalfa = " + tt._1.remainingAlfalfa)
//          val monthly = tt._1.remainingAlfalfa min calConsumption(c, m)
//          (Consumption.alfalfa.modify(rAlfalfa => 0.0 max (rAlfalfa - monthly))(
//            Consumption.perMonth.modify((_: Map[String, Double]).updated(m.name, tt._1.perMonth.getOrElse(m.name, 0.0) + monthly))(tt._1)), totalPerMonth)
//        }
//      } else t
//    } match {
//      case (c, days) if c.remainingAlfalfa > c.storedAlfalfa =>
//        println("c.totalAlfalfa - c.storedAlfalfa  = " + (c.totalAlfalfa - c.storedAlfalfa))
//        println("days = " + days)
//        Some((c.totalAlfalfa - c.storedAlfalfa) * 1000.0 / days)
//      case _ => None
//    }
  }

  private def p(s: String = "", fontSize: Int = 10, font: PdfFont): Paragraph = {
    new Paragraph(s).setFontSize(fontSize).setFont(font)
  }

  private def t(s: String = "", fontSize: Int = 10, font: PdfFont): Text = {
    new Text(s).setFontSize(fontSize).setFont(font)
  }

  private def centeredText(d: Document, s: String, font: PdfFont, bold: Boolean = true) = {
    val elem = p(font = font).addTabStops(new TabStop(width(d) / 2, TAB_CENTER), new TabStop(width(d), TAB_LEFT)).add(new Tab()).add(s).add(new Tab())
    if (bold)
      elem.setBold()
    else
      elem
  }

  private def width(document: Document) = {
    A4.getWidth - document.getLeftMargin - document.getRightMargin
  }

  private def lineBreak(lines: Int, font: PdfFont): IBlockElement = {
    (0 until lines).foldLeft(new Div())((div, _) => div.add(p(font = font)))
  }

  private def calcMinConsumption(c: Col, kgAvg: Option[Double] = None): Double =
    computeMonths.foldLeft(Double.MaxValue) { (minConsumption, m) =>
      val calc = calConsumption(c, m, kgAvg)
      if (calc > 0)
        minConsumption min calc
      else
        minConsumption
    }

  private def animalFedTable(c: Col, normal: PdfFont, bold: PdfFont, initConsumption: Consumption, kgAvg: Option[Double]): (Consumption, Table) = {
    val table = (1 to 9).foldLeft(new Table(9, false)
      .addCell(hCell("Luna/\nPerioada", w = 53, font = normal))
      .addCell(hCell("Efectiv la \u00eenceputul perioadei (cap.)", font = normal))
      .addCell(hCell("Total\nintr\u0103ri\n(cap.)", font = normal))
      .addCell(hCell("Total\nie\u015firi\n(cap.)", font = normal))
      .addCell(hCell("Efectiv la sf\u00e2r\u015fitul perioadei (cap.)**)", font = normal))
      .addCell(hCell("Zile animale furajate (nr.)", font = normal))
      .addCell(hCell("Efectiv mediu furajat (cap)", font = normal))
      .addCell(hCell("Efectiv mediu furajat (UVM)", font = normal))
      .addCell(hCell("Consum lucern\u0103/soia/f\u00e2n/mazare\nboabe total pe UVM\n (to)", w = 108.0f, font = normal)))((t, i) => t.addCell(cell(i.toString, font = normal)))
    var t2, t6, t7 = 0
    var t8, t9 = 0.0
    val newConsumption = computeMonths.foldLeft(initConsumption) { (consumption, m) =>
      if(m.isApril()) {
        consumption
      } else {
        t2 = c.v.getOrElse("0").toInt
        t6 += c.v.getOrElse("0").toInt * m.days * m.fedFactor
        t7 = c.v.getOrElse("0").toInt * m.fedFactor
        t8 = c.v.getOrElse("0").toInt * c.h.coef
        val monthlyConsumption = consumption.remainingAlfalfa min calConsumption(c, m, kgAvg)
        t9 += monthlyConsumption
        table.addCell(cell(m.name + " " + m.year.toString, w = 53, font = normal))
          .addCell(cell(c.valueAsText, font = normal))
          .addCell(cell("-", font = normal)).addCell(cell("-", font = normal))
          .addCell(cell(c.valueAsText, font = normal))
          .addCell(cell((c.v.getOrElse("0").toInt * m.days * m.fedFactor).toString, font = normal))
          .addCell(cell((c.v.getOrElse("0").toInt * m.fedFactor).toString, font = normal))
          .addCell(cell("%1.2f".format(c.v.getOrElse("0").toInt * c.h.coef), font = normal))
          .addCell(cell("%1.4f".format(monthlyConsumption), font = normal))
        Consumption.alfalfa.modify(alfalfa => 0.0 max (alfalfa - monthlyConsumption))(
          Consumption.perMonth.modify((_: Map[String, Double]).updated(m.name, consumption.perMonth.getOrElse(m.name, 0.0) + monthlyConsumption))(consumption)
        )
      }
    }
    (newConsumption, table.addCell(cell("Total\nperioad\u0103/\nan", h = 51, w = 53, font = bold, bold = true))
      .addCell(cell(t2.toString, font = normal))
      .addCell(cell("-", font = normal)).addCell(cell("-", font = normal))
      .addCell(cell(t2.toString, font = normal))
      .addCell(cell(t6.toString, font = normal))
      .addCell(cell(t7.toString, font = normal))
      .addCell(cell("%1.2f".format(t8), font = normal))
      .addCell(cell("%1.4f".format(t9), font = normal)))
  }

  private def calConsumption(c: Col, m: M, kgAvg: Option[Double] = None) = {
    (consumptionPerMonth(c, m) * kgAvg.getOrElse(c.h.kg)) / 1000.0
  }

  private def consumptionPerMonth(c: parser.Col, m: pdf.M): Double = c.v.getOrElse("0").toInt * c.h.coef * m.days * m.fedFactor

  private def alfalfaProductionTable(cols: Seq[Col], initConsumption: Consumption, normal: PdfFont, bold: PdfFont): Table = {
    val table = (1 to 6).foldLeft(new Table(6, false).addCell(hCell("Luna/\nPerioada", w = 53, h = 100, rSpan = 2, font = normal))
      .addCell(hCell("Produc\u0163ia lucern\u0103/soia/f\u00e2n\nrealizat\u0103(tone)", h = 50, w = 100, cSpan = 2, font = normal))
      .addCell(hCell("(to) din care:", w = 163.0f, h = 15, cSpan = 3, font = normal))
      .addCell(hCell("total\u0103\n(to)", w = 50, h = 50, font = normal))
      .addCell(hCell("medie pe\nhectar\n(to)", w = 50, h = 50, font = normal))
      .addCell(hCell("Livrat\u0103", w = 50, h = 85, font = normal))
      .addCell(hCell("Consumat\u0103\ncu\nanimalele", w = 50, h = 85, font = normal))
      .addCell(hCell("Depozitat\u0103\n\u00een vederea\nlivr\u0103rii/\nconsumului\ncu animalele", w = 63f, h = 85, font = normal)))((t, i) => t.addCell(cell(if (i == 5) i + "***)" else i.toString, font = normal)))
      .setMarginLeft(91f)
    var t2, t3, t5 = 0.0
    computeMonths.foldLeft(table) { (t, m) =>
      if(m.isApril()) {
        t
      } else {
        t2 += initConsumption.hectares * m.alfalfaAvgPerHectar
        t3 += m.alfalfaAvgPerHectar
        t5 += initConsumption.perMonth.getOrElse(m.name, 0.0)
        t.addCell(cell(m.name + " " + m.year.toString, w = 53, font = normal))
          .addCell(cell("%1.2f".format(initConsumption.hectares * m.alfalfaAvgPerHectar), font = normal))
          .addCell(cell("%1.2f".format(m.alfalfaAvgPerHectar), font = normal))
          .addCell(cell("0", font = normal))
          .addCell(cell("%1.4f".format(initConsumption.perMonth.getOrElse(m.name, 0.0)), font = normal))
          .addCell(cell("%1.2f".format(initConsumption.hectares * m.alfalfaAvgPerHectar), font = normal))
      }
    }
    table.addCell(cell("Total\nperioad\u0103/\nan", h = 51, w = 53, font = bold, bold = true))
      .addCell(cell("%1.2f".format(t2), font = normal))
      .addCell(cell("%1.2f".format(t3), font = normal))
      .addCell(cell("0", font = normal))
      .addCell(cell("%1.4f".format(t5), font = normal))
      .addCell(cell("%1.4f".format(if (t2 - t5 > 0) t2 - t5 else 0.0), font = normal))
    table
  }

  private def hCell(text: String, w: Float = 40, h: Float = 64.0f, rSpan: Int = 1, cSpan: Int = 1, font: PdfFont): Cell = {
    cell(text, h = h, w = w, rSpan = rSpan, cSpan = cSpan, font = font)
  }

  private def cell(text: String, h: Float = 16.0f, fontSize: Int = 10, w: Float = 36.0f, font: PdfFont, bold: Boolean = false, rSpan: Int = 1, cSpan: Int = 1): Cell = {
    val cell = new Cell(rSpan, cSpan).setHeight(h).setWidth(w)
    cell.setNextRenderer(new ClipCenterCellContentCellRenderer(cell, p(text, fontSize, font = font), font, bold))
    cell
  }

  import com.itextpdf.kernel.pdf.canvas.PdfCanvas
  import com.itextpdf.kernel.pdf.xobject.PdfFormXObject
  import com.itextpdf.layout.element.Paragraph
  import com.itextpdf.layout.layout.LayoutArea
  import com.itextpdf.layout.layout.LayoutContext
  import com.itextpdf.layout.renderer.CellRenderer
  import com.itextpdf.layout.renderer.DrawContext

  private class ClipCenterCellContentCellRenderer(val cell: Cell, val content: Paragraph, val font: PdfFont, val bold: Boolean) extends CellRenderer(cell) {
    override def draw(drawContext: DrawContext): Unit = {
      try {
        val pr = content.createRendererSubTree.setParent(this)
        val textArea = pr.layout(new LayoutContext(new LayoutArea(0, new Rectangle(cell.getWidth.getValue, cell.getHeight.getValue))))
        val area = textArea.getOccupiedArea
        val box = area.getBBox
        val spaceneeded = box.getHeight + 6
        val offset = (getOccupiedAreaBBox.getHeight - box.getHeight) / 2
        val xObject = new PdfFormXObject(new Rectangle(getOccupiedAreaBBox.getWidth, getOccupiedAreaBBox.getHeight))
        val layoutCanvas = new Canvas(new PdfCanvas(xObject, drawContext.getDocument), drawContext.getDocument, new Rectangle(-5, offset, cell.getWidth.getValue + 10, spaceneeded))
        layoutCanvas.setTextAlignment(TEXT_CENTER).setFont(font)
        if (bold) {
          layoutCanvas.setBold()
        }
        layoutCanvas.add(content)
        drawContext.getCanvas.addXObject(xObject, occupiedArea.getBBox.getLeft, occupiedArea.getBBox.getBottom)
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }
  }

}