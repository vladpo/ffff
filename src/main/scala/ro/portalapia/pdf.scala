package ro.portalapia

import java.io.{ByteArrayOutputStream, File, FileOutputStream}

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
import monocle.macros.GenLens

object pdf {

  case class Farmer(id: String, sn: Option[String] = None, n: Option[String] = None, county: Option[String] = None, town: Option[String] = None, village: Option[String] = None, street: Option[String] = None, streetNb: Option[String] = None) {
    override def toString: String = {
      def spaced(ms: Option[String]): String = ms.map(_ + " ").getOrElse("")

      spaced(sn) + spaced(n) + id + "\n" + spaced(county) + spaced(town) + spaced(village) + spaced(street) + spaced(streetNb)
    }
  }

  case class M(name: String, days: Int, fedFactor: Int = 1, alfalfaAvg: Float = 0.0f)
  case class Consumption(perMonth: Map[String, Double], totalAlfalfa: Double, remainingAlfalfa: Double)
  object Consumption {
    def perMonth = GenLens[Consumption](_.perMonth)
    def alfalfa = GenLens[Consumption](_.remainingAlfalfa)
  }

  private lazy val months: Vector[M] = Vector(
    M("Mai", daysOfMonth(5), fedFactor = 0, alfalfaAvg = 2.0f), M("Iunie", daysOfMonth(6), fedFactor = 0), M("Iulie", daysOfMonth(7), fedFactor = 0, alfalfaAvg = 1.2f),
    M("August", daysOfMonth(8), fedFactor = 0), M("Septembrie", daysOfMonth(9), fedFactor = 0, alfalfaAvg = 0.8f), M("Octombrie", daysOfMonth(10), fedFactor = 0),
    M("Noiembrie", daysOfMonth(11)), M("Decembrie", daysOfMonth(12)), M("Ianuarie", daysOfMonth(1)), M("Februarie", daysOfMonth(2)), M("Martie", daysOfMonth(3)),
    M("Aprilie", daysOfMonth(4), fedFactor = 0))

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
    "\n(col. 7) Efectivul mediu furajat an/total perioad\u0103 = (col. 6) - 365 zile (sau nr. zile perioad\u0103).",
    "\n(col. 8) = col. (7) X coef. de transformare \u00een UVM al categoriei de animale respective din anexa nr. 5 la Ordinul nr. 619/2015, cu modificarile \u015fi complet\u0103rile ulterioare",
    "\n(col. 9) consum lucern\u0103/soia/f\u00e2n lunar = (col. 8) X cantitatea de lucern\u0103/soia consumat\u0103 pe UVM stabilit\u0103 prin ra\u0163ie de c\u0103tre fermier X nr. zile lun\u0103",
    "\n(col. 9) consum lucern\u0103/soia/f\u00e2n an/total perioad\u0103 = (col. 8) X cantitatea de lucern\u0103/soia consumat\u0103 pe UVM stabilit\u0103 prin ra\u0163ie de",
    "\nc\u0103tre fermier X 365 zile (sau nr. zile perioad\u0103). Cantitatea calculat\u0103 nu poate dep\u0103\u015fi cantitatea maxim\u0103/UVM/an prev\u0103zut\u0103 \u00een anexa nr. 5 la Ordinul nr. 619/2015.")

  private lazy val ignore: (String) => Boolean = _ => false
  private lazy val SURNAME = "01. Nume"
  private lazy val NAME = "02. Prenume"
  private lazy val COUNTY = "15. Jude"
  private lazy val LOCALITY = "16. Localitate"
  private lazy val TOWN = "16.1 Ora"
  private lazy val VILLAGE = "16.2 Sat"
  private lazy val STREET = "17. Strada"
  private lazy val STREET_NB = "18. Nr."
  private lazy val ZIP_CODE = "19. Cod po"
  private lazy val BUILDING = "20. Bl"
  private lazy val STAIRCASE = "21. Sc."
  private lazy val APARTMENT = "22. Ap."

  private def daysOfMonth(monthIndex: Int): Int = {
    DateTime.now.month(monthIndex).dayOfMonth.maxValue
  }

  def fillAnnex(farmerId: String, zootechnic: scala.List[Col], bs: Array[Byte], storedAlfalfa: Double, isFileSave: Boolean = true): Either[String, Array[Byte]] = {
    val pdfDoc = asPdfDoc(bs)
    (farmer(farmerId, pdfDoc), alfalfa(pdfDoc)) match {
      case (Right(farmer), Right(alfalfa)) => Right(buildPdf(farmer, zootechnic, Consumption(Map(), alfalfa, computeMonths.foldLeft(0.0)((t, m) => t + alfalfa * m.alfalfaAvg) - storedAlfalfa), isFileSave))
      case (Left(err), _) => Left(err)
      case (_, Left(err)) => Left(err)
    }
  }

  private def asPdfDoc(bs: Array[Byte]): PdfDocument = new PdfDocument(new PdfReader(new RandomAccessSourceFactory().createSource(bs), new ReaderProperties()))

  private def farmer(farmerId: String, pdfDoc: PdfDocument): Either[String, Farmer] = {
    getPdfContent(pdfDoc, 1).flatMap { content =>
      pTables(Seq(
        T(0, H(SURNAME, repExactly = 4), H(NAME, repExactly = 4, isLast = true)),
        T(0, H(COUNTY, repExactly = 4), H(LOCALITY, repExactly = 7, skip = true), H(TOWN, repExactly = 4),
          H(VILLAGE, repExactly = 4, isLast = true)),
        T(0, H(STREET, repExactly = 4), H(STREET_NB, repExactly = 4), H(ZIP_CODE, repExactly = 4), H(BUILDING, repExactly = 4),
          H(STAIRCASE, repExactly = 4), H(APARTMENT, repExactly = 4, isLast = true))), content)
    }.fold(err => Left(err), cs => Right(toFarmer(farmerId, cs)))
  }

  private def toFarmer(farmerId: String, cols: Seq[Col]): Farmer = {
    cols.foldLeft(Farmer(farmerId)) { (farmer, c) =>
      c.h.name match {
        case SURNAME => farmer.copy(sn = c.v)
        case NAME => farmer.copy(n = c.v)
        case COUNTY => farmer.copy(county = c.v)
        case VILLAGE => farmer.copy(village = c.v)
        case TOWN => farmer.copy(town = c.v)
        case STREET => farmer.copy(street = c.v)
        case STREET_NB => farmer.copy(streetNb = c.v)
        case _ => farmer
      }
    }
  }

  def zootechnic(bs: Array[Byte]): Either[String, Seq[Col]] = {
    getPdfContent(asPdfDoc(bs), 2).flatMap(content =>
      pTables(Seq(
        T(2,
          H("Bovine > 2 ani", coef = 1.00, kg = 4), H("Bovine 6 luni - 2 ani", coef = 0.6, kg = 3.6), H("Bovine < 6 luni", cond = _.toInt > 5, coef = 0.4, kg = 2.4),
          H("Ecvidee > 6 luni", coef = 1.00, kg = 4), H("Ecvidee < 6 luni", ignore), H("Ovine", cond = _.toInt > 30, coef = 0.15, kg = 0.898, isLast = true)),
        T(3, H("Caprine", cond = _.toInt > 25, coef = 0.15, kg = 0.898), H("Scroafe rep. > 50kg", ignore), H("Alte porcine", ignore), H("Gaini ou", ignore), H("Alte pasari curte", ignore, isLast = true))
      ),
        content
      ))
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

  private def buildPdf(farmer: Farmer, cs: scala.List[Col], init: Consumption, isFileSave: Boolean): Array[Byte] = {
    val normal: PdfFont = PdfFontFactory.createFont("./src/main/resources/DejaVuSansCondensed.ttf", "Cp1250", true)
    val bold: PdfFont = PdfFontFactory.createFont("./src/main/resources/DejaVuSansCondensed-Bold.ttf", "Cp1250", true)
    val oblique: PdfFont = PdfFontFactory.createFont("./src/main/resources/DejaVuSansCondensed-Oblique.ttf", "Cp1250", true)
    val os = if (isFileSave) new FileOutputStream(new File(farmerFileName(farmer))) else new ByteArrayOutputStream()
    val pdfDoc = new PdfDocument(new PdfWriter(os))
    val doc = new Document(pdfDoc, A4)

    doc.setLeftMargin(48)
    val consumption = cs.foldLeft(init) { (consumption, c) =>
      if (consumption.remainingAlfalfa >= calcMinConsumption(c) && c.v.exists(c.h.cond)) {
        doc.add(p(farmer.toString, font = normal))
        doc.add(
          p("Anexa Nr. 5", font = bold).setBold().setTextAlignment(TEXT_RIGHT)
            .add(t("\n(Anexa nr.5", font = oblique))
            .add(t("1", font = oblique).setTextRise(5).setFontSize(5))
            .add(t("  la Ordinul nr. 619/2015)", font = oblique).setItalic())
        )
        doc.add(lineBreak(3, font = normal))
        doc.add(centeredTitle(doc, "Calculul efectivului mediu furajat din ferm\u0103 \u015fi produc\u0163ia de lucern\u0103/soia/f\u00e2n*)", font = bold))
        doc.add(p(c.h.name, font = bold).setTextAlignment(TEXT_CENTER).setBold())
        doc.add(lineBreak(1, font = normal))
        val (newConsumption, table) = animalFedTable(c, normal, bold, consumption)
        doc.add(table)
        doc.add(legend.foldLeft(p(font = normal))((p, s) => p.add(t(s, fontSize = 8, font = normal))))
        if (cs.filter(c => c.v.exists(c.h.cond)).last != c) {
          doc.add(new AreaBreak())
        }
        newConsumption
      } else consumption
    }
    doc.add(new AreaBreak())
    doc.add(p(farmer.toString, font = normal))
    doc.add(centeredTitle(doc, "Produc\u0163ia de lucern\u0103/soia/f\u00e2n \u015fi calculul utiliz\u0103rii acesteia", font = bold))
    doc.add(alfalfaProductionTable(cs, consumption, normal, bold))
    doc.add(p("(col.5***) se calculeaz\u0103 prin \u00eensumarea consumului de lucern\u0103/soia total pe UVM prev\u0103zut la col.9 din tabelul anterior", fontSize = 8, normal).setMarginLeft(91))
    doc.close()
    if(isFileSave)
      Array[Byte]()
    else {
      val out = os.asInstanceOf[ByteArrayOutputStream].toByteArray
      os.close()
      out
    }
  }

  private def farmerFileName(farmer: Farmer): String = farmer.id + farmer.n.fold(farmer.sn.fold("")("_" + _))("_" + _ + farmer.sn.fold("")("_" + _)).replace(" ", "_")

  private def p(s: String = "", fontSize: Int = 10, font: PdfFont): Paragraph = {
    new Paragraph(s).setFontSize(fontSize).setFont(font)
  }

  private def t(s: String = "", fontSize: Int = 10, font: PdfFont): Text = {
    new Text(s).setFontSize(fontSize).setFont(font)
  }

  private def centeredTitle(d: Document, s: String, font: PdfFont) = {
    p(font = font).addTabStops(new TabStop(width(d) / 2, TAB_CENTER), new TabStop(width(d), TAB_LEFT)).add(new Tab()).add(s).add(new Tab()).setBold()
  }

  private def width(document: Document) = {
    A4.getWidth - document.getLeftMargin - document.getRightMargin
  }

  private def lineBreak(lines: Int, font: PdfFont): IBlockElement = {
    (0 until lines).foldLeft(new Div())((div, _) => div.add(p(font = font)))
  }

  private def calcMinConsumption(c: Col): Double =
    computeMonths.foldLeft(Double.MaxValue){ (minConsumption, m) =>
      val calc = calConsumption(c, m)
      if(calc > 0)
        minConsumption min calc
      else
        minConsumption
    }

  private def animalFedTable(c: Col, normal: PdfFont, bold: PdfFont, initConsumption: Consumption): (Consumption, Table) = {
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
    var t2, t6 = 0
    var t8, t9 = 0.0
    val newConsumption = computeMonths.foldLeft(initConsumption) { (consumption, m) =>
      t2 += c.v.getOrElse("0").toInt
      t6 += c.v.getOrElse("0").toInt * m.days
      t8 += c.v.getOrElse("0").toInt * c.h.coef
      val monthlyConsumption = consumption.remainingAlfalfa min calConsumption(c, m)
      t9 += monthlyConsumption
      table.addCell(cell(m.name, w = 53, font = normal))
        .addCell(cell(c.valueAsText, font = normal))
        .addCell(cell("-", font = normal)).addCell(cell("-", font = normal))
        .addCell(cell(c.valueAsText, font = normal))
        .addCell(cell((c.v.getOrElse("0").toInt * m.days).toString, font = normal))
        .addCell(cell(c.valueAsText, font = normal))
        .addCell(cell("%1.2f".format(c.v.getOrElse("0").toInt * c.h.coef), font = normal))
        .addCell(cell("%1.4f".format(monthlyConsumption), font = normal))
      Consumption.alfalfa.modify(alfalfa => 0.0 max (alfalfa - monthlyConsumption))(
        Consumption.perMonth.modify((_: Map[String, Double]).updated(m.name, consumption.perMonth.getOrElse(m.name, 0.0) + monthlyConsumption))(consumption)
      )
    }
    (newConsumption, table.addCell(cell("Total\nperioad\u0103/\nan", h = 51, w = 53, font = bold, bold = true))
      .addCell(cell(t2.toString, font = normal))
      .addCell(cell("-", font = normal)).addCell(cell("-", font = normal))
      .addCell(cell(t2.toString, font = normal))
      .addCell(cell(t6.toString, font = normal))
      .addCell(cell(t2.toString, font = normal))
      .addCell(cell("%1.2f".format(t8), font = normal))
      .addCell(cell("%1.4f".format(t9), font = normal)))
  }

  private def calConsumption(c: Col, m: M) = (c.v.getOrElse("0").toInt * m.days * m.fedFactor * c.h.kg) / 1000.0

  private def alfalfaProductionTable(cols: Seq[Col], consumption: Consumption, normal: PdfFont, bold: PdfFont): Table = {
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
      t2 += consumption.totalAlfalfa * m.alfalfaAvg
      t3 += m.alfalfaAvg
      t5 += consumption.perMonth.getOrElse(m.name, 0.0)
      t.addCell(cell(m.name, w = 53, font = normal))
        .addCell(cell("%1.2f".format(consumption.totalAlfalfa * m.alfalfaAvg), font = normal))
        .addCell(cell("%1.2f".format(m.alfalfaAvg), font = normal))
        .addCell(cell("0", font = normal))
        .addCell(cell("%1.2f".format(consumption.perMonth.getOrElse(m.name, 0.0)), font = normal))
        .addCell(cell("%1.2f".format(consumption.totalAlfalfa * m.alfalfaAvg), font = normal))
    }
    table.addCell(cell("Total\nperioad\u0103/\nan", h = 51, w = 53, font = bold, bold = true))
      .addCell(cell("%1.2f".format(t2), font = normal))
      .addCell(cell("%1.2f".format(t3), font = normal))
      .addCell(cell("0", font = normal))
      .addCell(cell("%1.2f".format(t5), font = normal))
      .addCell(cell("%1.2f".format(if (t2 - t5 > 0) t2 - t5 else 0.0), font = normal))
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