package ro.portalapia

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer

import cats.effect.IO
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.asynchttpclient.fs2.AsyncHttpClientFs2Backend
import fs2.Stream
import ro.portalapia.http._
import ro.portalapia.pdf.{fillAnnex, _}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scalafx.application.{JFXApp, Platform}
import scalafx.geometry.Insets
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.{Button, Label, TextField}
import scalafx.scene.layout.{HBox, VBox}

object Main extends JFXApp {

  implicit val client: SttpBackend[IO, Stream[IO, ByteBuffer]] = AsyncHttpClientFs2Backend[IO]()
  val scalaFxExecutionContext: ExecutionContext = ExecutionContext.fromExecutor((r: Runnable) => Platform.runLater(r))

  val user: TextField = loginTf()
  val password: TextField = loginTf()
  val storedAlfalfa: TextField = loginTf("0")
  val fetchParcelButton: Button = new Button {
    text = "Afiseaza animale"
    disable = user.text.value.isEmpty || password.text.value.isEmpty
  }
  val nodes: Seq[Node] = Seq(new HBox {
    spacing = 3
    margin = Insets(12)
    children = Seq(label("Nume utilizator"), user, label("Parola"), password, label("Lucerna dep."), storedAlfalfa, fetchParcelButton)
  })
  val container: VBox = new VBox {
    spacing = 6
    margin = Insets(21)
    children = nodes
  }

  stage = new JFXApp.PrimaryStage {
    title.value = "Apia Portal"
    width = 800
    height = 400
    scene = new Scene {
      content = container
      Platform.runLater(user.requestFocus())
    }
    onCloseRequest = _ => {
      client.close()
      Platform.exit()
    }
  }

  val printButton: Button = new Button {
    text = "Printeaza anexele"
    margin = Insets(12)
  }
  val saveFileButton: Button = new Button {
    text = "Salveaza ca fisier"
    margin = Insets(12)
  }

  user.text.onChange((_, _, newValue) => (fetchParcelButton: Button).disable = newValue.isEmpty || password.text.value.isEmpty)
  password.text.onChange((_, _, newValue) => (fetchParcelButton: Button).disable = newValue.isEmpty || user.text.value.isEmpty)

  fetchParcelButton.onMouseClicked = _ => (for {
    either <- fetchParcelContent(User(user.text.value), Password(password.text.value), 2017).map(_.map(bs => (bs, zootechnic(bs))))
    _ <- IO.shift(scalaFxExecutionContext)
    nodes <- addZootechnicNodes(either)
    _ <- IO(container.children = nodes)
  } yield ()).unsafeRunAsync {
    case Right(()) =>
    case Left(err) => println(err)
  }

  private def addZootechnicNodes(either: Either[String, (Array[Byte], Either[String, Seq[parser.Col]])]): IO[Seq[Node]] = either match {
    case Right((bs, Right(cols))) =>
      val colBoxes = ArrayBuffer[ColBox]()
      val vBoxes = cols.grouped(4).map{ gr =>
        val cbs = gr.map(ColBox)
        colBoxes ++= cbs
        new HBox{ children = cbs}
      }.toList
      val finalNodes = nodes :+ new VBox {margin = Insets(12); spacing = 12; children = vBoxes } :+ new HBox { spacing = 6; children = List(printButton, saveFileButton)}
      saveFileButton.onMouseClicked = _ => mouseClicked(colBoxes, finalNodes, bs, _ => Right())
      printButton.onMouseClicked = _ => mouseClicked(colBoxes, finalNodes, bs, print, isFileSave = false)
      IO(finalNodes)
    case Left(err) =>
      printButton.disable = true
      IO(nodes :+ label("Eroare: " + err))
  }

  def mouseClicked(colBoxes: Seq[ColBox], nodes: Seq[Node], bs: Array[Byte], f: Array[Byte] => Either[String, Unit], isFileSave: Boolean = true): Unit =
    fillAnnex(user.text.value, colBoxes.foldRight(List[parser.Col]())((colPane, cols) =>
      colPane.col.copy(v = Some(colPane.tf.text.value)) :: cols), bs, storedAlfalfa.text.value.toDouble, isFileSave).map(f).fold(
      err => container.children = nodes :+ label("Eroare: " + err), _ => ())

  def loginTf(value: String = ""): TextField = new TextField {minWidth = 110; maxWidth = 110; text = value}

  def label(s: String): Label = {
    new Label {
      margin = Insets(6)
      text = s
    }
  }

  def fixLabel(s: String): Label = {
    new Label {
      margin = Insets(6)
      text = s
      minWidth = 135
      maxWidth = 135
    }
  }

  def print(annexToPrint: Array[Byte]): Either[String, Unit] = {
    import javax.print.DocFlavor
    import javax.print.PrintServiceLookup
    import javax.print.SimpleDoc
    import javax.print.attribute.HashPrintRequestAttributeSet
    val psInFormat = DocFlavor.INPUT_STREAM.AUTOSENSE
    val myDoc = new SimpleDoc(new ByteArrayInputStream(annexToPrint), psInFormat, null)
    val aset = new HashPrintRequestAttributeSet
    val services = PrintServiceLookup.lookupPrintServices(psInFormat, aset)

    services.headOption.map { myPrinter =>
      val job = myPrinter.createPrintJob
      try {
        job.print(myDoc, aset)
        Right(())
      } catch {
        case pe: Exception =>
          Left(pe.getMessage)
      }
    }.getOrElse(Left("Nici o imprimanta detectata"))
  }

  private case class ColTextField(col: parser.Col) extends TextField {
    text = col.valueAsText
    minWidth = 135
    maxWidth = 135
  }

  private case class ColBox(col: parser.Col) extends VBox {
    val tf = ColTextField(col)
    margin = Insets(0, 5, 0, 0)
    children = Seq(fixLabel(col.h.name), tf)
  }
}

