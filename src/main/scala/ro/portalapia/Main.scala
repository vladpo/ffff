package ro.portalapia

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import java.util.concurrent.{Executors, ThreadFactory}

import cats.effect.IO
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.asynchttpclient.fs2.AsyncHttpClientFs2Backend
import fs2.Stream
import io.netty.util.concurrent.DefaultThreadFactory
import ro.portalapia.http._
import ro.portalapia.pdf.{fillAnnex, _}

import scala.concurrent.ExecutionContext
import scalafx.application.{JFXApp, Platform}
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, TextField}
import scalafx.scene.layout.{GridPane, Pane}

object Main extends JFXApp {
  implicit val client: SttpBackend[IO, Stream[IO, ByteBuffer]] = AsyncHttpClientFs2Backend[IO]()
  val user: TextField = new TextField
  val password: TextField = new TextField
  val fetchParcelButton: Button = new Button {
    text = "Afiseaza animale"
    disable = true
    margin = Insets(6)
  }
  val gridPane: GridPane = new GridPane {
    margin = Insets(21)
    add(label("Nume utilizator"), 1, 1)
    add(user, 2, 1)
    add(label("Parola"), 3, 1)
    add(password, 4, 1)
    add(fetchParcelButton, 5, 1)
    add(space, 1, 2, 5, 1)
  }
  stage = new JFXApp.PrimaryStage {
    title.value = "Apia Portal"
    width = 600
    height = 400
    scene = new Scene {
      content = gridPane
      Platform.runLater(user.requestFocus())
    }
    onCloseRequest = _ => {
      client.close()
      close()
    }
  }
  val printButton: Button = new Button {
    text = "Printeaza anexele"
    visible = false
    margin = Insets(6)
  }

  user.text.onChange((_, _, newValue) => (fetchParcelButton: Button).disable = newValue.isEmpty || password.text.value.isEmpty)
  password.text.onChange((_, _, newValue) => (fetchParcelButton: Button).disable = newValue.isEmpty || user.text.value.isEmpty)


  ExecutionContext.fromExecutor(Executors.newCachedThreadPool(new ThreadFactory {
    override def newThread(r: Runnable): Thread =
  }))
  fetchParcelButton.onMouseClicked = _ => for {
    either <- fetchParcelContent(User(user.text.value), Password(password.text.value), 2017).map(_.map(bs => (bs, zootechnic(bs))))
    _ <- IO.shift
  }
      .map {
        case (bs, Right(cols)) =>
          val tfs = cols.foldLeft(List[ColTextField]()) { (tfs, col) =>
            val tf = ColTextField(col)
            gridPane.add(tf, tfs.size + 1, 3)
            tf :: tfs
          }
          gridPane.add(printButton, 1, 1)
          printButton.onMouseClicked = _ => for {
            annex <- fillAnnex(user.text.value, tfs.foldRight(List[parser.Col]())((tf, cols) => tf.col.copy(v = Some(tf.text.value)) :: cols), bs)
            _ <- print(annex)
          } yield ()
      }).unsafeRunSync()

  def label(s: String): Label = {
    new Label {
      margin = Insets(6)
      text = s
    }
  }

  def space: Pane = new Pane { prefHeight = 12 }

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
  }

}

