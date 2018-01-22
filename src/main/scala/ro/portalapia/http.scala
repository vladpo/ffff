package ro.portalapia

import com.softwaremill.sttp._
import cats.effect.IO

object http {

  case class User(id: String)

  case class Password(p: String)

  val url = "https://lpis.apia.org.ro/mapbender/frames/login.php"
  val req: RequestT[Empty, String, Nothing] = sttp.header("User-Agent", "Mozilla/5.0 (Android 4.4; Tablet; rv:41.0) Gecko/41.0 Firefox/41.0")

  def fetchParcelContent[S](user: User, password: Password, year: Int)(implicit client: SttpBackend[IO, S]): IO[Either[String, Array[Byte]]] = {
    req
      .post(uri"""$url""")
      .body("name" -> user.id, "password" -> password.p)
      .response(ignore)
      .send[IO]().flatMap { r =>
      r.body match {
        case Right(()) => fetchParcelPdf(r, user, year)
        case Left(err) => IO.pure(Left(s"""Autentificarea taranului $user cu CNP-ul $password nu a functionat.${"\n" + err}"""))
      }
    }
  }

  private def fetchParcelPdf[T, S](res: Response[T], user: User, year: Int)(implicit client: SttpBackend[IO, S]): IO[Either[String, Array[Byte]]] = {
    req
      .cookies(res)
      .contentType("application/pdf")
      .response(ResponseAsByteArray)
      .get(uri"""https://lpis.apia.org.ro/reporting/$year/pdf/parcel_list.php?farmid=${user.id}&sirsup_code=&bloc_nr=""")
      .send[IO]().map(_.body)
  }
}
