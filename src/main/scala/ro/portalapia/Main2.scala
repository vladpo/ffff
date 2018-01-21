package ro.portalapia

object Main2 extends App {

  println(toUnicode("Luna/ Perioada"))
  println(toUnicode("Efectiv la începutul perioadei (cap.)"))
  println(toUnicode("Total intrări (cap.)"))
  println(toUnicode("Total ieşiri (cap.)"))
  println(toUnicode("Efectiv la sfârşitul perioadei (cap.)**)"))
  println(toUnicode("Zile animale furajate (nr.)"))
  println(toUnicode("Efectiv mediu furajat (cap)"))
  println(toUnicode("Efectiv mediu furajat (UVM)"))
  println(toUnicode("Consum lucernă/soia/fân/mazare boabe total pe UVM (to)"))
  println(toUnicode("*) Calculul efectivului mediu furajat din fermă se completează lunar şi anual pe fiecare specie şi categorie de animale."))
  println(toUnicode("**) (col. 5) efectiv la sfârşitul perioadei reprezintă şi efectiv la începutul lunii/perioadei următoare"))
  println(toUnicode("(col. 6) Zile animale furajate lunar = (col. 2 X nr. zile lună) + (col. 3. X 0,5 X nr. zile lună) - (col. 4 X 0,5 X nr. zile lună)"))
  println(toUnicode("(col. 6) Zile animale furajate an/total perioadă = (col. 2 X 365 sau nr. zile perioadă) + (col. 3 X 0,5 X 365 sau nr. zile perioadă) -"))
  println(toUnicode("(col. 4 X 0,5 X 365 sau nr. zile perioadă)"))
  println(toUnicode("(col. 7) Efectivul mediu furajat lunar = (col. 6) ÷ nr. zile lună"))
  println(toUnicode("(col. 7) Efectivul mediu furajat an/total perioadă = (col. 6) - 365 zile (sau nr. zile perioadă)."))
  println(toUnicode("(col. 8) = col. (7) X coef. de transformare în UVM al categoriei de animale respective din anexa nr. 5 la Ordinul nr. 619/2015, cu"))
  println(toUnicode("modificarile şi completările ulterioare"))
  println(toUnicode("(col. 9) consum lucernă/soia/fân lunar = (col. 8) X cantitatea de lucernă/soia consumată pe UVM stabilită prin raţie de către fermier"))
  println(toUnicode("X nr. zile lună"))
  println(toUnicode("(col. 9) consum lucernă/soia/fân an/total perioadă = (col. 8) X cantitatea de lucernă/soia consumată pe UVM stabilită prin raţie de"))
  println(toUnicode("către fermier X 365 zile (sau nr. zile perioadă). Cantitatea calculată nu poate depăşi cantitatea maximă/UVM/an prevăzută în anexa nr."))
  println(toUnicode("5 la Ordinul ministrului agriculturii şi dezvoltării rurale nr. 619/2015 pentru aprobarea criteriilor de eligibilitate, condiţiilor specifice"))
  println(toUnicode("şi a modului de implementare a schemelor de plăţi prevăzute la art. 1 alin. (2) şi (3) din Ordonanţa de urgenţă a Guvernului nr. 3/2015"))
  println(toUnicode("pentru aprobarea schemelor de plăţi care se aplică în agricultură în perioada 2015 - 2020 şi pentru modificarea art. 2 din Legea nr."))
  println(toUnicode("36/1991 privind societăţile agricole şi alte forme de asociere în agricultură, precum şi a condiţiilor specifice de implementare pentru"))
  println(toUnicode("măsurile compensatorii de dezvoltare rurală aplicabile pe terenurile agricole, prevăzute în Programul Naţional de Dezvoltare Rurală"))
  println(toUnicode("2014 - 2020, cu modificările şi completările ulterioare."))
  println(toUnicode("  la Ordinul nr. 619/2015, cu modificarile şi completările ulterioare))"))
  println(toUnicode("Calculul efectivului mediu furajat din fermă şi producţia de lucernă/soia/fân*)"))
  println(toUnicode("ţ"))
  println(toUnicode("Producția de lucernă/soia/fân și calculul utilizării acesteia"))
  println(toUnicode("Producția lucernă/soia/fân realizată (tone)"))
  def toUnicode(s: String): String = {
    s.map { c =>
      if (c > 31 && c < 127)
        c
      else
        "\\u%04x".format(c.toInt)
    }.mkString
  }
}
