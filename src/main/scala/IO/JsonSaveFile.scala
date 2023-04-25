package IO
import ATCG._
import io.circe.Decoder
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._
import java.io.{FileWriter, PrintWriter}
import scala.io.Source
import scala.util.Using


object JsonSaveFile {
  // encodes Player object into JSON
  def toJson(user: Player): String = user.asJson.noSpaces

  // creates a JSON file to a computer's directory (where the project is located) which has Player data stored
  def saveFile(file: String, json: String) = {
    Using(new PrintWriter(new FileWriter(file))) {
      writer => writer.println(json)
    }
  }

  // loads JSON file and decodes it into a Player object
  def loadUserFile(file: String): Option[Player] = {
    Using(Source.fromFile(file)) {
      source => decode[Player](source.getLines().mkString(""))
    }.toEither.flatten.toOption
  }

  case class MyCity(country: String, geonameid: Int, name: String, subcountry: String)
  // city data for departure and arrival aircraft
  def loadCityFile: List[MyCity] = {
     val inputFields: String =
       """
         |[{"country" : "Zimbabwe","geonameid" : 1085510,"name" : "Epworth","subcountry" : "Harare"},
         |{"country": "India", "geonameid": 1279064, "name": "Alandur", "subcountry": "Tamil Nadu"},
         |{"country": "Germany", "geonameid": 2866930, "name": "Nauen", "subcountry": "Brandenburg"},
         |{"country": "Germany", "geonameid": 2864053, "name": "Neustadt", "subcountry": "Bavaria"},
         |{"country": "France", "geonameid": 2988507, "name": "Paris", "subcountry": "\u00cele-de-France"},
         |{"country": "Spain", "geonameid": 3128760, "name": "Barcelona", "subcountry": "Catalonia"},
         |{"country": "Netherlands", "geonameid": 2759794, "name": "Amsterdam", "subcountry": "North Holland"},
         |{"country": "Russia", "geonameid": 498817, "name": "Moscow", "subcountry": "Moscow"},
         |{"country": "Japan", "geonameid": 1850147, "name": "Tokyo", "subcountry": "Tokyo"},
         |{"country": "Singapore", "geonameid": 1880252, "name": "Singapore", "subcountry": "Central Singapore"},
         |{"country": "Slovakia", "geonameid": 3060972, "name": "Bratislava", "subcountry": "Bratislavsk\u00fd"},
         |{"country": "Hungary", "geonameid": 3054643, "name": "Budapest", "subcountry": "Budapest"},
         |{"country": "Ukraine", "geonameid": 698740, "name": "Odessa", "subcountry": "Odessa"},
         |{"country": "Czech Republic", "geonameid": 3067696, "name": "Prague", "subcountry": "Praha"},
         |{"country": "Poland", "geonameid": 756135, "name": "Warsaw", "subcountry": "Masovian Voivodeship"},
         |{"country": "Portugal", "geonameid": 2267057, "name": "Lisbon", "subcountry": "Lisbon"},
         |{"country": "Belgium", "geonameid": 2800866, "name": "Brussels", "subcountry": "Brussels Capital"},
         |{"country": "Italy", "geonameid": 3169070, "name": "Rome", "subcountry": "Latium"},
         |{"country": "Italy", "geonameid": 3173435, "name": "Milano", "subcountry": "Lombardy"},
         |{"country": "Sweden", "geonameid": 3173435, "name": "Stockholm", "subcountry": "Stockholm"},
         |{"country": "Slovenia", "geonameid": 3173435, "name": "Sarajevo", "subcountry": "Sarajevo"},
         |{"country": "Croatia", "geonameid": 3173435, "name": "Zagreb", "subcountry": "Zagreb"},
         |{"country": "Bulgaria", "geonameid": 3173435, "name": "Sofia", "subcountry": "Sofia"},
         |{"country": "Austria", "geonameid": 3173435, "name": "Vienna", "subcountry": "Vienna"}
         |]""".stripMargin

     val inputFile: String = inputFields
     val fileSource = Using(Source.fromString(inputFile)) { source => source.getLines().mkString }

     // implemented implicit for practice purposes; the program will work also without it
     implicit val decodeMyConfig: Decoder[MyCity] =
      Decoder.forProduct4[MyCity, String, Int, String, String](
      "country",
      "geonameid",
      "name",
      "subcountry"
    ) { case (country, geo, name, sub) => MyCity(country, geo, name, sub) }

  decode[List[MyCity]](fileSource.get) match {
       case Left(decodingError) => throw new IllegalArgumentException(s"Invalid JSON object: ${decodingError.getMessage}")
       case Right(cityList) => cityList
     }
  }


}