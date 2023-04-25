package Auxiliary
import ATCG.Runway
import Data.GameConstants
import IO.JsonSaveFile
import IO.JsonSaveFile.MyCity
import scala.util.Random


object Randomizer {
  def getRandomIndex(componentsNotOccupied: Int): Int = Random.between(0, componentsNotOccupied)  // randomize at which gate the aircraft gets created

  // randomizes when the next aircraft is ready for departure from the gate
  def getRandomMinsDeparture(isEmpty: Boolean, multiplier: Double): Int = {
    if(isEmpty) GameConstants.aircraftDepartureMinFrequency + Random.nextInt(GameConstants.aircraftDepartureMaxFrequency - GameConstants.aircraftDepartureMinFrequency)
    else (GameConstants.aircraftDepartureMinFrequencyNonEmpty * multiplier).toInt + Random.nextInt(GameConstants.aircraftDepartureMaxFrequencyNonEmpty - GameConstants.aircraftDepartureMinFrequencyNonEmpty)
  }

  // randomizes when the next aircraft gets created at the radar
  def getRandomMinsArrival(isEmpty: Boolean, multiplier: Double): Int = {
    if(isEmpty) GameConstants.aircraftArrivalMinFrequency + Random.nextInt(GameConstants.aircraftArrivalMaxFrequency - GameConstants.aircraftArrivalMinFrequency)
    else (GameConstants.aircraftArrivalMinFrequencyNonEmpty * multiplier).toInt + Random.nextInt(GameConstants.aircraftArrivalMaxFrequencyNonEmpty - GameConstants.aircraftArrivalMinFrequencyNonEmpty)
  }

  def getRandomOfTwo: Int = Random.between(0, 2)  // randomize 0 and 1

  // randomizes a letter char A - Z for the aircraft identifier
  def randomLetter: Char = {
    val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val randomIndex = Random.between(0, letters.length)
    letters(randomIndex)
  }

  // randomizes a numerical char 0-9 for the aircraft identifier
  def randomInteger: Char = {
    val numbers ="1234567890"
    val randomIndex = Random.between(0, numbers.length)
    numbers(randomIndex)
  }

  def randomPassangers(min: Int, max: Int): Int = Random.between(min, max)  // random passanger amount in the aircraft; depends on aircraft type

  def randomOfTen: Int = Random.between(0, 11)  // random integer between 0 - 10

  def randomDouble: Double = Random.between(35.0, 45.0)   // randomize double relating to arriving aircraft fuel amount

  // randomizes a city the aircraft is flying to or arriving from
  def randomCity: String = {
    val cities: List[MyCity] = JsonSaveFile.loadCityFile
    val randomIndex = Random.between(0, cities.length)
    cities(randomIndex).name
  }

  def randomElement(elements: Seq[Runway]): Runway = elements(Random.nextInt(elements.length))  // randomize an element from the Seq given as a parameter


}
