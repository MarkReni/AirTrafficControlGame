package ATCG
import Auxiliary.{RedoTimer, TimeSurveiller}
import Data.{GameConstants, LevelsData}
import UI.UI_GameMainWindow
import javax.swing.Timer
import scala.swing.Swing


object Level {
  var game: Option[UI_GameMainWindow] = None
  def updateLevelParameters(newLevel: Int, airportName: String): Unit = GameConstants.changeLevel(LevelsData.difficultyData(airportName)(newLevel)._1)  // update aircraft creation frequency parameters in GameConstants
  def checkBoeing(airportName: String): Boolean = LevelsData.difficultyData(airportName)(game.get.playerLevels(airportName))._2  // checks if Boeing should be introduced to the airport
  def checkAirbus(airportName: String): Boolean = LevelsData.difficultyData(airportName)(game.get.playerLevels(airportName))._3  // checks if Airbus should be introduced to the airport
  def getBoeingAndAirbusProb(airportName: String): Int = LevelsData.difficultyData(airportName)(game.get.playerLevels(airportName))._4  // gives probability of Boeing and Airbus creation according to the level
  def getWeatherProb(airportName: String): Int = LevelsData.difficultyData(airportName)(game.get.playerLevels(airportName))._6  // gives probability of unstable weather according to the level

  // checks whether unstable weather should be introduced to the airport or not
  def checkWeather(airportName: String): Unit = {
    if(LevelsData.difficultyData(airportName)(this.game.get.playerLevels(airportName))._5) {
      if(!this.game.get.weatherStart && !this.game.get.weatherEnd) {
        this.steptimerWeatherStart.start()
        this.game.get.weatherStart = true
      }
    } else {
        this.steptimerWeatherStart.stop()
        this.steptimerWeatherEnd.stop()
        this.game.get.airportComponent.airport.runways.foreach(_.closed = false)
    }
  }
  val steptimerWeatherStart: Timer = TimeSurveiller.stepTimerWeatherStart(Swing.ActionListener(e => { RedoTimer.redoTimerWeatherStart(game.get) }))  // redoes redoTimerWeatherStart function every some instant
  val steptimerWeatherEnd: Timer = TimeSurveiller.stepTimerWeatherEnd(Swing.ActionListener(e => { RedoTimer.redoTimerWeatherEnd(game.get) }))  // redoes redoTimerWeatherEnd function every some instant


}
