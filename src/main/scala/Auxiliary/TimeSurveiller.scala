package Auxiliary
import Data.GameConstants
import org.joda.time.DateTime
import java.awt.event.ActionListener
import java.util.Date
import javax.swing.Timer


object TimeSurveiller {
  // gets the current real time; used by Clock object
  def getRealTime: DateTime = {
    val dt: Date = new Date()
    val timeOfCreation: DateTime = new DateTime(dt)
    timeOfCreation
  }

  // gets current game time and adds some period to it after which the counter starts when to create a new airplane; used by RedoTimer object
  def addTime(mode: Int, isEmpty: Boolean, multiplier: Double): DateTime = {  // multiplier represents the amount of aircraft already created whether on radar or gates; the more the aircraft the more minutes until next airplane will be created
    mode match {
      case 0 => Clock.getCurrentClock.plusMinutes(Randomizer.getRandomMinsDeparture(isEmpty, multiplier + 1))  // mode == 0 for departure aircraft frequency
      case 1 => Clock.getCurrentClock.plusMinutes(Randomizer.getRandomMinsArrival(isEmpty, multiplier + 1))   // mode == 1 for arrival aircraft frequency
      case _ => throw new Exception("Clock not working properly!")
    }
  }

  // timer used by stepTimerAirport in UI_GameMainWindow class
  def stepTimerAirport(action: ActionListener): Timer = {
    new javax.swing.Timer(GameConstants.gameSpeedAirportComponent, action)  // milliseconds
  }

  // timer used by stepTimerRadar in UI_GameMainWindow class
  def stepTimerRadar(action: ActionListener): Timer = {
    new javax.swing.Timer(GameConstants.gameSpeedRadarComponent, action)  // milliseconds
  }

  // timer used by steptimerClock in UI_GameMainWindow class
  def stepTimerClock(action: ActionListener): Timer = {
    new javax.swing.Timer(GameConstants.gameSpeedClock, action)  // milliseconds
  }

  // timer used by steptimerWeatherStart in Level object
  def stepTimerWeatherStart(action: ActionListener): Timer = {
    new javax.swing.Timer(GameConstants.gameSpeedWeatherStart, action)  // milliseconds
  }

  // timer used by steptimerWeatherEnd in Level object
  def stepTimerWeatherEnd(action: ActionListener): Timer = {
    new javax.swing.Timer(GameConstants.gameSpeedWeatherEnd, action)  // milliseconds
  }


}
