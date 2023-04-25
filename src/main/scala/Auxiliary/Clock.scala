package Auxiliary
import org.joda.time.DateTime


object Clock {
  var clockStart: DateTime = TimeSurveiller.getRealTime  // initialize the real time based on which the game clock is started
  def seconds: Int = clockStart.getSecondOfMinute  // gets the seconds of the game clock
  def minutes: Int = clockStart.getMinuteOfHour  // gets the minutes of the game clock
  def hours: Int = clockStart.getHourOfDay  // gets the hours of the game clock
  def resetClock(): Unit = this.clockStart = TimeSurveiller.getRealTime  // resets the game clock to match the real time
  def getCurrentClock: DateTime = this.clockStart  // returns the current time of the game

  // advance the game clock by one second
  def advanceOneSec(): String = {
    this.clockStart = this.clockStart.plusSeconds(1)
    this.displayTime(this.hours, this.minutes, this.seconds)
  }

  // display the game clock as a string; used by the above function
  private def displayTime(hours: Int, minutes: Int, seconds: Int): String = {
    if(hours < 10 && minutes < 10 && seconds < 10) s"0${hours}:0${minutes}:0${seconds}"
    else if(hours < 10 && minutes < 10) s"0${hours}:0${minutes}:${seconds}"
    else if(hours < 10 && seconds < 10) s"0${hours}:${minutes}:0${seconds}"
    else if(minutes < 10 && seconds < 10) s"${hours}:0${minutes}:0${seconds}"
    else if(minutes < 10) s"${hours}:0${minutes}:${seconds}"
    else if(hours < 10) s"0${hours}:${minutes}:${seconds}"
    else if(seconds < 10) s"${hours}:${minutes}:0${seconds}"
    else s"${hours}:${minutes}:${seconds}"
  }

  // display the game clock as a string without seconds; needed for timetable windows
  def displayTimeWithoutSecs(hours: Int, minutes: Int): String = {
    if(hours < 10 && minutes < 10 && seconds < 10) s"0${hours}:0${minutes}"
    else if(hours < 10 && minutes < 10) s"0${hours}:0${minutes}"
    else if(hours < 10 && seconds < 10) s"0${hours}:${minutes}"
    else if(minutes < 10 && seconds < 10) s"${hours}:0${minutes}"
    else if(minutes < 10) s"${hours}:0${minutes}"
    else if(hours < 10) s"0${hours}:${minutes}"
    else if(seconds < 10) s"${hours}:${minutes}"
    else s"${hours}:${minutes}"
  }


}
