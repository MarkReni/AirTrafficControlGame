package Auxiliary
import ATCG.{Aircraft, Command, Runway, Level}
import Data.{GameConstants, LevelsData}
import UI.{UI_AirportComponent, UI_GameMainWindow}
import org.joda.time.DateTime
import scala.collection.mutable.Queue
import scala.swing.ComboBox


// ANIMATION USING SWING TIMER
object RedoTimer {
  // UI_GameWindow uses this redoTimer to 1) advance movement of aircraft in the UI_AirportComponent  2) update dropdown list and in UI_InformationComponent
    /// 3) update aircraft info table window  4) update aircraft in UI_ScheduleComponent
  def redoTimerForMany(game: UI_GameMainWindow): Unit = {
    // aircraft movement for airport window
    this.checkAircraftToLeaving(game)
    val movingAirplanes: Queue[Aircraft] = game.airportComponent.airport.aircraftMoving
    if(movingAirplanes.nonEmpty) movingAirplanes.filter(_.status != "Gate").foreach(_.moveOneStepAirport())  // invoke moveOneStepAirport()
    // update aircraft for schedule window
    game.defaultTableModelDepartures.setRowCount(0)
    game.defaultTableModelArrivals.setRowCount(0)
    var scheduleText: String = ""
    for ((airplane, index) <- game.airportComponent.airport.aircraftAtGates.zipWithIndex) {
      // create airplane's departure text on schedule window
      scheduleText = getAirplaneScheduleText(airplane)
      game.defaultTableModelDepartures.addRow(Array[AnyRef](scheduleText))
    }
    for ((airplane, index) <- (game.airportComponent.airport.aircraftAtMissed ++ game.airportComponent.airport.aircraftAtRadar).zipWithIndex) {
      // create airplane's arrival text on schedule window
      scheduleText = getAirplaneScheduleText(airplane)
      game.defaultTableModelArrivals.addRow(Array[AnyRef](scheduleText))
    }
    // update aircraft info table window
    var infoText: String = ""
    game.defaultTableModelPlaneInfo.setRowCount(0)
    val allAircraft: Queue[Aircraft] = game.airportComponent.airport.aircraftAtGates ++ game.airportComponent.airport.aircraftMoving ++ game.airportComponent.airport.aircraftAtMissed ++ game.airportComponent.airport.aircraftAtRadar
    for ((airplane, index) <- allAircraft.zipWithIndex) {
      // create airplane's departure text on schedule window
      infoText = s""  // adds space between aircraft information text
      game.defaultTableModelPlaneInfo.addRow(Array[AnyRef](infoText))
      infoText = s"-${airplane.identifier}-"
      game.defaultTableModelPlaneInfo.addRow(Array[AnyRef](infoText))
      infoText = s"Aircraft type: ${airplane.aircraftType}"
      game.defaultTableModelPlaneInfo.addRow(Array[AnyRef](infoText))
      infoText = s"Passenger amount: ${airplane.passengerAmount}"
      game.defaultTableModelPlaneInfo.addRow(Array[AnyRef](infoText))
      infoText = s"Fuel status: ${airplane.fuelAmount}%"
      game.defaultTableModelPlaneInfo.addRow(Array[AnyRef](infoText))
    }
    // update dropdown list for command input
    val newListElements: Queue[String] = game.airportComponent.airport.aircraftMoving.map(_.identifier) ++ game.airportComponent.airport.aircraftAtRadar.map(_.identifier) ++ game.airportComponent.airport.aircraftAtGates.map(_.identifier) ++ game.airportComponent.airport.aircraftAtMissed.map(_.identifier)
    if(newListElements != game.informationComponent.listElements) {
      game.informationComponent.listElements = newListElements
      game.informationComponent.dropDownIdentifiers.peer.setModel(ComboBox.newConstantModel[String](game.informationComponent.listElements))
    }
    game.informationComponent.commandPanel.repaint()
    game.informationComponent.progressLabel.text = game.getProgressLabelText  // update statistics label text
    game.informationComponent.airportLevelLabel.text = game.getAirportLevelText   // update airport level's label text
    game.mainGameWindow.repaint()

  }

  private var addedTimeTimer: Option[DateTime] = None  // controls when the new aircraft should be created at the radar
  private var timeAdded: Boolean = false  // true if adedTimeTimer variable is activated
  def changeTimeAdded(): Unit = this.timeAdded = false  // resets the time added; used when "Return to lobby" is pressed
  // UI_GameWindow uses this redoTimer to advance movement of aircraft in the UI_RadarComponent
  def redoTimerRadarMovement(game: UI_GameMainWindow): Unit = {
    // aircraft movement for radar window
    if(!timeAdded) {
      val radarAirplanes: Queue[Aircraft] = game.airportComponent.airport.aircraftAtRadar
      addedTimeTimer = Some(TimeSurveiller.addTime(1, radarAirplanes.isEmpty, radarAirplanes.length))  // the more the aircraft the more minutes until next airplane will be created; affected by GameConstants
      timeAdded = true
    }
    this.checkAircraftToComing(game.airportComponent)
    val radarAirplanes: Queue[Aircraft] = game.airportComponent.airport.aircraftAtRadar
    val missedAirplanes: Queue[Aircraft] = game.airportComponent.airport.aircraftAtMissed
    if(radarAirplanes.nonEmpty) radarAirplanes.filter(x => x.status == "Approaching" || x.status == "RunwayReady" || x.status == "RunwayLand").foreach(_.moveOneStepRadar())
    if(missedAirplanes.nonEmpty) missedAirplanes.filter(x => x.status == "Approaching"|| x.status == "RunwayMissed" || x.status == "RunwayReady" || x.status == "RunwayLand").foreach(_.moveOneStepRadar())
  }

  // UI_GameWindow uses this redoTimer to advance Clock in the informationComponent
  def redoTimerClock(game: UI_GameMainWindow): Unit = game.informationComponent.clockLabel.text = Clock.advanceOneSec()

  // Level uses this redoTimer to start unstable weather conditions
  def redoTimerWeatherStart(game: UI_GameMainWindow): Unit = {
    if(Randomizer.randomOfTen < Level.getWeatherProb(game.airportText)) {
      val components: Vector[Runway] = game.airportComponent.airport.components.collect({ case runway: Runway => runway })
      val randomRunway: Runway = Randomizer.randomElement(components)
      Level.steptimerWeatherStart.stop()
      game.weatherStart = false
      randomRunway.closed = true
      Command.outputTextLabel(runwayName=randomRunway.name, index=3)
      game.airportComponent.repaint()
      Level.steptimerWeatherEnd.start()
      game.weatherEnd = true
    }
  }

  // Level uses this redoTimer to end unstable weather conditions
  def redoTimerWeatherEnd(game: UI_GameMainWindow): Unit = {
    if(Randomizer.randomOfTen == 0) {
      val components: Vector[Runway] = game.airportComponent.airport.components.collect({ case runway: Runway => runway })
      components.foreach(_.closed = false)
      Command.outputTextLabel(index=4)
      game.airportComponent.repaint()
      Level.steptimerWeatherEnd.stop()
      game.weatherEnd = false
      Level.steptimerWeatherStart.start()
      game.weatherStart = true
    }
  }

  // get airplane's display text for schedule window
  private def getAirplaneScheduleText(airplane: Aircraft): String = {
    val timeOfMoving: Option[DateTime] = airplane.timeOfMoving
    if(airplane.aircraftDestination == "Away") s"${airplane.identifier} to ${airplane.randomCity} ${if(timeOfMoving.isDefined) s"at ${Clock.displayTimeWithoutSecs(timeOfMoving.get.getHourOfDay, timeOfMoving.get.getMinuteOfHour)} " else ""}" +
      s"${if(airplane.getGateNo.isDefined) s"from ${airplane.getGateNo.get}" else ""}"
    else s"${airplane.identifier} from ${airplane.randomCity} ${if(timeOfMoving.isDefined) s"at ${Clock.displayTimeWithoutSecs(timeOfMoving.get.getHourOfDay, timeOfMoving.get.getMinuteOfHour)} " else ""}"
  }

  var readyToPushback: Boolean = false
  // function that moves an airplane to the queue of moving aircraft
  private def checkAircraftToLeaving(game: UI_GameMainWindow): Unit = {
    val airplanesAtGates: Queue[Aircraft] = game.airportComponent.airport.aircraftAtGates
    var nextToPushback: Option[Aircraft] = None
    if(airplanesAtGates.nonEmpty) {
      nextToPushback = Some(airplanesAtGates(0))
      nextToPushback.get.timeOfMoving match {
        case Some(time) => {
          if(time.getMillis <= Clock.getCurrentClock.getMillis & !readyToPushback) {
            Command.outputTextDeparting(nextToPushback.get, 0)
            nextToPushback.get.status = LevelsData.status(1)    // change status to GateReady
            readyToPushback = true
          }
        }
        case None => throw new Exception("No moving time defined for an aircraft.")
      }
    }
  }

  // function that moves an airplane to the queue of moving aircraft
  private def checkAircraftToComing(airportComponent: UI_AirportComponent): Unit = {
    if(Clock.getCurrentClock.getMillis >= this.addedTimeTimer.get.getMillis) {
      val aircraftAtRadar: Queue[Aircraft] = airportComponent.airport.aircraftAtRadar
      val addedRadarTime: Some[DateTime] = Some(addedTimeTimer.get.plusMinutes(GameConstants.timeToFlyAcrossRadar))
      if(aircraftAtRadar.isEmpty) airportComponent.airport.createAirplaneAtRadar(addedRadarTime)
      else {
        if(this.addedTimeTimer.get.plusMinutes(GameConstants.timeToFlyAcrossRadar).getMillis <= aircraftAtRadar.last.timeOfMoving.get.getMillis) throw new Exception("Failure at radar!")
        else airportComponent.airport.createAirplaneAtRadar(addedRadarTime)
      }
      this.timeAdded = false
    }
  }


}
