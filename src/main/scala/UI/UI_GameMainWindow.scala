package UI
import ATCG._
import Auxiliary._
import Data._
import java.awt.Color
import javax.swing.Timer
import javax.swing.table.DefaultTableModel
import scala.swing._
import scala.swing.event._


case class UI_GameMainWindow(game: Game, airportText: String) extends SimpleSwingApplication {
   Command.game = Some(this)  // initiate mainGameWindow in the Command-object
   Level.game = Some(this)  // initiate mainGameWindow in the Level-object
   // AIRPORT WINDOW
   /// label for game progress
   def playerCoins = this.game.playerCoins
   def playerLevels = this.game.playerLevels

   /// MainGameWindow airportComponent: initializes UI_AirportComponent based on received parameter "airportText"
   val airportComponent: UI_AirportComponent = this.airportText match {
      case "Helsinki-Vantaa" => UI_AirportComponent(AirportHelsinki)
      case "Frankfurt" => UI_AirportComponent(AirportFrankfurt)
      case "Istanbul" => UI_AirportComponent(AirportIstanbul)
      case "Doha" => UI_AirportComponent(AirportDoha)
      case _ => {
        println(this.airportText)
        throw new Exception("No airport found")
      }
   }

   def getProgressLabelText = s"Destroyed aircraft: ${this.airportComponent.airport.destroyedAircraft}          Takeoffs: ${this.airportComponent.airport.takeoffCounter}           Coins:  ${playerCoins}"  // progress label on UI_InformationComponent
   def getAirportLevelText = s"Airport level:  ${playerLevels(airportText)}"  //level label on UI_InformationComponent

   /// MainGameWindow radarComponent: initializes UI_RadarComponent based on received parameter "airportText"
   val radarComponent: UI_RadarComponent = this.airportText match {
      case "Helsinki-Vantaa" => {
        AirportHelsinki.game = Some(this.game)
        UI_RadarComponent(AirportHelsinki)
      }
      case "Frankfurt" => {
        AirportFrankfurt.game = Some(this.game)
        UI_RadarComponent(AirportFrankfurt)
      }
      case "Istanbul" => {
        AirportIstanbul.game = Some(this.game)
        UI_RadarComponent(AirportIstanbul)
      }
      case "Doha" => {
        AirportDoha.game = Some(this.game)
        UI_RadarComponent(AirportDoha)
      }
      case _ => throw new Exception("No airport found")
   }

   /// MainGameWindow UI_Time
   val TimetableDepartures: UI_Timetable = UI_Timetable()
   val TimetableArrivals: UI_Timetable = UI_Timetable()
   val planeInfoComponent: UI_Timetable = UI_Timetable()
   planeInfoComponent.rowHeight = 20  // change row height and margins between rows
   val defaultTableModelDepartures: DefaultTableModel = this.TimetableDepartures.defaultTableModel
   val defaultTableModelArrivals: DefaultTableModel = this.TimetableArrivals.defaultTableModel
   val defaultTableModelPlaneInfo: DefaultTableModel = this.planeInfoComponent.defaultTableModel
   this.defaultTableModelDepartures.setColumnIdentifiers(Array[AnyRef]("Departures:"))
   this.defaultTableModelArrivals.setColumnIdentifiers(Array[AnyRef]("Arrivals:"))
   this.defaultTableModelPlaneInfo.setColumnIdentifiers(Array[AnyRef]("Aircraft info table:"))

   /// MainGameWindow UI_InformationComponent
   val informationComponent: UI_InformationComponent = new UI_InformationComponent(this)

   /// mainframe
   val backgroundPic: Image = Animator.scalePic("pics/airport_background_mainGameWindow.jpg", GameConstants.gameMainWindow_width, GameConstants.gameMainWindow_height)
   val mainGameWindow = new MainFrame {
     title = "Airport"
     background = GameConstants.background_color
     val width = Data.GameConstants.gameMainWindow_width
     val height = Data.GameConstants.gameMainWindow_height
     minimumSize = new Dimension(width, height)
     preferredSize = new Dimension(width, height)
     maximumSize = new Dimension(width, height)
     resizable = false

     val planeInfo: ScrollPane = new ScrollPane() {
            val width = 240
            val height = Data.GameConstants.timetableScroll_height + 400
            minimumSize = new Dimension(width, height)
            preferredSize = new Dimension(width, height)
            maximumSize = new Dimension(width, height)
            contents = planeInfoComponent
            verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
    }

     /// main window layout
      val mainWindowLayout = new GridPanel(2, 2) {
        contents += airportComponent
        contents += new BoxPanel(Orientation.Horizontal) {
          contents ++= Vector(planeInfo, radarComponent)
          background = new Color(0,65,106,150)
        }
        contents += informationComponent
        contents += new BoxPanel(Orientation.Horizontal) {
          val leftScroll: ScrollPane = new ScrollPane() {
            val width = Data.GameConstants.timetableScroll_width
            val height = Data.GameConstants.timetableScroll_height
            minimumSize = new Dimension(width, height)
            preferredSize = new Dimension(width, height)
            maximumSize = new Dimension(width, height)
            contents = TimetableDepartures
            verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
          }
          val rightScroll: ScrollPane = new ScrollPane() {
            val width = Data.GameConstants.timetableScroll_width
            val height = Data.GameConstants.timetableScroll_height
            minimumSize = new Dimension(width, height)
            preferredSize = new Dimension(width, height)
            maximumSize = new Dimension(width, height)
            contents = TimetableArrivals
            verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
          }
          contents ++= Vector(leftScroll, rightScroll)
        }
        border = Swing.EmptyBorder(5)
        override def paintComponent(g: Graphics2D): Unit = g.drawImage(backgroundPic, 0, 0, null)
      }

     contents = this.mainWindowLayout // contents of GridPanel
     peer.setLocationRelativeTo(null)
   }

   def top = this.mainGameWindow

   /// empty all existent moving aircraft and populate airport gates with stationary aircraft
   airportComponent.airport.resetAirport()  // resets airport before initiating it
   airportComponent.airport.populateGates()  // creates aircraft at the gates
   Level.checkWeather(airportName=airportText)  // initiate to check unstable weather conditins immediately the game starts
   Level.updateLevelParameters(newLevel=game.playerLevels(airportText), airportName=airportText)  // change the level parameters according to the airport's current level immediately the game starts

   val windowtBounds = mainGameWindow.bounds // window bounds
   val airportBounds = airportComponent.bounds // airport window bounds
   val radarBounds = radarComponent.bounds  // radar window bounds

   val stepTimerAirport: Timer = TimeSurveiller.stepTimerAirport(Swing.ActionListener(e => { RedoTimer.redoTimerForMany(this) }))  // redoes redoTimerForMany function every some instant
   val stepTimerRadar: Timer = TimeSurveiller.stepTimerRadar(Swing.ActionListener(e => { RedoTimer.redoTimerRadarMovement(this) }))  // redoes redoTimerRadarMovement function every some instant
   val steptimerClock: Timer = TimeSurveiller.stepTimerClock(Swing.ActionListener(e => { RedoTimer.redoTimerClock(this) }))  // redoes redoTimerClock function every some instant
   // start timers
   stepTimerAirport.start()
   stepTimerRadar.start()
   steptimerClock.start()

   // EVENT HANDLING
   this.listenTo(informationComponent.speedGameButton)
   this.listenTo(informationComponent.lobbyButton)
   this.listenTo(informationComponent.pauseGameButton)
   this.listenTo(informationComponent.commandAction)
   this.listenTo(informationComponent.dropDownCommands)

   var togglePause: Int = 0  // toggle variable for pause button
   var toggleSpeed: Int = 0  // toggle variable for speed button
   var weatherStart: Boolean = false  // necessary for "Pause" button and weather checking in Level object
   var weatherEnd: Boolean = false  // necessary for "Pause" button and weather checking in Level object
   this.reactions += {
      case clickEvent: ButtonClicked => {
        val buttonClicked = clickEvent.source
        val text = buttonClicked.text
        if(text == "Return to lobby") {
          stepTimerAirport.stop()
          stepTimerRadar.stop()
          steptimerClock.stop()
          Clock.resetClock()
          RedoTimer.changeTimeAdded()
          this.mainGameWindow.close()
          new UI_GameOpeningWindow(this.game).top.visible = true
        } else if(text == "Pause Game" || text == "Resume Game") {
          togglePause = (togglePause + 1) % 2
          if(togglePause == 0) {
            stepTimerAirport.start()
            stepTimerRadar.start()
            steptimerClock.start()
            if(weatherStart) Level.steptimerWeatherStart.start()
            if(weatherEnd) Level.steptimerWeatherEnd.start()
            informationComponent.pauseGameButton.text = "Pause Game"
          } else {
            stepTimerAirport.stop()
            stepTimerRadar.stop()
            steptimerClock.stop()
            if(weatherStart) Level.steptimerWeatherStart.stop()
            if(weatherEnd) Level.steptimerWeatherEnd.stop()
            informationComponent.pauseGameButton.text = "Resume Game"
          }
        } else if(text == "Fast Game" || text == "Slow Game") {
            toggleSpeed = (toggleSpeed + 1) % 2
            if(toggleSpeed == 0) {  // slows the game to normal speed
              stepTimerAirport.setDelay(GameConstants.gameSpeedAirportComponent)
              stepTimerRadar.setDelay(GameConstants.gameSpeedRadarComponent)
              steptimerClock.setDelay(GameConstants.gameSpeedClock)
              Level.steptimerWeatherStart.setDelay(GameConstants.gameSpeedWeatherStart)
              Level.steptimerWeatherEnd.setDelay(GameConstants.gameSpeedWeatherEnd)
              informationComponent.speedGameButton.tooltip = "Press to speed the game up"
              informationComponent.speedGameButton.text = "Fast Game"
          } else {  /// speeds the game up by x4
            stepTimerAirport.setDelay(GameConstants.gameSpeedAirportComponent / GameConstants.delayComponent)
            stepTimerRadar.setDelay(GameConstants.gameSpeedRadarComponent / GameConstants.delayComponent)
            steptimerClock.setDelay(GameConstants.gameSpeedClock / GameConstants.delayComponent)
            Level.steptimerWeatherStart.setDelay(GameConstants.gameSpeedWeatherStart / GameConstants.delayComponent)
            Level.steptimerWeatherEnd.setDelay(GameConstants.gameSpeedWeatherEnd / GameConstants.delayComponent)
            informationComponent.speedGameButton.tooltip = "Press to return to normal speed"
            informationComponent.speedGameButton.text = "Slow Game"   // space added to the end of text due to positioning reasons
          }
        } else if(text == "Action") {  // Action button is pressed
           var selectedCommand: Option[String] = None
           var selectedComponent: Option[String] = None
           var selectedAircraft: Option[String] = None
           val listElements = airportComponent.airport.aircraftMoving.map(_.identifier) ++ airportComponent.airport.aircraftAtRadar.map(_.identifier) ++ airportComponent.airport.aircraftAtGates.map(_.identifier) ++ airportComponent.airport.aircraftAtMissed.map(_.identifier)
           if(listElements.nonEmpty) {
             selectedCommand = Some(informationComponent.dropDownCommands.peer.getSelectedItem.toString)
             selectedComponent = Some(informationComponent.dropDownComponents.peer.getSelectedItem.toString)
             selectedAircraft= Some(informationComponent.dropDownIdentifiers.peer.getSelectedItem.toString)
           }
           Command.nextCommand(selectedCommand, selectedComponent, selectedAircraft)
        }
      }
   }


}
