package UI
import ATCG._
import Auxiliary.Clock
import Data.GameConstants
import java.awt.Color
import java.awt.event.ActionEvent
import scala.collection.mutable.Queue
import scala.swing._


class UI_InformationComponent(game: UI_GameMainWindow) extends BoxPanel(Orientation.Vertical) {
   val outputLabel = new Label {   // outputs all text related to general information
          text = s"Welcome to the airport of ${game.airportComponent.airport.airportName}"
          minimumSize = new Dimension(GameConstants.statisticsLabel_width, GameConstants.statisticsLabel_height)
          preferredSize = new Dimension(GameConstants.statisticsLabel_width, GameConstants.statisticsLabel_height)
          maximumSize = new Dimension(GameConstants.statisticsLabel_width, GameConstants.statisticsLabel_height)
          tooltip = "Command output window"
          opaque = true
          background = new Color(192,192,192,150)
          horizontalAlignment = Alignment.Center
          verticalAlignment = Alignment.Center
    }

   val progressLabel = new Label {  // displays game statistics
     text = game.getProgressLabelText
     minimumSize = new Dimension(GameConstants.statisticsLabel_width, GameConstants.statisticsLabel_height)
     preferredSize = new Dimension(GameConstants.statisticsLabel_width, GameConstants.statisticsLabel_height)
     maximumSize = new Dimension(GameConstants.statisticsLabel_width, GameConstants.statisticsLabel_height)
     opaque = true
   }

   val progressOutputLabel = new BoxPanel(Orientation.Vertical) {
        contents ++= Vector(outputLabel, progressLabel)
        opaque = false
      }

   val airportLevelLabel = new Label {  // displays the airport's current level
        text = game.getAirportLevelText
        minimumSize = new Dimension(150, 30)
        preferredSize = new Dimension(150, 30)
        maximumSize = new Dimension(150, 30)
        opaque = true
     }

   val clockLabel = new Label {  // displays the game clock
        text = s"${Clock.hours}:${Clock.minutes}:${Clock.seconds}"
        minimumSize = new Dimension(150, 30)
        preferredSize = new Dimension(150, 30)
        maximumSize = new Dimension(150, 30)
        opaque = true
     }

   val levelClockPanel =  new BoxPanel(Orientation.Vertical) {
     tooltip = "Level panel displays airport's current level and game's internal clock"
     contents ++= Vector(airportLevelLabel, clockLabel)
     opaque = false
   }

   val progressPanel = new BoxPanel(Orientation.Horizontal) {   //  joined statisticslabel with new Panel for better positioning relative to the buttons
     tooltip = "Progress panel displays amount of missed arrivals, amount of successful takeoffs and accumulated coins"
     contents ++= Vector(progressOutputLabel, new Panel {
       minimumSize = new Dimension(10, 30)
       preferredSize = new Dimension(10, 30)
       maximumSize = new Dimension(10, 30)
       opaque = false
     }, levelClockPanel)
     opaque = false
     border = Swing.EmptyBorder(5)
   }
   /// buttons
   val pauseGameButton = new Button("Pause Game") { tooltip = "Press to pause the game" }  // pauses the game by stopping all the timers
   val lobbyButton = new Button("Return to lobby") { tooltip = "Press to return to the lobby" }  // goes back to UI_GameOpeningWindow
   val speedGameButton = new Button("Fast Game") { tooltip = "Press to speed up the game by x4" } // speeds the game by x4
   val buttons = new BoxPanel(Orientation.Horizontal) {
        contents ++= Vector(speedGameButton, new Panel {
          opaque = false;
          maximumSize = new Dimension(GameConstants.buttons_separator_width, GameConstants.buttons_separator_height)
        }, pauseGameButton, new Panel {
          opaque = false;
          maximumSize = new Dimension(GameConstants.buttons_separator_width, GameConstants.buttons_separator_height)
        }, lobbyButton)
        minimumSize = new Dimension(GameConstants.buttons_width, GameConstants.buttons_height)
        preferredSize = new Dimension(GameConstants.buttons_width, GameConstants.buttons_height)
        maximumSize = new Dimension(GameConstants.buttons_width, GameConstants.buttons_height)
        opaque = false
   }
   /// command related
   val dropDownCommands = new ComboBox[String](Command.commands) {  // dropdown table for commands
        minimumSize = new Dimension(190, 40)
        preferredSize = new Dimension(190, 40)
        maximumSize = new Dimension(190, 40)
        tooltip = "Select a command to be performed"
    }
    dropDownCommands.peer.addActionListener((e: ActionEvent) => itemSelected())   // Actionlistener for command dropdown table

  private def itemSelected(): Unit = {  // based on the selected command decides whether names of gates or runways should be shown
        val selectedCommand: String = dropDownCommands.peer.getSelectedItem.toString
        if(selectedCommand == "Move to gate") {
            components = game.airportComponent.airport.gates.map(_.name)
            dropDownComponents.peer.setModel(ComboBox.newConstantModel[String](components))
          } else if(selectedCommand == "Move to runway (land)" | selectedCommand == "Move to takeoff") {
            components = game.airportComponent.airport.runways.filterNot(_.closed).map(_.name)
            dropDownComponents.peer.setModel(ComboBox.newConstantModel[String](components))
          } else {
            components = Vector[String]("")
            dropDownComponents.peer.setModel(ComboBox.newConstantModel[String](components))
          }
      }

    var listElements: Queue[String] = game.airportComponent.airport.aircraftMoving.map(_.identifier) ++ game.airportComponent.airport.aircraftAtRadar.map(_.identifier) ++  // initialize aircraft identifiers for dropdown table
      game.airportComponent.airport.aircraftAtGates.map(_.identifier) ++ game.airportComponent.airport.aircraftAtMissed.map(_.identifier)

    val dropDownIdentifiers = new ComboBox[String](listElements) {  // dropdown table for aircraft identifiers
        minimumSize = new Dimension(150, 40)
        preferredSize = new Dimension(150, 40)
        maximumSize = new Dimension(150, 40)
        tooltip = "Select an aircraft to which commands will be targeted"
    }
    var components: Vector[String] = Vector[String]("")
    val dropDownComponents = new ComboBox[String](components) {  // dropdown table for gate/runway names
          minimumSize = new Dimension(150, 40)
          preferredSize = new Dimension(150, 40)
          maximumSize = new Dimension(150, 40)
          tooltip = "Select a gate/runway to which the command will be targeted"
      }
    val commandAction = new Button("Action") { tooltip = "Press to perform the command" }  // Action button
    val commandPanel = new BoxPanel(Orientation.Horizontal) {
         tooltip = "Input command here to control airtraffic"
         contents ++= Vector(dropDownCommands, dropDownComponents, dropDownIdentifiers, commandAction)
         opaque = false
    }

  val outputLabelDeparting = new Label {  // outputs all text related to the departing aircraft
          text = s""
          minimumSize = new Dimension(280, 150)
          preferredSize = new Dimension(280, 150)
          maximumSize = new Dimension(280, 150)
          tooltip = "Command output window for departing aircraft"
          opaque = true
          background = new Color(192,192,192,150)
          horizontalAlignment = Alignment.Left
          verticalAlignment = Alignment.Top
    }

  val outputLabelArriving = new Label {  // outputs all text related to the arriving aircraft
          text = s""
          minimumSize = new Dimension(280, 150)
          preferredSize = new Dimension(280, 150)
          maximumSize = new Dimension(280, 150)
          tooltip = "Command output window for arriving aircraft"
          opaque = true
          background = new Color(192,192,192,150)
          horizontalAlignment = Alignment.Left
          verticalAlignment = Alignment.Top
    }

  val commandPanelOutput = new BoxPanel(Orientation.Vertical) {
    contents ++= Vector(
          commandPanel,
          new Panel {
          minimumSize = new Dimension(550, 10)
          preferredSize = new Dimension(550, 10)
          maximumSize = new Dimension(550, 10)
          opaque = false
         }, new BoxPanel(Orientation.Horizontal) {
          contents ++= Vector(outputLabelDeparting, new Panel {
              minimumSize = new Dimension(10, 90)
              preferredSize = new Dimension(10, 90)
              maximumSize = new Dimension(10, 90)
              opaque = false
         }, outputLabelArriving)
          opaque = false
      })
    opaque = false
  }

  contents ++= Vector(progressPanel, new Panel {
          minimumSize = new Dimension(GameConstants.airportWindow_width, 30)
          preferredSize = new Dimension(GameConstants.airportWindow_width, 30)
          maximumSize = new Dimension(GameConstants.airportWindow_width, 30)
          opaque = false
        }, commandPanelOutput, new Panel {
          minimumSize = new Dimension(GameConstants.airportWindow_width, 30)
          preferredSize = new Dimension(GameConstants.airportWindow_width, 30)
          maximumSize = new Dimension(GameConstants.airportWindow_width, 30)
          opaque = false
        }, buttons)
        minimumSize = new Dimension(GameConstants.informationPanel_width, GameConstants.informationPanel_height)
        preferredSize = new Dimension(GameConstants.informationPanel_width, GameConstants.informationPanel_height)
        maximumSize = new Dimension(GameConstants.informationPanel_width, GameConstants.informationPanel_height)
        background = new Color(255, 255, 255, 60)
        peer.setLocation(0, 0)


}
