package ATCG
import Auxiliary.RedoTimer
import Data.LevelsData
import UI.UI_GameMainWindow


object Command {
    var game: Option[UI_GameMainWindow] = None
    val commands: Vector[String] = Vector("Move to pushback", "Move to takeoff", "Move to runway (land)", "Move to runway (takeoff)", "Move to gate")
    // output text to departing output screen
    def outputTextDeparting(aircraft: Aircraft, index: Int): Unit = {
      val texts: Vector[String] = Vector(s"${aircraft.identifier} is ready for pushback.", s"${aircraft.identifier} is ready to move to takeoff.",
        s"${aircraft.identifier} is ready to move to runway.", s"Tower can you read me, Captain of ${aircraft.aircraftType} ${aircraft.identifier} speaking here. We are about to run out of the fuel...",
        "Boom!", s"${aircraft.aircraftType} ${aircraft.identifier} collided. Truck carriers removed it from the airport.")
      val oldText: String = game.get.informationComponent.outputLabelDeparting.text
      game.get.informationComponent.outputLabelDeparting.text = "<html>" + texts(index) + "<br><br>" + oldText.replace("<html>", "") + "<html>"
    }

   // output text to arriving output screen
   def outputTextArriving(aircraft: Aircraft, index: Int): Unit = {
      val texts: Vector[String] = Vector(s"${aircraft.identifier} is ready to land.", s"${aircraft.identifier} is approaching.", s"${aircraft.identifier} missed landing. Let's try soon again.",
        s"Tower can you read me, Captain of ${aircraft.aircraftType} ${aircraft.identifier} speaking here. We are about to run out of the fuel...", "Boom!",
        s"${aircraft.identifier} missed landing. Let's try soon again.", s"Captain of ${aircraft.aircraftType} ${aircraft.identifier} speaking here. What is our gate number?",
        s"Thank you tower! ${aircraft.aircraftType} ${aircraft.identifier} has arrved safely to ${game.get.airportComponent.airport.airportName} with ${aircraft.passengerAmount} passengers.",
        s"${aircraft.aircraftType} ${aircraft.identifier} collided. Truck carriers removed it from the airport.")
      val oldText: String = game.get.informationComponent.outputLabelArriving.text
      game.get.informationComponent.outputLabelArriving.text = "<html>" + texts(index) + "<br><br>" + oldText.replace("<html>", "") + "<html>"
    }

    // output text to the general output screen (the one that outputs "Welcome to the airport of ...")
    def outputTextLabel(level: => Int = 0, runwayName: => String = "", index: Int): Unit = {    // by-name parameter
        val texts: Vector[String] = Vector(s"Congratulations level of airport ${game.get.airportComponent.airport.airportName} is now ${level}", "Due to collision takeoffs are reset",
          "Max destroyed aircraft reached, takeoffs are reset", s"Runway ${runwayName} is now closed due to weather conditions", s"All runways are now open")
        game.get.informationComponent.outputLabel.text = texts(index)
      }

    // moves the aircraft based on given commands
    def nextCommand(command: Option[String], component: Option[String], airplaneIdentifier: Option[String]): Unit = {
      game match {
        case Some(gameWindow) => {
          if(airplaneIdentifier.isDefined) {
            val allAircraft: Vector[Aircraft] = (gameWindow.airportComponent.airport.aircraftAtRadar ++ gameWindow.airportComponent.airport.aircraftMoving ++ gameWindow.airportComponent.airport.aircraftAtGates ++ gameWindow.airportComponent.airport.aircraftAtMissed).toVector
            val aircraft: Option[Aircraft] = allAircraft.find(_.identifier == airplaneIdentifier.get)
            aircraft match {
              case Some(airplane) => {
                val airplaneStatus = airplane.status
                val oldTextDeparting: String = game.get.informationComponent.outputLabelDeparting.text
                val oldTextArriving: String = game.get.informationComponent.outputLabelArriving.text
                airplaneStatus match {
                  case "Gate" => gameWindow.informationComponent.outputLabelDeparting.text = "<html>" + s"Too soon for ${airplane.aircraftType} ${airplane.identifier} to move." + "<br><br>" + oldTextDeparting.replace("<html>", "") + "<html>"
                  case "GateReady" => {
                    if(command.get != "Move to pushback") gameWindow.informationComponent.outputLabelDeparting.text = "<html>" + s"Captain of the ${airplane.aircraftType} ${airplane.identifier} speaking to the tower here, we should be push backed!" + "<br><br>" + oldTextDeparting.replace("<html>", "") + "<html>"
                    else {
                      game.get.airportComponent.airport.selectNextAirplane()
                      airplane.status = LevelsData.status(2)  // change airplane status to "Pushback"
                      RedoTimer.readyToPushback = false
                      gameWindow.informationComponent.outputLabelDeparting.text = "<html>" + s"Thank you tower, ${airplane.aircraftType} ${airplane.identifier} is pushing back..." + "<br><br>" + oldTextDeparting.replace("<html>", "") + "<html>"
                    }
                  }
                  case "Pushback" => {
                    if(command.get != "Move to takeoff") gameWindow.informationComponent.outputLabelDeparting.text = "<html>" + s"Captain of ${airplane.aircraftType} ${airplane.identifier} speaking to the tower here, we should be moved to take off! Which is our runway?" + "<br><br>" + oldTextDeparting.replace("<html>", "") + "<html>"
                    else {
                      airplane.nextComponentName = component.get    // remember the name of the next component the aircraft is going to
                      val runwayComponent: Option[Runway] = gameWindow.airportComponent.airport.runways.find(_.name == component.get)
                      if(runwayComponent.get.closed) gameWindow.informationComponent.outputLabelDeparting.text = "<html>" + s"Tower can you read me, runway ${component.get} seems to be closed due to weather conditions, could we get another runway?" + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                      else {
                        airplane.status = LevelsData.status(3)  // change airplane status to "Takeoff"
                        gameWindow.informationComponent.outputLabelDeparting.text = "<html>" + s"Thank you tower, ${airplane.aircraftType} ${airplane.identifier} is moving to takeoff via runway ${component.get}." + "<br><br>" + oldTextDeparting.replace("<html>", "") + "<html>"
                      }
                    }
                  }
                  case "Takeoff" => {
                    if(command.get != "Move to runway (takeoff)") gameWindow.informationComponent.outputLabelDeparting.text = "<html>" + s"Captain of ${airplane.aircraftType} ${airplane.identifier} speaking to the tower here, I am repeating, we are ready to take off!" + "<br><br>" + oldTextDeparting.replace("<html>", "") + "<html>"
                    else {
                      airplane.status = LevelsData.status(4)  // change airplane status to "RunwayTakeoff"
                      gameWindow.informationComponent.outputLabelDeparting.text = "<html>" + s"Thank you tower, ${airplane.aircraftType} ${airplane.identifier} is moving to runway to take off..." + "<br><br>" + oldTextDeparting.replace("<html>", "") + "<html>"
                    }
                  }
                  case "RunwayTakeoff" => gameWindow.informationComponent.outputLabelDeparting.text = "<html>" + s"${airplane.aircraftType} ${airplane.identifier} is already flying to its destination..." + "<br><br>" + oldTextDeparting.replace("<html>", "") + "<html>"
                  case "RunwayLand" => {
                    if(airplane.currentComponent == null) gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Captain of ${airplane.aircraftType} ${airplane.identifier} speaking to the tower here, we have not arrived to the airport yet." + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                    else if(command.get != "Move to gate") gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Captain of ${airplane.aircraftType} ${airplane.identifier} speaking to the tower here, can I please get our gate number?" + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                    else {
                      val selectedGate: Option[Gate] = game.get.airportComponent.airport.gates.find(_.name == component.get)
                      selectedGate match {
                        case Some(gate) => {
                          if(gate.occupied) gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Captain of ${airplane.aircraftType} ${airplane.identifier} speaking to the tower here, gate ${gate.name} is occupied, is there any available gates?" + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                          else {
                            if(gate.suitableAircraft.length == 1 && airplane.aircraftType != "Bombardier") gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Captain of ${airplane.aircraftType} ${airplane.identifier} speaking to the tower here, gate ${gate.name} is not suitable for our ${airplane.aircraftType}, is there any available gates suitable for us?" + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                            else if(gate.suitableAircraft.length == 2 && airplane.aircraftType == "Airbus") gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Captain of ${airplane.aircraftType} ${airplane.identifier} speaking to the tower here, gate ${gate.name} is not suitable for our ${airplane.aircraftType}, is there any available gates suitable for us?" + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                            else {
                              gate.occupied = true
                              airplane.status = LevelsData.status(5)  // change airplane status to "HomeGate"
                              airplane.nextComponentName = component.get    // remember the name of the next component the aircraft is going to
                              gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Thank you tower, ${airplane.aircraftType} ${airplane.identifier} is moving to the gate ${gate.name}..." + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                            }
                          }
                        }
                        case None => throw new Exception("No gate defined.")
                      }
                    }
                  }
                  case "HomeGate" => gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"${airplane.aircraftType} ${airplane.identifier} is already moving to the gate ${component.get}..." + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                  case "RunwayReady" => {
                    if(command.get != "Move to runway (land)") gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Captain of ${airplane.aircraftType} ${airplane.identifier} speaking to the tower here, we are ready to land!" + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                    else {
                      airplane.nextComponentName = component.get    // remember the name of the next component the aircraft is going to
                      val runwayComponent: Option[Runway] = gameWindow.airportComponent.airport.runways.find(_.name == component.get)
                      if(runwayComponent.get.closed) gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Tower can you read me, runway ${component.get} seems to be closed due to weather conditions, could we get another runway?" + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                      else {
                        airplane.status = LevelsData.status(6)  // change airplane status to "RunwayLand"
                        gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Thank you tower, ${airplane.aircraftType} ${airplane.identifier} preparing to land at runway ${component.get}..." + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                      }
                    }
                  }
                  case "Approaching" => gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Too soon for ${airplane.aircraftType} ${airplane.identifier} to land. It is still approaching..." + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                  case "RunwayMissed" => gameWindow.informationComponent.outputLabelArriving.text = "<html>" + s"Too soon for ${airplane.aircraftType} ${airplane.identifier} to land. It is still turning back towards the runways..." + "<br><br>" + oldTextArriving.replace("<html>", "") + "<html>"
                  case _ => throw new Exception("No status defined")
                }
              }
              case None => throw new Exception("No aircraft found")
            }
          } else gameWindow.informationComponent.outputLabelDeparting.text = s"There is no aircraft ready to move yet"
        }
        case None => throw new Exception("No UI_GameMainWindow created yet")
      }
    }


}
