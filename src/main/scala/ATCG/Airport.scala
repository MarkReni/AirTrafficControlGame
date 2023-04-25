package ATCG
import Data._
import Auxiliary._
import org.joda.time.DateTime
import scala.collection.mutable.Queue


abstract class Airport {
  val airport_X_start: Int = 13  // used to construct the airport; first X-coordinate of the airport
  val airport_Y_start: Int = 1  // used to construct the airport; first Y-coordinate of the airport
  val airportWindow_X_size: Int = GameConstants.airport_X_size  // indicates max X-coordinate of the airport
  val airportWindow_Y_size: Int = GameConstants.airport_Y_size  // indicates max Y-coordinate of the airport
  val airportName: String  // the name of the airport
  var game: Option[Game] = None  // stores Game-class; used
  var takeoffCounter: Int = 0  // counts successful takeoffs
  var destroyedAircraft: Int = 0  // counts destroyed aircraft
  var aircraftAtGates: Queue[Aircraft] = Queue()  // queue for aircraft at gates
  var aircraftMoving: Queue[Aircraft] = Queue()  // queue for aircraft moving at the airport
  var aircraftAtRadar: Queue[Aircraft] = Queue()  // queue for aircraft moving at the radar
  var aircraftAtMissed: Queue[Aircraft] = Queue()  // queue for aircraft that missed landing and are moving at the radar
  val components: Vector[AirportComponent]  // all the components the airport consists of
  val gates: Vector[Gate]  // all the gates at the airport
  val runways: Vector[Runway]  // all the runways at the airport

  // when the airport is initialized, this function creates aircraft into some gates randomly
  private def createAirplaneAtRandomGates(): Unit = {
    var airplane: Option[Aircraft] = None
    val gatessNotOccupied: Vector[AirportComponent] = this.components.filter(component => !(component.occupied) && component.isInstanceOf[Gate])    // filters out components that are occupied and other than gates
    if(gatessNotOccupied.nonEmpty) {
      val randomIndex: Int = Randomizer.getRandomIndex(gatessNotOccupied.length)  // randomize at which gate the aircraft gets created
      val randomComponent: AirportComponent = gatessNotOccupied(randomIndex)
      randomComponent.occupied = true // set component as occupied
      randomComponent(randomComponent.left, randomComponent.bottom).get.occupied = true //set tile as occupied
      var addedTime: Some[DateTime] = Some(TimeSurveiller.addTime(0, true, this.aircraftAtGates.length))
      if(this.aircraftAtGates.nonEmpty) while(addedTime.get.getMillis <= this.aircraftAtGates.last.timeOfMoving.get.getMillis) addedTime = Some(TimeSurveiller.addTime(0, this.aircraftAtGates.isEmpty, this.aircraftAtGates.length))
      airplane = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(0), Data.LevelsData.aircraftDestination(0), this.components, this, addedTime, randomComponent.left, randomComponent.right, randomComponent.bottom, randomComponent.top, aircraftRotation = -90))
      this.aircraftAtGates.enqueue(airplane.get)
    }
  }

  // when the aircraft arrives at the gate, this function creates a new aircraft using arrived aircraft's parameters
  private def createAirplaneFromParameter[A <: AirportComponent](gate: A, airplaneType: String): Unit = {
    var addedTime: Some[DateTime] = Some(TimeSurveiller.addTime(0, this.aircraftAtGates.isEmpty, this.aircraftAtGates.length))
    if(this.aircraftAtGates.nonEmpty) while(addedTime.get.getMillis <= this.aircraftAtGates.last.timeOfMoving.get.getMillis) addedTime = Some(TimeSurveiller.addTime(0, this.aircraftAtGates.isEmpty, this.aircraftAtGates.length))
    val airplane: Some[Aircraft] = Some(new Aircraft(airplaneType, LevelsData.status(0), Data.LevelsData.aircraftDestination(0), this.components, this, addedTime, gate.left, gate.right, gate.bottom, gate.top, aircraftRotation = -90))
    this.aircraftAtGates.enqueue(airplane.get)
  }

  // this function creates a new aircraft into the radar
  def createAirplaneAtRadar(addedTime: Option[DateTime]): Unit = {
    var airplaneRadar: Option[Aircraft] = None
    val randomDirection: Int = Randomizer.getRandomOfTwo    // randomize from which side the airplane will come
    val randomAirplane: Int = Randomizer.getRandomOfTwo    // randomize whether Boeing or Airbus is selected
    if(this.airportName == "Helsinki-Vantaa") {
        if(Level.checkBoeing(this.airportName) && Level.checkAirbus(this.airportName)) {
          if(Randomizer.randomOfTen < Level.getBoeingAndAirbusProb(this.airportName)) {  // randomize the probability of Boeing or Airbus creation
            if(randomAirplane == 0) {  // Boeing
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
            } else {  // Airbus
                if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(2), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
                else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(2), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
            }
          } else {
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          }
        } else if(Level.checkBoeing(this.airportName)) {
            if(Randomizer.randomOfTen < Level.getBoeingAndAirbusProb(this.airportName)) {  // randomize the probability of Boeing creation
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          } else {
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          }
      } else {
          if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
          else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
      }
    } else if(this.airportName == "Frankfurt") {
        if(Level.checkBoeing(this.airportName) && Level.checkAirbus(this.airportName)) {
          if(Randomizer.randomOfTen < Level.getBoeingAndAirbusProb(this.airportName)) {  // randomize the probability of Boeing or Airbus creation
            if(randomAirplane == 0) {  // Boeing
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
            } else {  // Airbus
                if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(2), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
                else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(2), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
            }
          } else {
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          }
        } else if(Level.checkBoeing(this.airportName)) {
            if(Randomizer.randomOfTen < Level.getBoeingAndAirbusProb(this.airportName)) {  // randomize the probability of Boeing creation
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          } else {
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          }
      } else {
          if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
          else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
      }
    } else if(this.airportName == "Istanbul") {
        if(Level.checkBoeing(this.airportName) && Level.checkAirbus(this.airportName)) {
          if(Randomizer.randomOfTen < Level.getBoeingAndAirbusProb(this.airportName)) {  // randomize the probability of Boeing or Airbus creation
            if(randomAirplane == 0) {  // Boeing
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
            } else {  // Airbus
                if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(2), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
                else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(2), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
            }
          } else {
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          }
        } else if(Level.checkBoeing(this.airportName)) {
            if(Randomizer.randomOfTen < Level.getBoeingAndAirbusProb(this.airportName)) {  // randomize the probability of Boeing creation
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          } else {
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          }
      } else {
          if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
          else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
      }
    } else if(this.airportName == "Doha") {
        if(Level.checkBoeing(this.airportName) && Level.checkAirbus(this.airportName)) {
          if(Randomizer.randomOfTen < Level.getBoeingAndAirbusProb(this.airportName)) {  // randomize the probability of Boeing or Airbus creation
            if(randomAirplane == 0) {  // Boeing
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
            } else {  // Airbus
                if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(2), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
                else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(2), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
            }
          } else {
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          }
        } else if(Level.checkBoeing(this.airportName)) {
            if(Randomizer.randomOfTen < Level.getBoeingAndAirbusProb(this.airportName)) {  // randomize the probability of Boeing creation
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(1), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          } else {
              if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
              else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
          }
      } else {
          if(randomDirection == 0) airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_start, GameConstants.radar_X_end, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 180))   // left side airplane
          else airplaneRadar = Some(new Aircraft(LevelsData.aircraftType(0), LevelsData.status(8), Data.LevelsData.aircraftDestination(1), this.components, this, addedTime, GameConstants.radar_X_end, GameConstants.radar_X_start, GameConstants.radar_Y_start, GameConstants.radar_Y_end, aircraftRotation = 0))  // right side airplane
      }
    } else throw new Exception("No airport defined.")

    airplaneRadar match {
      case Some(airplane) => {
        Command.outputTextArriving(airplane, 1)
        this.aircraftAtRadar.enqueue(airplane)
      }
      case None => throw new Exception("No aircraft created at radar.")
    }

  }

  // after command is given to move to pushback, this function selects the next aircraft that is moved to the moving aircraft queue
  def selectNextAirplane(): Unit = {
    if(aircraftAtGates.nonEmpty) {
      val aircraftToMoving: Aircraft = aircraftAtGates.dequeue()
      aircraftMoving.enqueue(aircraftToMoving)
    }
  }

  // this function updates the game's and the airport's statistics after the aircraft has successfully taken off; used by the Command-object
  def updateStatistics(): Unit = {
    this.game match {
      case Some(game) => {
        val currentLevel: Int = game.playerLevels(this.airportName)
        val takeoffsRequiredToAdvance: Int = LevelsData.levelsRange(currentLevel)
        takeoffCounter += 1
        if(takeoffCounter%LevelsData.maxTakeoffs == 0) game.playerCoins += LevelsData.coinsRewardedForTakeoffs  // checks whether max takeoffs have been reached
        if(takeoffCounter > takeoffsRequiredToAdvance) {  // after certain amount of successful takeoffs the level changes
          val newLevel: Int = math.min(10, currentLevel + 1)
          game.playerLevels = game.playerLevels - this.airportName + (this.airportName -> (newLevel))
          takeoffCounter = 0  // takeoffs reset
          destroyedAircraft = 0  // destroyed aircraft reset
          game.playerCoins += LevelsData.coinsRewardedForLevel  // coins rewarded for completing a level
          Command.outputTextLabel(newLevel, index=0)  // output text
          Level.updateLevelParameters(newLevel, this.airportName)  // aircraft creation frequency gets tighter according to level parameters
          Level.checkWeather(this.airportName)
        }
      }
      case None => throw new Exception("No game object created.")
    }
  }

  // this function resets the game after a collision between the aircraft or after max destroyed aircraft reached
  def resetGame(cause: String): Unit = {
    this.takeoffCounter = 0
    this.destroyedAircraft = 0
    this.game match {
      case Some(game) => {  // coins are reduced due to collision or destruction
        if(cause == "Destroyed") {
          if(game.playerCoins >= LevelsData.coinsDeductedAfterDestruction) game.playerCoins -= LevelsData.coinsDeductedAfterDestruction
          else game.playerCoins = 0
        } else {  // cause == "Collision"
          if(game.playerCoins >= LevelsData.coinsDeductedAfterCollision) game.playerCoins -= LevelsData.coinsDeductedAfterCollision
          else game.playerCoins = 0
        }
      }
      case None => throw new Exception("No game object created.")
    }
    if(cause == "Destroyed") Command.outputTextLabel(index=2)
    else Command.outputTextLabel(index=1)
  }

  // this function resets the aircraft that has arrived to the gate; used by the Aircraft Class
  def resetAirplane(aircraft: Aircraft): Unit = {
    val removedAirplane: Option[Aircraft] = this.aircraftMoving.dequeueFirst(_.identifier == aircraft.identifier) // must use dequeueFirst because aircraft might not arrive to gates in FIFO order
    removedAirplane match {
      case Some(removedAirplane) =>  this.createAirplaneFromParameter(removedAirplane.currentComponent, removedAirplane.aircraftType)   // new airplane is created from removed airplane's current component's (gate's) coordinates
      case None => throw new Exception("Dequeue works wrongly.")
    }
  }

  // before airport window opens some data structures and variables need to be reset
  def resetAirport(): Unit = {
    this.aircraftMoving.clear()
    this.aircraftAtRadar.clear()
    this.aircraftAtMissed.clear()
    this.aircraftAtGates.clear()
    this.takeoffCounter = 0
    this.destroyedAircraft = 0
    RedoTimer.readyToPushback = false
    this.components.foreach(_.occupied = false)
  }

  // used by UI_GameMainWindow to populate gates when the airport is first initialized
  def populateGates(): Unit = {
    createAirplaneAtRandomGates()
    createAirplaneAtRandomGates()
    createAirplaneAtRandomGates()
  }


}

// Airport objects (Helsinki-Vantaa, Frankfurt, Istanbul, Doha)
object AirportHelsinki extends Airport {
  override val airportName = "Helsinki-Vantaa"
  val components = Vector[AirportComponent](
    new AirportZone(0, airportWindow_X_size, 0, airportWindow_Y_size)
      .placeElement(airport_X_start + 5, airport_Y_start + 2).placeElement(airport_X_start + 6, airport_Y_start + 2).placeElement(airport_X_start + 7, airport_Y_start + 2).placeElement(airport_X_start + 8, airport_Y_start + 2).placeElement(airport_X_start + 9, airport_Y_start + 2).placeElement(airport_X_start + 10, airport_Y_start + 2)
      .placeElement(airport_X_start + 5, airport_Y_start + 3).placeElement(airport_X_start + 6, airport_Y_start + 3).placeElement(airport_X_start + 7, airport_Y_start + 3).placeElement(airport_X_start + 8, airport_Y_start + 3).placeElement(airport_X_start + 9, airport_Y_start + 3).placeElement(airport_X_start + 10, airport_Y_start + 3)
      .placeElement(airport_X_start + 5, airport_Y_start + 4).placeElement(airport_X_start + 6, airport_Y_start + 4).placeElement(airport_X_start + 7, airport_Y_start + 4).placeElement(airport_X_start + 8, airport_Y_start + 4).placeElement(airport_X_start + 9, airport_Y_start + 4).placeElement(airport_X_start + 10, airport_Y_start + 4)
      .placeElement(airport_X_start + 5, airport_Y_start + 5).placeElement(airport_X_start + 6, airport_Y_start + 5).placeElement(airport_X_start + 7, airport_Y_start + 5).placeElement(airport_X_start + 8, airport_Y_start + 5).placeElement(airport_X_start + 9, airport_Y_start + 5).placeElement(airport_X_start + 10, airport_Y_start + 5)
      .placeElement(airport_X_start + 5, airport_Y_start + 6).placeElement(airport_X_start + 6, airport_Y_start + 6).placeElement(airport_X_start + 7, airport_Y_start + 6).placeElement(airport_X_start + 8, airport_Y_start + 6).placeElement(airport_X_start + 9, airport_Y_start + 6).placeElement(airport_X_start + 10, airport_Y_start + 6)
      .placeElement(airport_X_start + 5, airport_Y_start + 7).placeElement(airport_X_start + 6, airport_Y_start + 7).placeElement(airport_X_start + 7, airport_Y_start + 7).placeElement(airport_X_start + 8, airport_Y_start + 7).placeElement(airport_X_start + 9, airport_Y_start + 7).placeElement(airport_X_start + 10, airport_Y_start + 7)
      .placeElement(airport_X_start + 5, airport_Y_start + 8).placeElement(airport_X_start + 6, airport_Y_start + 8).placeElement(airport_X_start + 7, airport_Y_start + 8).placeElement(airport_X_start + 8, airport_Y_start + 8).placeElement(airport_X_start + 9, airport_Y_start + 8).placeElement(airport_X_start + 10, airport_Y_start + 8)
      .placeElement(airport_X_start + 5, airport_Y_start + 9).placeElement(airport_X_start + 6, airport_Y_start + 9).placeElement(airport_X_start + 7, airport_Y_start + 9).placeElement(airport_X_start + 8, airport_Y_start + 9).placeElement(airport_X_start + 9, airport_Y_start + 9).placeElement(airport_X_start + 10, airport_Y_start + 9)
      .placeElement(airport_X_start + 5, airport_Y_start + 10).placeElement(airport_X_start + 6, airport_Y_start + 10).placeElement(airport_X_start + 7, airport_Y_start + 10).placeElement(airport_X_start + 8, airport_Y_start + 10).placeElement(airport_X_start + 9, airport_Y_start + 10).placeElement(airport_X_start + 10, airport_Y_start + 10)
      .placeElement(airport_X_start + 5, airport_Y_start + 11).placeElement(airport_X_start + 6, airport_Y_start + 11).placeElement(airport_X_start + 7, airport_Y_start + 11).placeElement(airport_X_start + 8, airport_Y_start + 11).placeElement(airport_X_start + 9, airport_Y_start + 11).placeElement(airport_X_start + 10, airport_Y_start + 11)
      .placeElement(airport_X_start + 5, airport_Y_start + 12).placeElement(airport_X_start + 6, airport_Y_start + 12).placeElement(airport_X_start + 7, airport_Y_start + 12).placeElement(airport_X_start + 8, airport_Y_start + 12).placeElement(airport_X_start + 9, airport_Y_start + 12).placeElement(airport_X_start + 10, airport_Y_start + 12)
      .placeElement(airport_X_start + 5, airport_Y_start + 13).placeElement(airport_X_start + 6, airport_Y_start + 13).placeElement(airport_X_start + 7, airport_Y_start + 13).placeElement(airport_X_start + 8, airport_Y_start + 13).placeElement(airport_X_start + 9, airport_Y_start + 13).placeElement(airport_X_start + 10, airport_Y_start + 13)
      .placeElement(airport_X_start + 5, airport_Y_start + 14).placeElement(airport_X_start + 6, airport_Y_start + 14).placeElement(airport_X_start + 7, airport_Y_start + 14).placeElement(airport_X_start + 8, airport_Y_start + 14).placeElement(airport_X_start + 9, airport_Y_start + 14).placeElement(airport_X_start + 10, airport_Y_start + 14)
      .placeElement(airport_X_start + 5, airport_Y_start + 15).placeElement(airport_X_start + 6, airport_Y_start + 15).placeElement(airport_X_start + 7, airport_Y_start + 15).placeElement(airport_X_start + 8, airport_Y_start + 15).placeElement(airport_X_start + 9, airport_Y_start + 15).placeElement(airport_X_start + 10, airport_Y_start + 15),
    new RouteIn(airport_X_start + 3, airport_X_start + 3, airport_Y_start, airport_Y_start + 24).placeCorner(airport_X_start + 3, airport_Y_start).placeCorner(airport_X_start + 3, airport_Y_start + 24),
    new RouteIn(airport_X_start + 4, airport_X_start + 11, airport_Y_start, airport_Y_start, "R1"),
    new RouteOut(airport_X_start + 4, airport_X_start + 11, airport_Y_start + 17, airport_Y_start + 17),
    new RouteOut(airport_X_start + 4, airport_X_start + 11, airport_Y_start + 24, airport_Y_start + 24),
    new Runway(airport_X_start + 12, airport_X_start + 12, airport_Y_start, airport_Y_start + 24, "R1"),
    new GateZone(airport_X_start - 2, airport_X_start + 2, airport_Y_start + 1, airport_Y_start + 12),
    new Gate(airport_X_start + 1, airport_X_start + 2, airport_Y_start + 2, airport_Y_start + 2, "G1"),
    new Gate(airport_X_start, airport_X_start + 2, airport_Y_start + 5, airport_Y_start + 5, "G2", Vector("Bombardier", "Boeing")),
    new Gate(airport_X_start - 1, airport_X_start + 2, airport_Y_start + 8, airport_Y_start + 8, "G3", Vector("Bombardier", "Boeing", "Airbus")),
    new Gate(airport_X_start, airport_X_start + 2, airport_Y_start + 11, airport_Y_start + 11, "G4", Vector("Bombardier", "Boeing")))
  val gates: Vector[Gate] = components.collect({ case gate: Gate => gate })
  val runways: Vector[Runway] = components.collect({ case runway: Runway => runway })
}

object AirportFrankfurt extends Airport {
  override val airportName = "Frankfurt"
  val components = Vector[AirportComponent](
    new AirportZone(0, airportWindow_X_size, 0, airportWindow_Y_size)
      .placeElement(airport_X_start + 5, airport_Y_start + 5).placeElement(airport_X_start + 6, airport_Y_start + 5).placeElement(airport_X_start + 7, airport_Y_start + 5).placeElement(airport_X_start + 8, airport_Y_start + 5).placeElement(airport_X_start + 9, airport_Y_start + 5).placeElement(airport_X_start + 10, airport_Y_start + 5)
      .placeElement(airport_X_start + 5, airport_Y_start + 6).placeElement(airport_X_start + 6, airport_Y_start + 6).placeElement(airport_X_start + 7, airport_Y_start + 6).placeElement(airport_X_start + 8, airport_Y_start + 6).placeElement(airport_X_start + 9, airport_Y_start + 6).placeElement(airport_X_start + 10, airport_Y_start + 6)
      .placeElement(airport_X_start + 5, airport_Y_start + 9).placeElement(airport_X_start + 6, airport_Y_start + 9).placeElement(airport_X_start + 7, airport_Y_start + 9).placeElement(airport_X_start + 8, airport_Y_start + 9).placeElement(airport_X_start + 9, airport_Y_start + 9).placeElement(airport_X_start + 10, airport_Y_start + 9)
      .placeElement(airport_X_start + 5, airport_Y_start + 10).placeElement(airport_X_start + 6, airport_Y_start + 10).placeElement(airport_X_start + 7, airport_Y_start + 10).placeElement(airport_X_start + 8, airport_Y_start + 10).placeElement(airport_X_start + 9, airport_Y_start + 10).placeElement(airport_X_start + 10, airport_Y_start + 10)
      .placeElement(airport_X_start + 5, airport_Y_start + 13).placeElement(airport_X_start + 6, airport_Y_start + 13).placeElement(airport_X_start + 7, airport_Y_start + 13).placeElement(airport_X_start + 8, airport_Y_start + 13).placeElement(airport_X_start + 9, airport_Y_start + 13).placeElement(airport_X_start + 10, airport_Y_start + 13)
      .placeElement(airport_X_start + 5, airport_Y_start + 14).placeElement(airport_X_start + 6, airport_Y_start + 14).placeElement(airport_X_start + 7, airport_Y_start + 14).placeElement(airport_X_start + 8, airport_Y_start + 14).placeElement(airport_X_start + 9, airport_Y_start + 14).placeElement(airport_X_start + 10, airport_Y_start + 14)
      .placeElement(airport_X_start + 5, airport_Y_start + 17).placeElement(airport_X_start + 6, airport_Y_start + 17).placeElement(airport_X_start + 7, airport_Y_start + 17).placeElement(airport_X_start + 8, airport_Y_start + 17).placeElement(airport_X_start + 9, airport_Y_start + 17).placeElement(airport_X_start + 10, airport_Y_start + 17)
      .placeElement(airport_X_start + 5, airport_Y_start + 18).placeElement(airport_X_start + 6, airport_Y_start + 18).placeElement(airport_X_start + 7, airport_Y_start + 18).placeElement(airport_X_start + 8, airport_Y_start + 18).placeElement(airport_X_start + 9, airport_Y_start + 18).placeElement(airport_X_start + 10, airport_Y_start + 18)
    ,
    new RouteIn(airport_X_start + 3, airport_X_start + 3, airport_Y_start, airport_Y_start + 24).placeCorner(airport_X_start + 3, airport_Y_start).placeCorner(airport_X_start + 3, airport_Y_start + 24),
    new RouteIn(airport_X_start + 4, airport_X_start + 11, airport_Y_start + 3, airport_Y_start + 3, "R1"),
    new RouteIn(airport_X_start + 4, airport_X_start + 15, airport_Y_start, airport_Y_start, "R2"),
    new RouteOut(airport_X_start + 4, airport_X_start + 11, airport_Y_start + 20, airport_Y_start + 20),  // RouteOut for R1
    new RouteOut(airport_X_start + 4, airport_X_start + 15, airport_Y_start + 24, airport_Y_start + 24),  // RouteOut for R2
    new Runway(airport_X_start + 12, airport_X_start + 12, airport_Y_start + 3, airport_Y_start + 20, "R1"),
    new Runway(airport_X_start + 16, airport_X_start + 16, airport_Y_start, airport_Y_start + 24, "R2"),
    new GateZone(airport_X_start - 2, airport_X_start + 2, airport_Y_start + 6, airport_Y_start + 17),
    new Gate(airport_X_start - 1, airport_X_start + 2, airport_Y_start + 7, airport_Y_start + 7, "G1", Vector("Bombardier", "Boeing", "Airbus")),
    new Gate(airport_X_start, airport_X_start + 2, airport_Y_start + 10, airport_Y_start + 10, "G2", Vector("Bombardier", "Boeing")),
    new Gate(airport_X_start + 1, airport_X_start + 2, airport_Y_start + 13, airport_Y_start + 13, "G3"),
    new Gate(airport_X_start - 1, airport_X_start + 2, airport_Y_start + 16, airport_Y_start + 16, "G4", Vector("Bombardier", "Boeing", "Airbus")))
  val gates: Vector[Gate] = components.collect({ case gate: Gate => gate })
  val runways: Vector[Runway] = components.collect({ case runway: Runway => runway })
}

object AirportIstanbul extends Airport {
  override val airportName = "Istanbul"
  val components = Vector[AirportComponent](
    new AirportZone(0, airportWindow_X_size, 0, airportWindow_Y_size)
      .placeElement(airport_X_start + 5, airport_Y_start + 6).placeElement(airport_X_start + 6, airport_Y_start + 6).placeElement(airport_X_start + 7, airport_Y_start + 6).placeElement(airport_X_start + 8, airport_Y_start + 6).placeElement(airport_X_start + 9, airport_Y_start + 6).placeElement(airport_X_start + 10, airport_Y_start + 6).placeElement(airport_X_start + 11, airport_Y_start + 6)
      .placeElement(airport_X_start + 5, airport_Y_start + 8).placeElement(airport_X_start + 6, airport_Y_start + 8).placeElement(airport_X_start + 7, airport_Y_start + 8).placeElement(airport_X_start + 8, airport_Y_start + 8).placeElement(airport_X_start + 9, airport_Y_start + 8).placeElement(airport_X_start + 10, airport_Y_start + 8).placeElement(airport_X_start + 11, airport_Y_start + 8)
      .placeElement(airport_X_start + 5, airport_Y_start + 10).placeElement(airport_X_start + 6, airport_Y_start + 10).placeElement(airport_X_start + 7, airport_Y_start + 10).placeElement(airport_X_start + 8, airport_Y_start + 10).placeElement(airport_X_start + 9, airport_Y_start + 10).placeElement(airport_X_start + 10, airport_Y_start + 10).placeElement(airport_X_start + 11, airport_Y_start + 10)
      .placeElement(airport_X_start + 5, airport_Y_start + 12).placeElement(airport_X_start + 6, airport_Y_start + 12).placeElement(airport_X_start + 7, airport_Y_start + 12).placeElement(airport_X_start + 8, airport_Y_start + 12).placeElement(airport_X_start + 9, airport_Y_start + 12).placeElement(airport_X_start + 10, airport_Y_start + 12).placeElement(airport_X_start + 11, airport_Y_start + 12)
      .placeElement(airport_X_start + 5, airport_Y_start + 14).placeElement(airport_X_start + 6, airport_Y_start + 14).placeElement(airport_X_start + 7, airport_Y_start + 14).placeElement(airport_X_start + 8, airport_Y_start + 14).placeElement(airport_X_start + 9, airport_Y_start + 14).placeElement(airport_X_start + 10, airport_Y_start + 14).placeElement(airport_X_start + 11, airport_Y_start + 14)
      .placeElement(airport_X_start + 5, airport_Y_start + 16).placeElement(airport_X_start + 6, airport_Y_start + 16).placeElement(airport_X_start + 7, airport_Y_start + 16).placeElement(airport_X_start + 8, airport_Y_start + 16).placeElement(airport_X_start + 9, airport_Y_start + 16).placeElement(airport_X_start + 10, airport_Y_start + 16).placeElement(airport_X_start + 11, airport_Y_start + 16)
      .placeElement(airport_X_start + 5, airport_Y_start + 18).placeElement(airport_X_start + 6, airport_Y_start + 18).placeElement(airport_X_start + 7, airport_Y_start + 18).placeElement(airport_X_start + 8, airport_Y_start + 18).placeElement(airport_X_start + 9, airport_Y_start + 18).placeElement(airport_X_start + 10, airport_Y_start + 18).placeElement(airport_X_start + 11, airport_Y_start + 18)
    ,
    new RouteIn(airport_X_start + 3, airport_X_start + 3, airport_Y_start, airport_Y_start + 24).placeCorner(airport_X_start + 3, airport_Y_start).placeCorner(airport_X_start + 3, airport_Y_start + 24),
    new RouteIn(airport_X_start + 4, airport_X_start + 12, airport_Y_start + 4, airport_Y_start + 4, "R1"),
    new RouteIn(airport_X_start + 4, airport_X_start + 15, airport_Y_start + 2, airport_Y_start + 2, "R2"),
    new RouteIn(airport_X_start + 4, airport_X_start + 18, airport_Y_start, airport_Y_start, "R3"),
    new RouteOut(airport_X_start + 4, airport_X_start + 12, airport_Y_start + 20, airport_Y_start + 20),  // RouteOut for R1
    new RouteOut(airport_X_start + 4, airport_X_start + 15, airport_Y_start + 22, airport_Y_start + 22),  // RouteOut for R2
    new RouteOut(airport_X_start + 4, airport_X_start + 18, airport_Y_start + 24, airport_Y_start + 24),  // RouteOut for R3
    new Runway(airport_X_start + 13, airport_X_start + 13, airport_Y_start + 4, airport_Y_start + 20, "R1"),
    new Runway(airport_X_start + 16, airport_X_start + 16, airport_Y_start + 2, airport_Y_start + 22, "R2"),
    new Runway(airport_X_start + 19, airport_X_start + 19, airport_Y_start, airport_Y_start + 24, "R3"),
    new GateZone(airport_X_start - 2, airport_X_start + 2, airport_Y_start + 5, airport_Y_start + 19),
    new Gate(airport_X_start + 1, airport_X_start + 2, airport_Y_start + 6, airport_Y_start + 6, "G1"),
    new Gate(airport_X_start, airport_X_start + 2, airport_Y_start + 9, airport_Y_start + 9, "G2", Vector("Bombardier", "Boeing")),
    new Gate(airport_X_start, airport_X_start + 2, airport_Y_start + 12, airport_Y_start + 12, "G3", Vector("Bombardier", "Boeing")),
    new Gate(airport_X_start - 1, airport_X_start + 2, airport_Y_start + 15, airport_Y_start + 15, "G4", Vector("Bombardier", "Boeing", "Airbus")),
    new Gate(airport_X_start - 1, airport_X_start + 2, airport_Y_start + 18, airport_Y_start + 18, "G5", Vector("Bombardier", "Boeing", "Airbus")))
  val gates: Vector[Gate] = components.collect({ case gate: Gate => gate })
  val runways: Vector[Runway] = components.collect({ case runway: Runway => runway })
}

object AirportDoha extends Airport {
  override val airportName = "Doha"
  val components = Vector[AirportComponent](
    new AirportZone(0, airportWindow_X_size, 0, airportWindow_Y_size)
      .placeElement(airport_X_start + 5, airport_Y_start + 8).placeElement(airport_X_start + 6, airport_Y_start + 8).placeElement(airport_X_start + 7, airport_Y_start + 8).placeElement(airport_X_start + 8, airport_Y_start + 8).placeElement(airport_X_start + 9, airport_Y_start + 8).placeElement(airport_X_start + 10, airport_Y_start + 8).placeElement(airport_X_start + 11, airport_Y_start + 8)
      .placeElement(airport_X_start + 5, airport_Y_start + 10).placeElement(airport_X_start + 6, airport_Y_start + 10).placeElement(airport_X_start + 7, airport_Y_start + 10).placeElement(airport_X_start + 8, airport_Y_start + 10).placeElement(airport_X_start + 9, airport_Y_start + 10).placeElement(airport_X_start + 10, airport_Y_start + 10).placeElement(airport_X_start + 11, airport_Y_start + 10)
      .placeElement(airport_X_start + 5, airport_Y_start + 12).placeElement(airport_X_start + 6, airport_Y_start + 12).placeElement(airport_X_start + 7, airport_Y_start + 12).placeElement(airport_X_start + 8, airport_Y_start + 12).placeElement(airport_X_start + 9, airport_Y_start + 12).placeElement(airport_X_start + 10, airport_Y_start + 12).placeElement(airport_X_start + 11, airport_Y_start + 12)
      .placeElement(airport_X_start + 5, airport_Y_start + 14).placeElement(airport_X_start + 6, airport_Y_start + 14).placeElement(airport_X_start + 7, airport_Y_start + 14).placeElement(airport_X_start + 8, airport_Y_start + 14).placeElement(airport_X_start + 9, airport_Y_start + 14).placeElement(airport_X_start + 10, airport_Y_start + 14).placeElement(airport_X_start + 11, airport_Y_start + 14)
      .placeElement(airport_X_start + 5, airport_Y_start + 16).placeElement(airport_X_start + 6, airport_Y_start + 16).placeElement(airport_X_start + 7, airport_Y_start + 16).placeElement(airport_X_start + 8, airport_Y_start + 16).placeElement(airport_X_start + 9, airport_Y_start + 16).placeElement(airport_X_start + 10, airport_Y_start + 16).placeElement(airport_X_start + 11, airport_Y_start + 16)
      .placeElement(airport_X_start + 21, airport_Y_start + 3).placeElement(airport_X_start + 22, airport_Y_start + 3).placeElement(airport_X_start + 23, airport_Y_start + 3).placeElement(airport_X_start + 24, airport_Y_start + 3).placeElement(airport_X_start + 25, airport_Y_start + 3)  // DOHA sign
      .placeElement(airport_X_start + 21, airport_Y_start + 4).placeElement(airport_X_start + 25, airport_Y_start + 4)
      .placeElement(airport_X_start + 21, airport_Y_start + 5).placeElement(airport_X_start + 25, airport_Y_start + 5)
      .placeElement(airport_X_start + 22, airport_Y_start + 6).placeElement(airport_X_start + 23, airport_Y_start + 6).placeElement(airport_X_start + 24, airport_Y_start + 6)
      .placeElement(airport_X_start + 22, airport_Y_start + 8).placeElement(airport_X_start + 23, airport_Y_start + 8).placeElement(airport_X_start + 24, airport_Y_start + 8)
      .placeElement(airport_X_start + 21, airport_Y_start + 9).placeElement(airport_X_start + 25, airport_Y_start + 9)
      .placeElement(airport_X_start + 21, airport_Y_start + 10).placeElement(airport_X_start + 25, airport_Y_start + 10)
      .placeElement(airport_X_start + 22, airport_Y_start + 11).placeElement(airport_X_start + 23, airport_Y_start + 11).placeElement(airport_X_start + 24, airport_Y_start + 11)
      .placeElement(airport_X_start + 21, airport_Y_start + 13).placeElement(airport_X_start + 22, airport_Y_start + 13).placeElement(airport_X_start + 23, airport_Y_start + 13).placeElement(airport_X_start + 24, airport_Y_start + 13).placeElement(airport_X_start + 25, airport_Y_start + 13)
      .placeElement(airport_X_start + 23, airport_Y_start + 14)
      .placeElement(airport_X_start + 23, airport_Y_start + 15)
      .placeElement(airport_X_start + 21, airport_Y_start + 16).placeElement(airport_X_start + 22, airport_Y_start + 16).placeElement(airport_X_start + 23, airport_Y_start + 16).placeElement(airport_X_start + 24, airport_Y_start + 16).placeElement(airport_X_start + 25, airport_Y_start + 16)
      .placeElement(airport_X_start + 21, airport_Y_start + 18).placeElement(airport_X_start + 22, airport_Y_start + 18).placeElement(airport_X_start + 23, airport_Y_start + 18).placeElement(airport_X_start + 24, airport_Y_start + 18).placeElement(airport_X_start + 25, airport_Y_start + 18)
      .placeElement(airport_X_start + 23, airport_Y_start + 19).placeElement(airport_X_start + 25, airport_Y_start + 19)
      .placeElement(airport_X_start + 23, airport_Y_start + 20).placeElement(airport_X_start + 25, airport_Y_start + 20)
      .placeElement(airport_X_start + 21, airport_Y_start + 21).placeElement(airport_X_start + 22, airport_Y_start + 21).placeElement(airport_X_start + 23, airport_Y_start + 21).placeElement(airport_X_start + 24, airport_Y_start + 21).placeElement(airport_X_start + 25, airport_Y_start + 21)
    ,
    new RouteIn(airport_X_start + 3, airport_X_start + 3, airport_Y_start, airport_Y_start + 24).placeCorner(airport_X_start + 3, airport_Y_start).placeCorner(airport_X_start + 3, airport_Y_start + 24),
    new RouteIn(airport_X_start + 4, airport_X_start + 12, airport_Y_start + 5, airport_Y_start + 5, "R1"),
    new RouteIn(airport_X_start + 4, airport_X_start + 15, airport_Y_start + 3, airport_Y_start + 3, "R2"),
    new RouteIn(airport_X_start + 4, airport_X_start + 18, airport_Y_start + 1, airport_Y_start + 1, "R3"),
    new RouteIn(airport_X_start + 4, airport_X_start + 26, airport_Y_start, airport_Y_start, "R4"),
    new RouteOut(airport_X_start + 4, airport_X_start + 12, airport_Y_start + 19, airport_Y_start + 19),  // RouteOut for R1
    new RouteOut(airport_X_start + 4, airport_X_start + 15, airport_Y_start + 21, airport_Y_start + 21),  // RouteOut for R2
    new RouteOut(airport_X_start + 4, airport_X_start + 18, airport_Y_start + 23, airport_Y_start + 23),  // RouteOut for R3
    new RouteOut(airport_X_start + 4, airport_X_start + 26, airport_Y_start + 24, airport_Y_start + 24),  // RouteOut for R4
    new Runway(airport_X_start + 13, airport_X_start + 13, airport_Y_start + 5, airport_Y_start + 19, "R1"),
    new Runway(airport_X_start + 16, airport_X_start + 16, airport_Y_start + 3, airport_Y_start + 21, "R2"),
    new Runway(airport_X_start + 19, airport_X_start + 19, airport_Y_start + 1, airport_Y_start + 23, "R3"),
    new Runway(airport_X_start + 27, airport_X_start + 27, airport_Y_start, airport_Y_start + 24, "R4"),
    new GateZone(airport_X_start - 2, airport_X_start + 2, airport_Y_start + 5, airport_Y_start + 19),
    new Gate(airport_X_start + 1, airport_X_start + 2, airport_Y_start + 6, airport_Y_start + 6, "G1"),
    new Gate(airport_X_start, airport_X_start + 2, airport_Y_start + 9, airport_Y_start + 9, "G2", Vector("Bombardier", "Boeing")),
    new Gate(airport_X_start, airport_X_start + 2, airport_Y_start + 12, airport_Y_start + 12, "G3", Vector("Bombardier", "Boeing")),
    new Gate(airport_X_start - 1, airport_X_start + 2, airport_Y_start + 15, airport_Y_start + 15, "G4", Vector("Bombardier", "Boeing", "Airbus")),
    new Gate(airport_X_start - 1, airport_X_start + 2, airport_Y_start + 18, airport_Y_start + 18, "G5", Vector("Bombardier", "Boeing", "Airbus")))
  val gates: Vector[Gate] = components.collect({ case gate: Gate => gate })
  val runways: Vector[Runway] = components.collect({ case runway: Runway => runway })
}
