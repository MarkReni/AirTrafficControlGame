package ATCG
import Auxiliary._
import Data.{GameConstants, LevelsData}
import org.joda.time.DateTime


case class Aircraft(
                     aircraftType: String,
                     var status: String,
                     var aircraftDestination: String,
                     components: Vector[AirportComponent],
                     airport: Airport,
                     timeOfMoving: Option[DateTime],
                     var startX: Int, // left
                     var endX: Int, // right
                     var startY: Int, // bottom
                     var endY: Int, // top
                     var aircraftRotation: Int,
                     ) extends AirportGrid(startX, endX, startY, endY) {
  // randomized the identifier of the aircraft
  val identifier: String = {
    val numberCount: Int = 4
    val letterCount: Int = 2
    var identifier: String = ""
    for (letter <- 0 until letterCount) identifier += Randomizer.randomLetter
    for (number <- 0 until numberCount) identifier += Randomizer.randomInteger
    identifier
  }
  // select aircraft picture based on aircraft type
  val aircraftPicPath: String = {
    if(this.aircraftType == "Bombardier") "pics/bombardier.png"
    else if(this.aircraftType == "Boeing") "pics/boeing.png"
    else "pics/airbus.png"
  }
  // randomize fuel amount for arriving aircraft; departing aircraft have fuel amount at 100%
  var fuelAmount: Double = {
    if(this.aircraftDestination == "Away") 100.0
    else math.floor(Randomizer.randomDouble * 100) / 100
  }
  // randomize passanger amount for aircraft that is displayed info window
  val passengerAmount: Int = {
    if(this.aircraftType == "Bombardier") Randomizer.randomPassangers(40, 151)
    else if(this.aircraftType == "Boeing") Randomizer.randomPassangers(150, 301)
    else Randomizer.randomPassangers(300, 501)
  }
  var missed: Boolean = false  // true if aircraft misses landing
  val randomCity: String = Randomizer.randomCity.toUpperCase  // randomizes the city the aircraft is flying to or arriving from
  var currentX: Int = startX  // current X-coordinate of the aircraft
  var currentY: Int = startY  // current Y-coordinate of the aircraft
  var aircraftPicAirport = Animator.rotate(aircraftRotation, aircraftPicPath)  // rotates aicraft picture in UI_Airport_Component towards degree provided in aircraftRotation
  var aircraftPicScaledAirport = aircraftPicAirport.getScaledInstance(GameConstants.airportScaledPicSize, GameConstants.airportScaledPicSize, java.awt.Image.SCALE_DEFAULT)   // scaled aircraft picture used in UI_Airport_Component
  var aircraftPicRadar = Animator.rotate(aircraftRotation + 270, aircraftPicPath)  // rotates aicraft picture in UI_Radar_Component towards degree provided in aircraftRotation
  var aircraftPicScaledRadar = aircraftPicRadar.getScaledInstance(GameConstants.radarScaledPicSize, GameConstants.radarScaledPicSize, java.awt.Image.SCALE_DEFAULT)   // scaled aircraft picture used in UI_Radar_Component
  val aircraftPicDestroyed = Animator.rotate(0, "pics/fire.png")  // used when aircraft is being destroyed due to low fuel
  val aircraftPicScaledFire = aircraftPicDestroyed.getScaledInstance(30, 30, java.awt.Image.SCALE_DEFAULT)  // scaled fire picture
  var currentComponent: AirportComponent = null   // current AirportComponent of the aircraft; OK if aircraft at radar does not use anything related to currentComponent
  var currentTile: Tile = null    // current Tile of the aircraft; OK if aircraft at radar does not use anything related to currentComponent
  val timeToLiveMax: Double = GameConstants.timeToLiveMax    // how fast until airplane gets destroyed due to fuel consumption
  var timeToLive: Double = timeToLiveMax  // variable version of the above constant
  var almostDestroyed: Boolean = false  // necessary for the fire animation
  var destroyed: Boolean = false  // true when fuelAmount == 0.0
  var nextComponentName: String = ""   // stores the name of the next airportComponent the aircraft is moving to (runway or gate)
  var pushbackVisited: Boolean = false  // for controlling the usage of Commands.outputTextDeparting
  var takeoffVisited: Boolean = false   // for controlling the usage of Commands.outputTextDeparting
  var homeGateVisited: Boolean = false  // for controlling the usage of Commands.outputTextArriving
  var gateFound: Boolean = false  // true when airplane has arrived to the gate indicated in the variable "nextComponentName"
  var collided: Boolean = false  // true when aircraft collided with another aircraft

  // initializes AirportComponent for aircraft at the gates
  if(status != "Approaching") {
    currentComponent = components.find(component => (component.left == this.startX && component.bottom == this.startY)).get   // this would fail for radarAircraft
    currentTile = currentComponent(currentX, currentY).get  // this would fail for radarAircraft
    this.currentTile.occupied = true
  }

  private def YStatic: Boolean = this.currentY == this.currentComponent.top && this.currentY == this.currentComponent.bottom  // return true when airportComponent is horizontal
  private def XStatic: Boolean = this.currentX == this.currentComponent.left && this.currentX == this.currentComponent.right  // return true when airportComponent is vertical
  private def isRunaway: Boolean = this.currentComponent.isInstanceOf[Runway]  // returns true when airportComponent is a runway
  private def isGate: Boolean = this.currentComponent.isInstanceOf[Gate]  // returns true when airportComponent is a gate
  private val gameDirectionVertical: String = Data.LevelsData.movementDirectionVertical(1)  // initializes game's vertical movement direction
  private var gameDirectionHorizontal: String = Data.LevelsData.movementDirectionHorizontal(0)  // initializes game's horizontal movement direction

  // returns the name of the gate the aircraft is currently at
  def getGateNo: Option[String] = {
    this.currentComponent match {
        case Gate(startX: Int, endX: Int, startY: Int, endY: Int, name: String, suitableAircraft: Vector[String]) => Some(name)
        case _ => None
      }
  }

  // auxiliary function that helps in finding the next AirportComponent and moves the aircraft into it
  private def findNextComponent(): AirportComponent = {
    this.currentComponent.occupied = false
    var nextComponent: Option[AirportComponent] = None
    if(this.gameDirectionHorizontal == "right" && this.gameDirectionVertical == "up") {
      if(this.aircraftDestination == "Away") {
        this.currentComponent match {
          case _: Runway =>
            nextComponent = this.components.find(component => (component.top == this.currentComponent.top && component.right == this.currentComponent.left - 1))
            this.currentX = nextComponent.get.right
            this.currentY = nextComponent.get.top
          case _: RouteIn =>
            nextComponent = this.components.find(component => (component.left == this.currentComponent.right + 1 && component.name == this.nextComponentName))
            this.currentX = nextComponent.get.left
            this.currentY = this.currentComponent.bottom
          case _: Gate =>
            nextComponent = this.components.find(component => (component.left == this.currentComponent.right + 1))
            this.currentX = nextComponent.get.left
            this.currentY = this.currentComponent.bottom
          case _ =>
            nextComponent = this.components.find(component => (component.top == this.currentComponent.bottom))
            this.currentX = nextComponent.get.left
            this.currentY = this.currentComponent.bottom
        }
    } else {  // aircraftDestination == Home
        if(this.currentComponent.isInstanceOf[Runway]) {
          nextComponent = this.components.find(component => (component.top == this.currentComponent.top && component.right == this.currentComponent.left - 1))
          this.currentX = nextComponent.get.right
          this.currentY = nextComponent.get.top
        } else {
          nextComponent = this.components.find(component => (component.right == this.currentComponent.left - 1 & !component.isInstanceOf[GateZone] & !component.isInstanceOf[Gate]))
          this.currentX = nextComponent.get.right
          this.currentY = this.currentComponent.top
        }
      }
    }
    nextComponent.get
  }

  // auxiliary function that moves the aircraft to the next AirportComponent (RouteOut or Gate) given as parameter
  private def moveToComponent[A <: AirportComponent](component: A): Unit = {
    this.currentComponent = component
    if(component.isInstanceOf[Gate]) component.occupied = true
    if(component.isInstanceOf[RouteOut]) component.occupied = true
    this.currentTile.occupied = false
    if(gameDirectionHorizontal == "right" && this.aircraftDestination == "Home") {
      this.currentX = component.right
      this.currentY = component.top
      this.currentTile = currentComponent(currentX, currentY).get
      if(this.currentTile.occupied) collision()   // check for collision
      this.currentTile.occupied = true
    } else if(gameDirectionHorizontal == "right" && this.aircraftDestination == "Away") {
      this.currentX = component.left
      this.currentY = component.top
      this.currentTile = currentComponent(currentX, currentY).get
      if(this.currentTile.occupied) collision()   // check for collision
      this.currentTile.occupied = true
    } else {  // gameDirection == "left"
      this.currentX = component.left
      this.currentY = component.bottom
      this.currentTile = currentComponent(currentX, currentY).get
      if(this.currentTile.occupied) collision()
      this.currentTile.occupied = true
    }
  }

  // locates the gate the aircraft is destined to move to
  private def findGate(): Boolean = {
    val availableGate: Option[Gate] = this.components.collect({
      case gate: Gate => gate
    }).find(_.name == this.nextComponentName)

    val coordinates: Option[(Int, Int)] = {
      availableGate match {
      case Some(gate) => {
        if(gameDirectionHorizontal == "right") {
          Some(Tuple2(gate.right, gate.top))
        } else {  // gameDirection == "left"
          Some(Tuple2(gate.left, bottom))
        }
      }
      case None => None
      }
    }
    if(gameDirectionVertical == "up" && gameDirectionHorizontal == "right")
      coordinates.get._1 == currentX - 1 && coordinates.get._2 == currentY
    else false
  }

  // locates the first RouteOut AirportComponent (aircraft moves to the first available route from the runway)
  private def findRoute(): Option[AirportComponent] = {
    if(gameDirectionVertical == "up" && gameDirectionHorizontal == "right" && this.aircraftDestination == "Home") {
      this.components.find(component => component.isInstanceOf[RouteOut] && !component.occupied && component.right == currentX - 1 && component.top == currentY)
    } else if(gameDirectionVertical == "up" && gameDirectionHorizontal == "right" && this.aircraftDestination == "Away") {
      val routeIn: Option[RouteIn] = this.components.collect({
          case routeIn: RouteIn => routeIn
          }).find(_.name == this.nextComponentName)
      if(routeIn.get.left == currentX + 1 && routeIn.get.top == currentY) routeIn
      else None
    } else None
  }

  // function that controls the movement of the aircraft at the radar
  def moveOneStepRadar(): Unit = {
    if(currentX < 22 && this.status == "Approaching") this.currentX += 1
    else if(currentX > 22 && this.status == "Approaching") this.currentX -= 1
    else if(currentX == 22 && currentY == 23 && this.status == "Approaching") {
      this.status = LevelsData.status(7)  // change status to "RunwayReady"
      Command.outputTextArriving(this, 0)
      this.currentY += 1
    }
    else if(currentX == 22 && currentY < 40 && (this.status == "Approaching" || this.status == "RunwayReady" || this.status == "RunwayLand")) {
      this.aircraftRotation = 180
      this.aircraftPicRadar = Animator.rotate(aircraftRotation, aircraftPicPath)
      this.aircraftPicScaledRadar = this.aircraftPicRadar.getScaledInstance(GameConstants.radarScaledPicSize, GameConstants.radarScaledPicSize, java.awt.Image.SCALE_DEFAULT)
      this.currentY += 1
    }
    else if(currentX == 22 && currentY == 40 && this.status == "RunwayLand"){   // 1) Command given to land (status == "RunwayLand)" or already pressed to move to gate (status == "HomeGate")  2) Aircraft reached the end of radar
      if(!missed) this.airport.aircraftAtRadar.dequeue()
      else this.airport.aircraftAtMissed.dequeue()
      val runwayComponent: Option[AirportComponent] = this.components.find(component => component.isInstanceOf[Runway] && component.name == nextComponentName)
      runwayComponent match {  // the current ariport component is now a runway
        case Some(runway) => {
          this.currentComponent = runway
          this.currentX = runway.right
          this.currentY = runway.bottom
          this.currentTile = currentComponent(currentX, currentY).get
          this.aircraftRotation = 180
          this.aircraftPicAirport = Animator.rotate(aircraftRotation, aircraftPicPath)
          this.aircraftPicScaledAirport = aircraftPicAirport.getScaledInstance(GameConstants.airportScaledPicSize, GameConstants.airportScaledPicSize, java.awt.Image.SCALE_DEFAULT)
          this.airport.aircraftMoving.enqueue(this)
          if(this.currentTile.occupied) collision()   // check for collision
          this.currentTile.occupied = true
        }
        case None => throw new Exception("No runway defined.")
      }
    } else if(currentX == 22 && currentY == 40 && this.status == "RunwayReady"){
      this.status = LevelsData.status(9)  // change status to "RunwayMissed"
      Command.outputTextArriving(this, 2)
      if(!missed) {
          this.airport.aircraftAtRadar.dequeue()
          this.airport.aircraftAtMissed.enqueue(this)
          this.missed = true
      } else {
          this.airport.aircraftAtMissed.dequeue()
          this.airport.aircraftAtMissed.enqueue(this)
      }
      this.currentY += 1
    } else if(currentY == 41 && currentX > 5 && this.status == "RunwayMissed") {
        this.aircraftPicRadar = Animator.rotate(270, aircraftPicPath)
        this.aircraftPicScaledRadar = this.aircraftPicRadar.getScaledInstance(GameConstants.radarScaledPicSize, GameConstants.radarScaledPicSize, java.awt.Image.SCALE_DEFAULT)
        this.currentX -= 1
    }
      else if(currentY > 20 && currentX == 5 && this.status == "RunwayMissed") {
        this.aircraftRotation = 0
        this.aircraftPicRadar = Animator.rotate(aircraftRotation, aircraftPicPath)
        this.aircraftPicScaledRadar = this.aircraftPicRadar.getScaledInstance(GameConstants.radarScaledPicSize, GameConstants.radarScaledPicSize, java.awt.Image.SCALE_DEFAULT)
        this.currentY -= 1
    }
      else if(currentY == 20 && currentX < 22 && this.status == "RunwayMissed") {
        this.aircraftRotation = 90
        this.aircraftPicRadar = Animator.rotate(aircraftRotation, aircraftPicPath)
        this.aircraftPicScaledRadar = this.aircraftPicRadar.getScaledInstance(GameConstants.radarScaledPicSize, GameConstants.radarScaledPicSize, java.awt.Image.SCALE_DEFAULT)
        this.currentX += 1
    }
      else if(currentX == 22 & this.status == "RunwayMissed") {
        this.aircraftRotation = 180
        this.aircraftPicRadar = Animator.rotate(aircraftRotation, aircraftPicPath)
        this.aircraftPicScaledRadar = this.aircraftPicRadar.getScaledInstance(GameConstants.radarScaledPicSize, GameConstants.radarScaledPicSize, java.awt.Image.SCALE_DEFAULT)
        this.status = LevelsData.status(8)  // change status to "Approaching"
        this.currentY += 1
    } else throw new Exception("Error at radar.")
    // reduce fuel amount every step
    this.timeToLive -= 1
    if(fuelAmount > 0.0) fuelAmount = math.floor(fuelAmount * (timeToLive / timeToLiveMax) * 100) / 100   // decrease fuel amount according to steps taken
    else {
      this.fuelAmount = 0.0
      this.destroyed = true
    }
    if(!almostDestroyed & fuelAmount <= 1.0) {
      this.almostDestroyed = true
      Command.outputTextArriving(this, 3)
    }
    if(destroyed) {
      if(this.aircraftDestination == "Away") Command.outputTextDeparting(this, 4)
      else Command.outputTextArriving(this, 4)
      if(this.nextComponentName != "") this.components.find(_.name == this.nextComponentName).get.occupied = false  // nextDestination must be reset before destruction
      if(currentTile != null) this.currentTile.occupied = false
      if(currentComponent != null) this.currentComponent.occupied = false
      this.airport.aircraftAtRadar = airport.aircraftAtRadar.filterNot(_.destroyed)
      this.airport.aircraftAtMissed = airport.aircraftAtMissed.filterNot(_.destroyed)
      this.airport.destroyedAircraft += 1  // update amount of destroyed aircraft at airport
      if(this.airport.destroyedAircraft == LevelsData.maxDestroyedAircraft) this.airport.resetGame("Destroyed")
    }
  }

  // controls what happens when collision occurs
  private def collision(): Unit = {
    this.collided = true
    if(this.nextComponentName != "") this.components.find(_.name == this.nextComponentName).get.occupied = false  // nextDestination must be reset before destruction
    if(currentTile != null) this.currentTile.occupied = false  // reset current tile
    if(currentComponent != null) this.currentComponent.occupied = false  // reset current component
    this.airport.resetGame("Collision")
    if(this.aircraftDestination == "Away") Command.outputTextDeparting(this, 5)
    else Command.outputTextArriving(this, 8)
    this.airport.aircraftMoving = airport.aircraftMoving.filterNot(_.collided)
  }

  // function that controls the movement of the aircraft at the airport
  def moveOneStepAirport(): Unit = {
    if(gameDirectionVertical == "up" && gameDirectionHorizontal == "right") {
      if(aircraftDestination == "Away") {
          if(this.currentX != this.currentComponent.right && YStatic) {   //  1) currentComponent is horizontal   2) airplane is NOT in the end (right) of currentComponent
            this.currentTile.occupied = false
            this.currentX += 1
            this.currentTile = currentComponent(currentX, currentY).get
            if(this.currentTile.occupied) collision()  // check for collision
            this.currentTile.occupied = true
        } else if(this.isGate && this.currentX == this.currentComponent.right && YStatic && (this.status == "Pushback" | this.status == "Takeoff" | this.status == "RunwayTakeoff")) {  //  1) currentComponent is horizontal    2) currentComponent is Gate  3) airplane is in the end (right) of currentComponent  4) Aircraft's status is "RunwayTakeoff" or "Takeoff" or "RunwayTakeoff"
            this.currentTile.occupied = false
            this.currentComponent = findNextComponent()
            this.currentTile = currentComponent(currentX, currentY).get
            if(this.currentTile.occupied) collision()   // check for collision
            this.currentTile.occupied = true
        } else if(!this.isGate && this.currentX == this.currentComponent.right && YStatic && this.status == "RunwayTakeoff") {  //  1) currentComponent is horizontal    2) currentComponent is not Gate  3) airplane is in the end (right) of currentComponent  4) Aircraft's status is "RunwayTakeoff"
            this.currentTile.occupied = false
            this.currentComponent = findNextComponent()
            this.currentTile = currentComponent(currentX, currentY).get
            if(this.currentTile.occupied) collision()   // check for collision
            this.currentTile.occupied = true
        } else if(!this.isRunaway && this.currentY != this.currentComponent.bottom && XStatic && this.aircraftRotation < 0) {  //  1) currentComponent is vertical   2) airplane is NOT in the end (bottom) of currentComponent  3) currentComponent is not runway 4) Aircraft rotation degree is below 0
            this.aircraftRotation += 5  // how fast rotation happens; slower when pushing back
            this.aircraftPicAirport = Animator.rotate(aircraftRotation, aircraftPicPath)
            this.aircraftPicScaledAirport = aircraftPicAirport.getScaledInstance(GameConstants.airportScaledPicSize, GameConstants.airportScaledPicSize, java.awt.Image.SCALE_DEFAULT)   // for airport component
        } else if(!this.isRunaway && this.currentY != this.currentComponent.bottom && XStatic && (this.status == "Takeoff" | this.status == "RunwayTakeoff")) {  //  1) currentComponent is vertical   2) airplane is NOT in the end (bottom) of currentComponent  3) currentComponent is not runway 4) Aircraft's status is "Takeoff"
            val routeNextTo: Option[AirportComponent] = this.findRoute()  // check for RouteIn
            routeNextTo match {
              case Some(route) => {
                if(this.aircraftRotation != 90) {  // rotate to 90 degrees before continuing
                  this.aircraftRotation += 15  // how fast rotation happens
                  this.aircraftPicAirport = Animator.rotate(aircraftRotation, aircraftPicPath)
                  this.aircraftPicScaledAirport = aircraftPicAirport.getScaledInstance(GameConstants.airportScaledPicSize, GameConstants.airportScaledPicSize, java.awt.Image.SCALE_DEFAULT)   // for airport component
                } else this.moveToComponent(route)
              }
              case None => {
                this.currentTile.occupied = false
                this.currentY += -1
                this.currentTile = currentComponent(currentX, currentY).get
                if(this.currentTile.occupied) collision()   // check for collision
                this.currentTile.occupied = true
              }
            }
        } else if(!this.isRunaway && this.currentY == this.currentComponent.bottom && XStatic && this.aircraftRotation != 90) {  //  1) currentComponent is vertical   2) airplane is in the end (bottom) of currentComponent  3) currentComponent is not runway  4) Aircraft rotation degree is not 90
            this.aircraftRotation += 15  // how fast rotation happens
            this.aircraftPicAirport = Animator.rotate(aircraftRotation, aircraftPicPath)
            this.aircraftPicScaledAirport = aircraftPicAirport.getScaledInstance(GameConstants.airportScaledPicSize, GameConstants.airportScaledPicSize, java.awt.Image.SCALE_DEFAULT)   // for airport component
        } else if(!this.isRunaway && this.currentY == this.currentComponent.bottom && XStatic) {  //  1) currentComponent is vertical   2) airplane is in the end (bottom) of currentComponent  3) currentComponent is not runway
            this.currentTile.occupied = false
            this.currentComponent = findNextComponent()
            this.currentTile = currentComponent(currentX, currentY).get
            if(this.currentTile.occupied) collision()   // check for collision
            this.currentTile.occupied = true
        } else if(this.isRunaway && this.currentY != this.currentComponent.top && XStatic && this.aircraftRotation != 180) {  //  1) currentComponent is vertical   2) airplane is NOT in the end (top) of currentComponent  3) currentComponent is runway  4) Aircraft rotation degree is not 180
            this.aircraftRotation += 15  // how fast rotation happens
            this.aircraftPicAirport = Animator.rotate(aircraftRotation, aircraftPicPath)
            this.aircraftPicScaledAirport = aircraftPicAirport.getScaledInstance(GameConstants.airportScaledPicSize, GameConstants.airportScaledPicSize, java.awt.Image.SCALE_DEFAULT)   // for airport component
        } else if(this.isRunaway && this.currentY != this.currentComponent.top && XStatic) {  //  1) currentComponent is vertical   2) airplane is NOT in the end (top) of currentComponent  3) currentComponent is runway
            this.currentTile.occupied = false
            this.currentY += 1
            this.currentTile = currentComponent(currentX, currentY).get
            if(this.currentTile.occupied) collision()   // check for collision
            this.currentTile.occupied = true
        } else if(this.isRunaway && this.currentY == this.currentComponent.top && XStatic) {  //  1) currentComponent is vertical   2) airplane is in the end (top) of currentComponent  3) currentComponent is runway
            this.currentTile.occupied = false
            this.airport.aircraftMoving.dequeueFirst(_.identifier == this.identifier)   // airplane flies away; must use dequeueFirst because aircraft might not depart in FIFO order
            this.airport.updateStatistics()  // update statistics after each successful departure
        } else if(!this.isRunaway && this.currentY != this.currentComponent.bottom && XStatic && this.status == "Pushback" && !pushbackVisited) {  // do nothing if status != "Takeoff"
            Command.outputTextDeparting(this, 1)
            this.pushbackVisited = true  // makes sure that output text is printed only once
          }
          else if(this.currentX == this.currentComponent.right && YStatic && this.status == "Takeoff" && !takeoffVisited) {  // do nothing if status != "RunwayTakeoff"
            Command.outputTextDeparting(this, 2)
            this.takeoffVisited = true  // makes sure that output text is printed only once
          }
      } else {    // aircraftDestination == Home
          if(this.isRunaway && this.currentY != this.currentComponent.top && XStatic) {
            val routeNextTo: Option[AirportComponent] = this.findRoute()  // check for RouteOut
                routeNextTo match {
                  case Some(route) => {
                    if(this.aircraftRotation != 270) {
                      this.aircraftRotation += 10  // how fast rotation happens; a bit slower when turning from runway to outroute in order to increase chance of collision
                      this.aircraftPicAirport = Animator.rotate(aircraftRotation, aircraftPicPath)
                      this.aircraftPicScaledAirport = aircraftPicAirport.getScaledInstance(GameConstants.airportScaledPicSize, GameConstants.airportScaledPicSize, java.awt.Image.SCALE_DEFAULT)   // for airport component
                    } else this.moveToComponent(route)
                  }
                  case None => {    //  1) currentComponent is vertical   2) airplane is NOT in the end (top) of currentComponent  3) currentComponent is runway
                    this.currentTile.occupied = false
                    this.currentY += 1
                    this.currentTile = currentComponent(currentX, currentY).get
                    if(this.currentTile.occupied) collision()   // check for collision
                    this.currentTile.occupied = true
                  }
                }
              } else if(this.isRunaway && this.currentY == this.currentComponent.top && XStatic && this.aircraftRotation != 270) {  //  1) currentComponent is vertical   2) airplane is in the end (top) of currentComponent  3) currentComponent is runway
                  this.aircraftRotation += 15  // how fast rotation happens
                  this.aircraftPicAirport = Animator.rotate(aircraftRotation, aircraftPicPath)
                  this.aircraftPicScaledAirport = aircraftPicAirport.getScaledInstance(GameConstants.airportScaledPicSize, GameConstants.airportScaledPicSize, java.awt.Image.SCALE_DEFAULT)   // for airport component
              } else if(this.isRunaway && this.currentY == this.currentComponent.top && XStatic) {  //  1) currentComponent is vertical   2) airplane is in the end (top) of currentComponent  3) currentComponent is runway
                  this.currentTile.occupied = false
                  this.currentComponent = findNextComponent()
                  this.currentTile = currentComponent(currentX, currentY).get
                  if(this.currentTile.occupied) collision()   // check for collision
                  this.currentTile.occupied = true
              } else if(this.currentX != this.currentComponent.left && YStatic) { //  1) currentComponent is horizontal   2) airplane is NOT in the end (left) of currentComponent
                  this.currentTile.occupied = false
                  this.currentX += -1
                  this.currentTile = currentComponent(currentX, currentY).get
                  if(this.currentTile.occupied) collision()   // check for collision
                  this.currentTile.occupied = true
              }  else if(!this.isGate && this.currentX == this.currentComponent.left && YStatic && this.status == "HomeGate") {  //  1) currentComponent is horizontal   2) airplane is in the end (left) of currentComponent  3) CurrentComponent is NOT gate  4) Aircraft's status is "HomeGate"
                  this.currentTile.occupied = false
                  this.currentComponent = findNextComponent()
                  this.currentTile = currentComponent(currentX, currentY).get
                  this.currentTile.occupied = true
              } else if(!this.isGate && this.currentX == this.currentComponent.left && YStatic && this.status != "HomeGate") {  //  1) currentComponent is horizontal   2) airplane is in the end (left) of currentComponent  3) CurrentComponent is NOT gate  4) Aircraft's status is NOT "HomeGate"
                 if(!this.homeGateVisited) {
                   Command.outputTextArriving(this, 6)
                   this.homeGateVisited = true
                 }
              } else if(!this.isRunaway && this.currentY != this.currentComponent.bottom && XStatic && this.aircraftRotation != 360 && !gateFound) {  //  1) currentComponent is vertical   2) airplane is NOT in the end (bottom) of currentComponent  3) currentComponent is NOT runway 4) Aircraft rotation degree is not 360  5) Gate has not been found
                  this.aircraftRotation += 15
                  this.aircraftPicAirport = Animator.rotate(aircraftRotation, aircraftPicPath)
                  this.aircraftPicScaledAirport = aircraftPicAirport.getScaledInstance(GameConstants.airportScaledPicSize, GameConstants.airportScaledPicSize, java.awt.Image.SCALE_DEFAULT)   // for airport component
              } else if(!this.isRunaway && this.currentY != this.currentComponent.bottom && XStatic) {  //  1) currentComponent is vertical   2) airplane is NOT in the end (bottom) of currentComponent  3) currentComponent is NOT runway
                  this.gateFound = findGate()
                  if(!gateFound) {
                    this.currentTile.occupied = false
                    this.currentY += -1
                    this.currentTile = currentComponent(currentX, currentY).get
                    if(this.currentTile.occupied) collision()   // check for collision
                    this.currentTile.occupied = true
                  } else {
                    if(this.aircraftRotation != 270) {
                      this.aircraftRotation -= 15
                      aircraftPicAirport = Animator.rotate(aircraftRotation, aircraftPicPath)
                      aircraftPicScaledAirport = aircraftPicAirport.getScaledInstance(GameConstants.airportScaledPicSize, GameConstants.airportScaledPicSize, java.awt.Image.SCALE_DEFAULT)   // for airport component
                    } else {
                      val chosenGate: Option[AirportComponent] = components.find(x => x.isInstanceOf[Gate] && x.name == this.nextComponentName)
                      moveToComponent(chosenGate.get)
                    }
                  }
              } else if(!this.isRunaway && this.currentY == this.currentComponent.bottom && XStatic) {  //  1) currentComponent is vertical   2) airplane is in the end (bottom) of currentComponent  3) currentComponent is NOT runway
                  this.currentTile.occupied = false
                  this.currentComponent = findNextComponent()
                  this.currentTile = currentComponent(currentX, currentY).get
                  if(this.currentTile.occupied) collision()   // check for collision
                  this.currentTile.occupied = true
              } else {
                  Command.outputTextArriving(this, 7)
                  airport.resetAirplane(this)  // airplane arrives at the gate
              }
          }
      }
      this.timeToLive -= 1
      if(fuelAmount > 0.0) fuelAmount = math.floor(fuelAmount * (timeToLive / timeToLiveMax) * 100) / 100   // decrease the fuel amount
      else {  // in case the fuel amount is already zero
        this.fuelAmount = 0.0
        this.destroyed = true
      }
      if(!almostDestroyed & fuelAmount <= 1.0) {  // in case aircraft is almost destroyed; for animation purposes
        this.almostDestroyed = true
        if(this.status != "RunwayTakeoff" && this.status != "HomeGate") {
          if(this.aircraftDestination == "Away") Command.outputTextDeparting(this, 3)
          else Command.outputTextArriving(this, 3)
        }
      }
      if(destroyed) {  // in case aircraft gets destroyed
        if(this.aircraftDestination == "Away") Command.outputTextDeparting(this, 4)
        else Command.outputTextArriving(this, 4)
        if(this.nextComponentName != "") this.components.find(_.name == this.nextComponentName).get.occupied = false  // nextDestination must be reset before destruction
        if(currentTile != null) this.currentTile.occupied = false  // reset current tile
        if(currentComponent != null) this.currentComponent.occupied = false  // reset current component
        this.airport.aircraftMoving = airport.aircraftMoving.filterNot(_.destroyed)
        this.airport.destroyedAircraft += 1  // update amount of destroyed aircraft at airport
        if(this.airport.destroyedAircraft == LevelsData.maxDestroyedAircraft) this.airport.resetGame("Destroyed")
      }
  }


}
