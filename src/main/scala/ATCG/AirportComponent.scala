package ATCG


sealed trait AirportComponent extends AirportGrid {
  var occupied: Boolean = false  // true is component is occupied
  val width: Int = 1 + right - left  // width of the component
  val height: Int = 1 + top - bottom  // height of the component
  val location: Vector[Vector[Tile]] = Vector.fill(width, height)(new Tile(occupied))  // tile construction of the component
  val name: String

  def apply(x: Int, y: Int) = {   // x and y are given as location coordinates, not indexes
    if(this.contains(x, y)) {
      Some(location(x - left)(y - bottom))
    } else None
  }

  private def contains(x: Int, y: Int): Boolean = {
    x <= right && x >= left &&
    y >= bottom && y <= top
  }

  // marks the beginning or the end of the component as a corner based on given X-coordinate and Y-coordinate; used by the UI_AirportComponent class which builds the airport
  def placeCorner(x: Int, y: Int): AirportComponent = {
    this(x, y).foreach(_.isCorner = true)
    this
  }

  // places an element on the component based on given X-coordinate and Y-coordinate; used by the UI_AirportComponent class which builds the airport
  def placeElement(x: Int, y: Int): AirportComponent = {
    this(x, y).foreach(_.hasElement = true)
    this
  }


}

// airortComponents of the game
case class AirportZone(startX: Int, endX: Int, startY: Int, endY: Int, name: String = "") extends AirportGrid(startX, endX, startY, endY) with AirportComponent

case class Runway(startX: Int, endX: Int, startY: Int, endY: Int, name: String, var closed: Boolean = false) extends AirportGrid(startX, endX, startY, endY) with AirportComponent

case class Gate(startX: Int, endX: Int, startY: Int, endY: Int, name: String, suitableAircraft: Vector[String] = Vector("Bombardier")) extends AirportGrid(startX, endX, startY, endY) with AirportComponent

case class GateZone(startX: Int, endX: Int, startY: Int, endY: Int, name: String = "") extends AirportGrid(startX, endX, startY, endY) with AirportComponent

case class RouteIn(startX: Int, endX: Int, startY: Int, endY: Int, name: String = "") extends AirportGrid(startX, endX, startY, endY) with AirportComponent   // Route towards runway

case class RouteOut(startX: Int, endX: Int, startY: Int, endY: Int, name: String = "") extends AirportGrid(startX, endX, startY, endY) with AirportComponent  // Route towards gate

