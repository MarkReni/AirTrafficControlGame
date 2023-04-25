package UI
import ATCG._
import Data._
import java.awt.Font
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.swing._


case class UI_AirportComponent(airport: Airport) extends Component {
  tooltip = "Airport window displays information about the airport field: locations of runways and gates, which runways cross each other."
  // reset all component and tiles occupanicies to false
   for {
      component: AirportComponent <- airport.components
      x <- component.left to component.right
      y <- component.bottom to component.top
    } {
     component.occupied = false
     component(x, y).get.occupied = false
   }
  val width = Data.GameConstants.airportWindow_width
  val height = Data.GameConstants.airportWindow_height
  minimumSize = new Dimension(width, height)
  preferredSize = new Dimension(width, height)
  maximumSize = new Dimension(width, height)
  val squareWidth = Data.GameConstants.airportWindow_square_width
  val squareHeight =  Data.GameConstants.airportWindow_square_height
  val squareBorder = Data.GameConstants.airportWindow_square_border
  val fullWidth = squareWidth + squareBorder
  val fullHeight = squareHeight + squareBorder
  // PICTURES
  val runawayVerticalStart: BufferedImage = ImageIO.read(new File("pics/tile_0405.png"))
  val runawayVerticalMiddle: BufferedImage = ImageIO.read(new File("pics/tile_0432.png"))
  val runawayVerticalEnd: BufferedImage = ImageIO.read(new File("pics/tile_0459.png"))
  val runawayHorizontallStart: BufferedImage = ImageIO.read(new File("pics/tile_0434.png"))
  val runawayHorizontalMiddle: BufferedImage = ImageIO.read(new File("pics/tile_0435.png"))
  val runawayHorizontalEnd: BufferedImage = ImageIO.read(new File("pics/tile_0436.png"))
  val roadVerticalStart: BufferedImage = ImageIO.read(new File("pics/tile_0015.png"))
  val roadVerticalMiddle: BufferedImage = ImageIO.read(new File("pics/tile_0042.png"))
  val roadVerticalEnd: BufferedImage = ImageIO.read(new File("pics/tile_0069.png"))
  val roadHorizontallStart: BufferedImage = ImageIO.read(new File("pics/tile_0065.png"))
  val roadHorizontalMiddle: BufferedImage = ImageIO.read(new File("pics/tile_0066.png"))
  val roadHorizontalEnd: BufferedImage = ImageIO.read(new File("pics/tile_0067.png"))
  val roadLeftUpCorner: BufferedImage = ImageIO.read(new File("pics/tile_0011.png"))
  val roadLeftDownCorner: BufferedImage = ImageIO.read(new File("pics/tile_0038.png"))
  val roadRightUpCorner: BufferedImage = ImageIO.read(new File("pics/tile_0012.png"))
  val roadRightDownCorner: BufferedImage = ImageIO.read(new File("pics/tile_0039.png"))
  val roadBlock: BufferedImage = ImageIO.read(new File("pics/tile_0068.png"))
  val gateZone: BufferedImage = ImageIO.read(new File("pics/tile_0036.png"))
  val tree: BufferedImage = ImageIO.read(new File("pics/tile_0292.png"))

  // create airportComponent graphics from above pictures
  override def paintComponent(g: Graphics2D): Unit = {
    for {
      component: AirportComponent <- airport.components
      x <- component.left to component.right
      y <- component.bottom to component.top
    } {
      component match {
        case AirportZone(startX: Int, endX: Int, startY: Int, endY: Int, "") => {
          g.setColor(GameConstants.airport_color)
          g.fillRect(x * fullWidth, y * fullHeight, squareWidth + 5, squareHeight + 5)
          if(component(x, y).forall(_.hasElement)) {
            g.drawImage(tree, x * fullWidth, y * fullHeight, null) // place tree at airport
          }
        }
        case Runway(startX: Int, endX: Int, startY: Int, endY: Int, name: String, closed: Boolean) => {
          if(endX == x && startY == y) g.drawImage(runawayVerticalStart, x * fullWidth, y * fullHeight, null)
          else if(endX == x && endY == y) g.drawImage(runawayVerticalEnd, x * fullWidth, y * fullHeight, null)
          else g.drawImage(runawayVerticalMiddle, x * fullWidth, y * fullHeight, null)
          if((startY + 8) == y) {  // draw runway name
            g.setColor(GameConstants.runway_text_color)
            g.setFont(GameConstants.runway_font)
            g.drawString(name, x * fullWidth, y * fullHeight + 8)
            if(closed) {  // draw red X when runway is closed
              g.setColor(GameConstants.runway_text_color_closed)
              g.setFont(GameConstants.runway_font_closed)
              g.drawString("X", x * fullWidth + 3, y * fullHeight + 8)
            }
          }
        }
        case GateZone(startX: Int, endX: Int, startY: Int, endY: Int, "") => {
          g.drawImage(gateZone, x * fullWidth, y * fullHeight, null)
        }
        case Gate(startX: Int, endX: Int, startY: Int, endY: Int, name: String, suitableAircraft: Vector[String]) => {
          g.setColor(GameConstants.gate_color)
          g.fillRect(x * fullWidth, y * fullHeight, squareWidth + 5, squareHeight)
          if(startX == x) {  // draw gate name
            g.setColor(GameConstants.gate_text_color)
            g.setFont(GameConstants.gate_font)
            g.drawString(name, x * fullWidth, y * fullHeight + 8)  // draw gate number
            // draw text left to the gate
            g.setFont(GameConstants.gate_font_text)
            g.drawString(s"${name} suitable airplanes: ", 72, y * fullHeight + 8)  // x == 72
            g.drawString(s"${suitableAircraft.mkString(", ")}", 72, y * fullHeight + 17)
          }
        }
        case RouteIn(startX: Int, endX: Int, startY: Int, endY: Int, name: String) => {
          if(component(x, y).forall(_.isCorner)) {
            if(component(x, y - 1).isEmpty && component(x - 1, y).isEmpty) g.drawImage(roadLeftUpCorner, x * fullWidth, y * fullHeight, null)
            else if(component(x, y + 1).isEmpty && component(x - 1, y).isEmpty) g.drawImage(roadLeftDownCorner, x * fullWidth, y * fullHeight, null)
            else if(component(x, y + 1).isEmpty && component(x + 1, y).isEmpty) g.drawImage(roadRightDownCorner, x * fullWidth, y * fullHeight, null)
            else g.drawImage(roadRightUpCorner, x * fullWidth, y * fullHeight, null)
          }
          else if(startX == x && endX == x && startY != y && endY != y) g.drawImage(roadVerticalMiddle, x * fullWidth, y * fullHeight, null)
          else if(startY == y && endY == y && endX != x) g.drawImage(roadHorizontalMiddle, x * fullWidth, y * fullHeight, null)
          else if(endX == x && endY == y) g.drawImage(roadHorizontalEnd, x * fullWidth, y * fullHeight, null)
        }
        case RouteOut(startX: Int, endX: Int, startY: Int, endY: Int, "") => {
          if(component(x, y).forall(_.isCorner)) {
            if(component(x, y - 1).isEmpty && component(x - 1, y).isEmpty) g.drawImage(roadLeftUpCorner, x * fullWidth, y * fullHeight, null)
            else if(component(x, y + 1).isEmpty && component(x - 1, y).isEmpty) g.drawImage(roadLeftDownCorner, x * fullWidth, y * fullHeight, null)
            else if(component(x, y + 1).isEmpty && component(x + 1, y).isEmpty) g.drawImage(roadRightDownCorner, x * fullWidth, y * fullHeight, null)
            else g.drawImage(roadRightUpCorner, x * fullWidth, y * fullHeight, null)
          }
          else if(startX == x && endX == x && startY != y && endY != y) g.drawImage(roadVerticalMiddle, x * fullWidth, y * fullHeight, null)
          else if(startY == y && endY == y && endX != x) g.drawImage(roadHorizontalMiddle, x * fullWidth, y * fullHeight, null)
          else if(endX == x && endY == y) g.drawImage(roadHorizontalEnd, x * fullWidth, y * fullHeight, null)
        }
        case _ => throw new Exception("Component is missing.")
      }
    }
    for {
      airplane: Aircraft <- this.airport.aircraftAtGates
    } {
      g.drawImage(airplane.aircraftPicScaledAirport, airplane.currentX * fullWidth, airplane.currentY * fullHeight, null)  // draw airplane image
      g.setFont(new Font("TimesRoman", Font.PLAIN, 10))
      g.drawString(airplane.identifier, airplane.currentX * fullWidth, airplane.currentY * fullHeight)  // draws airplane identifier next to the aircraft
    }
    for {
      airplane: Aircraft <- this.airport.aircraftMoving
    } {
      g.drawImage(airplane.aircraftPicScaledAirport, airplane.currentX * fullWidth, airplane.currentY * fullHeight, null)  // draws airplane image
      if(airplane.almostDestroyed) g.drawImage(airplane.aircraftPicScaledFire, airplane.currentX * fullWidth, airplane.currentY * fullHeight, null)   // draws fire image if airplane is destroyed
      g.setFont(new Font("TimesRoman", Font.PLAIN, 10))
      g.drawString(airplane.identifier, airplane.currentX * fullWidth, airplane.currentY * fullHeight)  // draws airplane identifier next to the aircraft
    }
  }


}
