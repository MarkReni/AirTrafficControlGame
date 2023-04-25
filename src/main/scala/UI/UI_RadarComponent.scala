package UI
import ATCG._
import Auxiliary.Animator
import Data._
import java.awt.{Color, Dimension, Font}
import scala.swing._


case class UI_RadarComponent(airport: Airport) extends Component {
  tooltip = "Radar winodow displays all planes currently waiting on the same height levels."
  val width = Data.GameConstants.radarWindow_width
  val height = Data.GameConstants.radarWindow_height
  minimumSize = new Dimension(width, height)
  preferredSize = new Dimension(width, height)
  maximumSize = new Dimension(width, height)
  val squareWidth = Data.GameConstants.radarWindow_square_width
  val squareHeight =  Data.GameConstants.radarWindow_square_height
  val squareBorder = Data.GameConstants.radarWindow_square_border
  val fullWidth = squareWidth + squareBorder
  val fullHeight = squareHeight + squareBorder
  val runaway_X_start: Int = 22  // runway X-coordinate at the radar
  val runaway_Y_start: Int = 38  // runway Y-coordinate at the radar
  // PICTURES
  val radarPic: Image = Animator.scalePic("pics/radar.jpg", GameConstants.radarScaled_size, GameConstants.radarScaled_size)
  val runawayVerticalStart: Image = Animator.scalePic("pics/tile_0405.png", 10, 10)
  val runawayVerticalMiddle: Image = Animator.scalePic("pics/tile_0432.png", 10, 10)
  val runawayVerticalEnd: Image = Animator.scalePic("pics/tile_0459.png", 10, 10)

  override def paintComponent(g: Graphics2D): Unit = {
    g.drawImage(radarPic, 0, 0, null)  // draws radar picture
    g.drawImage(runawayVerticalStart, runaway_X_start * fullWidth, runaway_Y_start * fullHeight, null)
    g.drawImage(runawayVerticalMiddle, runaway_X_start * fullWidth, (runaway_Y_start + 1) * fullHeight, null)
    g.drawImage(runawayVerticalEnd, runaway_X_start * fullWidth, (runaway_Y_start + 2) * fullHeight, null)
    for {
      airplane: Aircraft <- (this.airport.aircraftAtRadar ++ this.airport.aircraftAtMissed)
    } {
      g.drawImage(airplane.aircraftPicScaledRadar, airplane.currentX * fullWidth, airplane.currentY * fullHeight, null)  // draws aircraft image
      if(airplane.almostDestroyed) g.drawImage(airplane.aircraftPicScaledFire, airplane.currentX * fullWidth, airplane.currentY * fullHeight, null)   // draws the fire image if airplane is destroyed
      g.setColor(Color.WHITE)
      g.setFont(new Font("TimesRoman", Font.PLAIN, 10))
      g.drawString(airplane.identifier, airplane.currentX * fullWidth, airplane.currentY * fullHeight)  // draws the identifier next to the aircraft
    }
  }


}
