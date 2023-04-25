package UI
import Data.GameConstants
import scala.swing._
import scala.swing.event._


class UI_UserNameWindow(UI_start: UI_GameStartWindow.type) extends SimpleSwingApplication {
  val inputNameLabel = new Label("Type in username that will be used in the game:")
  val textField = new TextField("", 50)
  val mainFrameGameWindow = new MainFrame() {
    preferredSize = new Dimension(500, 70)
    resizable = false
    background = GameConstants.background_color
    contents = new BoxPanel(Orientation.Vertical) {
      tooltip = "Type in username that will be used in the game."
      contents ++= Vector(inputNameLabel, textField)
    }
    peer.setLocationRelativeTo(null)
  }

  def top = mainFrameGameWindow

  // EVENT HANDLING
  this.listenTo(textField)

  this.reactions += {
    case EditDone(textField: TextField) => {
      val inputName: String = textField.text
      mainFrameGameWindow.close()
      UI_start.userName_=(inputName)
    }
  }


}

