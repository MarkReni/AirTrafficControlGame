package UI
import ATCG._
import Data._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import scala.swing._
import scala.swing.event._


case class UI_GameOpeningWindow(game: Game) extends SimpleSwingApplication {
  // initialize variables from Game class
  val playerName: String = this.game.playerName
  var playerCoins: Int = this.game.playerCoins
  val timesPlayed: Int = this.game.timesPlayed
  var lockedAirports: Map[String, Boolean] = this.game.lockedAirports
  var latestAirportPressed: String = "Helsinki-Vantaa"  // needed for event handling below

  // WINDOW
  /// menubar
  val menuPanel = new MenuBar {
    contents += new Menu(s"Player ${playerName}")
    contents += new Menu("")
    contents += new Menu(s"Played ${timesPlayed} times")
    contents += new Menu("")
    contents += new Menu(s"Coins $playerCoins")
  }

  /// buttons
  val saveButton = new Button("Save Game") { tooltip = "Press to save current game state." }  // saves the game by using Session class' method
  val quitButton = new Button("Quit Game") { tooltip = "Press to quit the game." }  // new Button(Action("Quit Game") { sys.exit() }) { tooltip = "Press to quit the game. Game state will be lost if not saved before quitting." }
  val resetButton = new Button("Reset Game") { tooltip = "Press to reset the game." }  // resets the game by using Session class' method
  val buttons = new BoxPanel(Orientation.Horizontal) {
    xLayoutAlignment = 100
    contents ++= Vector(
      saveButton,
      new Panel {minimumSize = new Dimension(GameConstants.buttons_separator_width, GameConstants.buttons_separator_height)},
      quitButton,
      new Panel {minimumSize = new Dimension(GameConstants.buttons_separator_width, GameConstants.buttons_separator_height)},
      resetButton
    )
  }

  /// window labels     NOTE! buttons are added to the label panel here
  val welcomeText = new Label(s"Welcome ${this.playerName}! You have ${this.playerCoins} coins.") {
    font = GameConstants.title_font_2
  }
  val ButtonPanelText = new BoxPanel(Orientation.Vertical) {
    contents ++= Vector(
      new BoxPanel(Orientation.Horizontal) { contents ++= Vector(welcomeText, new Panel { minimumSize = new Dimension(50, 20) }) },
      buttons
    )
  }
  val lockedPic: BufferedImage = ImageIO.read(new File("pics/locked.png"))  // picture used when airport is locked
  val lockedPicScaled = lockedPic.getScaledInstance(GameConstants.airport_pic_width + 20, GameConstants.airport_pic_height, java.awt.Image.SCALE_DEFAULT)

  /// Helsinki airport
  val label_Helsinki = new Label("Helsinki-Vantaa Airport")
  val button_Helsinki = new Button {
    tooltip = "Select to open Helsinki-Vantaa Airport."
    val helsinkiPic: BufferedImage = ImageIO.read(new File("pics/Helsinki.jpeg"))
    val helsinkiScaled = helsinkiPic.getScaledInstance(GameConstants.airport_pic_width + 20, GameConstants.airport_pic_height, java.awt.Image.SCALE_DEFAULT)  // code for scaling tilepics
    icon = {
      if(lockedAirports("Helsinki-Vantaa")) new ImageIcon(lockedPicScaled)
      else new ImageIcon(helsinkiScaled)
    }
    preferredSize = new Dimension(GameConstants.airport_pic_width, GameConstants.airport_pic_height)
    maximumSize = new Dimension(GameConstants.airport_pic_width, GameConstants.airport_pic_height)
    text = "Helsinki-Vantaa"
    name = "Helsinki-Vantaa"
  }
  val airportPanelHelsinki = new BoxPanel(Orientation.Vertical) {
    contents ++= Vector(label_Helsinki, button_Helsinki)
  }

  /// Frankfurt airport
  val label_Frankfurt = new Label("Frankfurt Airport")
  val button_Frankfurt = new Button {
    tooltip = "Select to open Frankfurt Airport. Note that you need to first unlock the airport by spending accumulated coins."
    val frankfurtPic: BufferedImage = ImageIO.read(new File("pics/Frankfurt.jpg"))
    val frankfurtScaled = frankfurtPic.getScaledInstance(GameConstants.airport_pic_width + 20, GameConstants.airport_pic_height, java.awt.Image.SCALE_DEFAULT)  // code for scaling tilepics
    icon = {
      if(lockedAirports("Frankfurt")) new ImageIcon(lockedPicScaled)
      else new ImageIcon(frankfurtScaled)
    }
    preferredSize = new Dimension(GameConstants.airport_pic_width, GameConstants.airport_pic_height)
    maximumSize = new Dimension(GameConstants.airport_pic_width, GameConstants.airport_pic_height)
    text = "Frankfurt"
    name = "Frankfurt"
  }
  val airportPanelFrankfurt = new BoxPanel(Orientation.Vertical) {
    contents ++= Vector(label_Frankfurt, button_Frankfurt)
  }

  /// Doha airport
  val label_Doha = new Label("Doha Hamad Airport")
  val button_Doha = new Button {
    tooltip = "Select to open Doha Hamad Airport. Note that you need to first unlock the airport by spending accumulated coins."
    val dohaPic: BufferedImage = ImageIO.read(new File("pics/Doha.jpg"))
    val dohaScaled = dohaPic.getScaledInstance(GameConstants.airport_pic_width + 20, GameConstants.airport_pic_height, java.awt.Image.SCALE_DEFAULT) // code for scaling tilepics
    icon = {
      if(lockedAirports("Doha")) new ImageIcon(lockedPicScaled)
      else new ImageIcon(dohaScaled)
    }
    preferredSize = new Dimension(GameConstants.airport_pic_width, GameConstants.airport_pic_height)
    maximumSize = new Dimension(GameConstants.airport_pic_width, GameConstants.airport_pic_height)
    text = "Doha"
    name = "Doha"
  }
  val airportPanelDoha = new BoxPanel(Orientation.Vertical) {
    contents ++= Vector(label_Doha, button_Doha)
  }

  /// Istanbul airport
  val label_Istanbul = new Label("Istanbul Airport")
  val button_Istanbul = new Button {
    tooltip = "Select to open Istanbul Airport. Note that you need to first unlock the airport by spending accumulated coins."
    val istanbulPic: BufferedImage = ImageIO.read(new File("pics/Istanbul.png"))
    val istanbulScaled = istanbulPic.getScaledInstance(GameConstants.airport_pic_width + 20, GameConstants.airport_pic_height, java.awt.Image.SCALE_DEFAULT) // code for scaling tilepics
    icon = {
      if(lockedAirports("Istanbul")) new ImageIcon(lockedPicScaled)
      else new ImageIcon(istanbulScaled)
    }
    preferredSize = new Dimension(GameConstants.airport_pic_width, GameConstants.airport_pic_height)
    maximumSize = new Dimension(GameConstants.airport_pic_width, GameConstants.airport_pic_height)
    text = "Istanbul"
    name = "Istanbul"
  }
  val airportPanelIstanbul = new BoxPanel(Orientation.Vertical) {
    contents ++= Vector(label_Istanbul, button_Istanbul)
  }

  /// airports together
  val airportsPanel = new GridPanel(2, 2) {
    contents += airportPanelHelsinki
    contents += airportPanelFrankfurt
    contents += airportPanelIstanbul
    contents += airportPanelDoha
  }

  /// add selectionText
  val selectionText = new Label("Select an airport to start playing:") { font = GameConstants.title_font_3 }
  val airportsPanelText = new BoxPanel(Orientation.Vertical) {
    contents ++= Vector(
      new BoxPanel(Orientation.Horizontal) { contents ++= Vector(selectionText, new Panel { minimumSize = new Dimension(50, 20) }) },
      airportsPanel)
  }

  /// unlockPanel
  val lockedText1 = new Label("")
  val lockedText2 = new Label("")
  val unlockButton = new Button("Unlock") { visible = false }
  val cancelButton = new Button("Cancel") { visible = false }
  val selectionPanel = new BoxPanel(Orientation.Vertical) {
    minimumSize = new Dimension(800, 80)
    maximumSize = new Dimension(800, 80)
    preferredSize = new Dimension(800, 80)
    contents ++= Vector(
      new BoxPanel(Orientation.Horizontal) {
        contents ++= Vector(lockedText1, new Panel { minimumSize = new Dimension(650, 20); maximumSize = new Dimension(650, 20); preferredSize = new Dimension(650, 20) })},
      new Panel { minimumSize = new Dimension(650, 10); maximumSize = new Dimension(650, 10); preferredSize = new Dimension(650, 10) },
      new BoxPanel(Orientation.Horizontal) {
        contents ++= Vector(lockedText2,
          new Panel { minimumSize = new Dimension(10, 20); maximumSize = new Dimension(10, 20); preferredSize = new Dimension(10, 20) },
          unlockButton,
          cancelButton,
          new Panel { minimumSize = new Dimension(620, 20); maximumSize = new Dimension(620, 20); preferredSize = new Dimension(620, 20) })},
    )
  }

  /// all panels together
  val togetherPanel = new BoxPanel(Orientation.Vertical) {
    contents ++= Vector(ButtonPanelText, airportsPanelText, selectionPanel)
  }

  /// main window
  val mainFrameOpeningWindow = new MainFrame {
     title = "Air Traffic Controller"
     background = GameConstants.background_color
     preferredSize = new Dimension(550, 700)
     minimumSize = new Dimension(550, 700)
     menuBar = menuPanel
     resizable = false
     contents = new BoxPanel(Orientation.Vertical) {
          contents += togetherPanel
          border = Swing.EmptyBorder(30, 30, 30, 30)
      }
     peer.setLocationRelativeTo(null)
  }

  // EVENT HANDLING
    this.listenTo(button_Helsinki)
    this.listenTo(button_Frankfurt)
    this.listenTo(button_Doha)
    this.listenTo(button_Istanbul)
    this.listenTo(saveButton)
    this.listenTo(resetButton)
    this.listenTo(unlockButton)
    this.listenTo(cancelButton)
    this.listenTo(quitButton)
    this.reactions += {
      case clickEvent: ButtonClicked => {
        val buttonClicked = clickEvent.source
        val text = buttonClicked.text
        if(text == "Save Game") {
          Session.saveGame(this.game.player, this.game)
          lockedText1.text = s"Game has been saved."
          lockedText2.text = s""
          unlockButton.visible = false
          cancelButton.visible = false
        } else if(text == "Quit Game") {
            lockedText1.text = s"Before quitting the game."
            lockedText2.text = s"Would you like to save the game?"
            unlockButton.text = "Save and quit"
            cancelButton.text = "Just quit"
            unlockButton.visible = true
            cancelButton.visible = true
        } else if(text == "Save and quit") {
            Session.saveGame(this.game.player, this.game)
            sys.exit()
        } else if(text == "Just quit") {
            sys.exit()
        } else if(text == "Reset Game") {
              lockedText1.text = s"All game progress will be lost."
              lockedText2.text = s"Are you sure you want to reset the game?"
              unlockButton.text = "Yes"
              cancelButton.text = "Cancel"
              unlockButton.visible = true
              cancelButton.visible = true
        } else if(text == "Yes") {
            this.mainFrameOpeningWindow.close()
            val resetGame: Game = Session.resetGame(this.game)
            new UI_GameOpeningWindow(resetGame).top.visible = true
            // lockedText1.text = s"The game has been reset."
        } else if(text == "Cancel") {
              unlockButton.text = "Unlock"
              lockedText1.text = ""
              lockedText2.text = ""
              unlockButton.visible = false
              cancelButton.visible = false
        } else if(text == "Unlock") {
            val coinsRequired: Int = LevelsData.unlockCost(latestAirportPressed)
            if(playerCoins < coinsRequired) {
              lockedText1.text = s"You don't have enought coins to unlock the airport of ${latestAirportPressed}."
              lockedText2.text = ""
              unlockButton.visible = false
              cancelButton.visible = false
            } else {
              this.game.playerCoins -= coinsRequired  // reduces coin amount by the unlock price of the airport
              this.game.lockedAirports = this.game.lockedAirports - latestAirportPressed + (latestAirportPressed -> false)
              this.mainFrameOpeningWindow.close()
              new UI_GameOpeningWindow(this.game).top.visible = true
              // lockedText1.text = s"The airport of ${latestAirportPressed} has been unlocked."
            }

        } else if(text == "Helsinki-Vantaa") {
            latestAirportPressed = "Helsinki-Vantaa"
            this.mainFrameOpeningWindow.close()
            UI.UI_GameMainWindow(this.game, text).top.visible = true
        } else if(text == "Frankfurt") {
            latestAirportPressed = "Frankfurt"
            if(!lockedAirports("Frankfurt")) {
              this.mainFrameOpeningWindow.close()
              UI.UI_GameMainWindow(this.game, text).top.visible = true
            } else {
              unlockButton.text = "Unlock"
              cancelButton.text = "Cancel"
              lockedText1.text = s"Airport of Frankfurt is locked."
              lockedText2.text = s"Do you want to unlock it for ${LevelsData.unlockCost("Frankfurt")} coins?"
              unlockButton.visible = true
              cancelButton.visible = true
              this.mainFrameOpeningWindow.repaint()
            }
        } else if(text == "Istanbul") {
            latestAirportPressed = "Istanbul"
            if(!lockedAirports("Istanbul")) {
            this.mainFrameOpeningWindow.close()
            UI.UI_GameMainWindow(this.game, text).top.visible = true
          } else {
            unlockButton.text = "Unlock"
            cancelButton.text = "Cancel"
            lockedText1.text = s"Airport of Istanbul is locked."
            lockedText2.text = s"Do you want to unlock it for ${LevelsData.unlockCost("Istanbul")} coins?"
            unlockButton.visible = true
            cancelButton.visible = true
            this.mainFrameOpeningWindow.repaint()
          }
        } else if(text == "Doha") {
            latestAirportPressed = "Doha"
            if(!lockedAirports("Doha")) {
            this.mainFrameOpeningWindow.close()
            UI.UI_GameMainWindow(this.game, text).top.visible = true
          } else {
            unlockButton.text = "Unlock"
            cancelButton.text = "Cancel"
            lockedText1.text = s"Airport of Doha seems is locked."
            lockedText2.text = s"Do you want to unlock it for ${LevelsData.unlockCost("Doha")} coins?"
            unlockButton.visible = true
            cancelButton.visible = true
            this.mainFrameOpeningWindow.repaint()
          }
        } else println("Button clicked!")
      }
  }

  def top = this.mainFrameOpeningWindow


}

