package UI
import ATCG._
import Data._
import java.awt.Dimension
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import scala.swing.event._
import scala.swing._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success}


object UI_GameStartWindow extends SimpleSwingApplication {
  val newSession: Session.type = Session
  var userName: Promise[String] = Promise[String]
  var userLevel: Int = 0
  var userPoints: Int = 0
  var player: Option[Player] = None
  var game: Promise[Game] = Promise[Game]

  // START WINDOW
  /// welcome labels
  val start_label1 = new Label("Welcome to the Air Traffic Control Game!", icon0=null, align=Alignment.Center)
  val start_label2 = new Label("Please start the game by pressing 'New Game' or 'Load Game'", icon0=null, align=Alignment.Center)

  /// buttons
  val newGameButton = new Button("New Game") { tooltip = "Press to start a new game." }  // new game button
  val loadGameButton = new Button("Load Game") { tooltip = "Press to resume an existent game by selecting a saved file." }  // load game button
  val buttons = new BoxPanel(Orientation.Horizontal) {
    minimumSize = new Dimension(300, 300)
    maximumSize = new Dimension(300, 300)
    preferredSize = new Dimension(300, 300)
    opaque = false
    contents ++= Vector(newGameButton, new Panel { opaque = false; maximumSize = new Dimension(GameConstants.buttons_separator_width, GameConstants.buttons_separator_height) }, loadGameButton)
  }

  /// background picture
  val backgroundPic: BufferedImage = ImageIO.read(new File("pics/airport_background.jpg"))

  /// main frame
  val mainFrameStartWindow = new MainFrame {
    title = "Air Traffic Controller"
    resizable = false
    background = GameConstants.background_color
    minimumSize = new Dimension(GameConstants.UI_Start_image_width, GameConstants.UI_Start_image_height)
    maximumSize = new Dimension(GameConstants.UI_Start_image_width, GameConstants.UI_Start_image_height)
    preferredSize = new Dimension(GameConstants.UI_Start_image_width, GameConstants.UI_Start_image_height)
    contents = new BoxPanel(Orientation.Vertical) {
      contents ++= Vector(new Panel { opaque = false; maximumSize = new Dimension(50, 250) }, buttons)
      border = Swing.EmptyBorder(10, 10, 10, 10)
      override def paintComponent(g: Graphics2D): Unit = {
          g.drawImage(backgroundPic, 0, 0, null)
          g.setFont(GameConstants.title_font_1)
          g.drawString(start_label1.text, 100, 100)
          g.drawString(start_label2.text, 100, 140)
        }
    }
    peer.setLocationRelativeTo(null)
  }

  def top = mainFrameStartWindow

  // EVENT HANDLING
  this.listenTo(newGameButton)
  this.listenTo(loadGameButton)
  this.reactions += {
    case clickEvent: ButtonClicked => {
      val buttonClicked = clickEvent.source
      val text = buttonClicked.text
      if(text == "New Game") {
        mainFrameStartWindow.close()
        new UI_UserNameWindow(this).top.visible = true  // opens window where user name can be typed in
        val userNameFuture: Future[String] = userName.future
        userNameFuture.foreach {
          userName: String => {
            player = Option(newSession.newGame(userName))
            game.success(this.createGame(player))
          }
        }
      } else {  // load game
        mainFrameStartWindow.close()
        val fileText: String = openFile()
        player = newSession.loadGame(fileText)  // loads the game by using Session class method
        game.success(this.createGame(player))
      }

      val gameFuture: Future[Game] = game.future
      gameFuture.onComplete {
        case Success(readyGame) => {
          readyGame.timesPlayed += 1
          new UI_GameOpeningWindow(readyGame).top.visible = true  // opens up the UI_GameOpeningWindow
        }
        case Failure(ex) => println(s" Couldn't create a game session! Cause: ${ex.printStackTrace()}")
      }
    }
  }

  // AUXILIARY METHODS
  def openFile(): String = {
    val chooser = new FileChooser
    var source: BufferedSource = null
    if(chooser.showOpenDialog(null)==FileChooser.Result.Approve) {
      source = Source.fromFile(chooser.selectedFile)
      val text = source.mkString
      val uri = chooser.selectedFile.toURI
      val fileName: String = uri.toString.split("/").last
      source.close()
      fileName
    } else {
      ""
    }
  }

  def createGame(player: Option[Player]): Game = {
    player match {
      case Some(player) => newSession.createGame(player)
      case None => {
        throw new Exception("Player not created")
      }
    }
  }

  def userName_=(name: String): Unit = this.userName.success(name)  // completes userName promise with success


}
