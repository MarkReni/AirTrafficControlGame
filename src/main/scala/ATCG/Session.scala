package ATCG
import IO.JsonSaveFile


object Session {
  def loadGame(fileName: String): Option[Player] = JsonSaveFile.loadUserFile(fileName)

  // creates an Player object
  def newGame(userName: String): Player = {
    val cointsGained: Int = 0
    val timesPlayed: Int = 0
    val levels: Map[String, Int] = Map("Helsinki-Vantaa" -> 1, "Frankfurt" -> 1, "Istanbul" -> 1, "Doha" -> 1)
    val locked: Map[String, Boolean] = Map("Helsinki-Vantaa" -> false, "Frankfurt" -> true, "Istanbul" -> true, "Doha" -> true)
    new Player(userName, cointsGained, timesPlayed, levels, locked)
  }

  // creates a Game instance
  def createGame(user: Player): Game = new Game(user)

  // saves the game by storing the Game class object's parameter values to the Player class object and using the JasonSaveFile object to create a file into the computer's directory from the Player object
  def saveGame(user: Player, game: Game): Unit = {
    user.levels = game.playerLevels
    user.coinsGained = game.playerCoins
    user.timesPlayed = game.timesPlayed
    JsonSaveFile.saveFile(s"${user.name}.json", JsonSaveFile.toJson(user))
  }

  // resets all progress to zero in the Game instance
  def resetGame(game: Game): Game = {
    val levels: Map[String, Int] = Map("Helsinki-Vantaa" -> 1, "Frankfurt" -> 1, "Istanbul" -> 1, "Doha" -> 1)
    game.playerLevels = levels
    game.playerCoins = 0
    game.timesPlayed = 0
    game.lockedAirports = Map("Helsinki-Vantaa" -> false, "Frankfurt" -> true, "Istanbul" -> true, "Doha" -> true)
    game
  }


}
