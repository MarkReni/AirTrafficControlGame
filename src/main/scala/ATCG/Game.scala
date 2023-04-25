package ATCG


class Game(val player: Player) {
  val playerName: String = this.player.name
  var playerCoins: Int = this.player.coinsGained
  var playerLevels: Map[String, Int] = this.player.levels
  var timesPlayed: Int = this.player.timesPlayed
  var lockedAirports: Map[String, Boolean] = player.locked


}
