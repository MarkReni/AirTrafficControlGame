package ATCG
import UI._


object Main extends App {
  new UI_GameMainWindow(new Game(new Player("Mark", 500, 5, Map("Helsinki-Vantaa" -> 9, "Frankfurt" -> 7, "Istanbul" -> 7, "Doha" -> 7), Map("Helsinki-Vantaa" -> false, "Frankfurt" -> false, "Istanbul" -> true, "Doha" -> true))), "Helsinki-Vantaa").top.visible = true


}



