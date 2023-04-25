package Data


object LevelsData {
  final val maxLevel: Int = 10  // the max level of the game; player can advance to level 10 at each airport separately
  final val maxTakeoffs: Int = 3  // how many takeoffs until coins are rewarded
  final val coinsRewardedForTakeoffs: Int = 10  // how many coins are rewarded after maxTakeoffs
  final val coinsRewardedForLevel: Int = 30  // how many coins are rewarded after completing a level
  final val coinsDeductedAfterCollision: Int = 5  // how many coins are deducted after a collision
  final val coinsDeductedAfterDestruction: Int = 5  // how many coins are deducted after some amount of destructions
  final val maxDestroyedAircraft: Int = 3  // max destroyed aircraft after which the takeoff amount resets
  final val levelsRange: Map[Int, Int] = Map(1 -> 3, 2 -> 4, 3 -> 5, 4 -> 6, 5 -> 7, 6 -> 8, 7 -> 9, 8 -> 10, 9 -> 11, 10 -> 12)  // Map(airportlevel -> max takeoff amount for the airportlevel in question)
  final val unlockCost: Map[String, Int] = Map("Helsinki-Vantaa" -> 0, "Frankfurt" -> 100, "Istanbul" -> 200, "Doha" -> 300)    // how much coins are needed to unlock the airport
  final val movementDirectionVertical: Vector[String] = Vector("down", "up")  // vertical movement directions
  final val movementDirectionHorizontal: Vector[String] = Vector("right", "left")  // horizontal movement directions
  final val difficultyData: Map[String, Map[Int, Tuple6[Int, Boolean, Boolean, Int, Boolean, Int]]] = Map(
    "Helsinki-Vantaa" -> Map(
        // difficulty level -> Tuple(aircraftFrequency, boeingCreated, airbusCreated, BoeingAirbusProbability (max 11), weatherDistruption, weatherChangeProbability (max 11))
        1 -> Tuple6(0, false, false, 0, false, 0),
        2 -> Tuple6(2, false, false, 0, false, 0),
        3 -> Tuple6(4, false, false, 0, false, 0),
        4 -> Tuple6(5, true, false, 1, false, 0),
        5 -> Tuple6(6, true, false, 1, false, 0),
        6 -> Tuple6(7, true, false, 1, false, 0),
        7 -> Tuple6(8, true, true, 2, false, 0),
        8 -> Tuple6(9, true, true, 3, false, 0),
        9 -> Tuple6(10, true, true, 4, false, 0),
        10 -> Tuple6(12, true, true, 5, false, 0)
      ),
    "Frankfurt" -> Map(
        1 -> Tuple6(0, false, false, 0, false, 0),
        2 -> Tuple6(2, false, false, 0, false, 0),
        3 -> Tuple6(4, true, false, 0, false, 0),
        4 -> Tuple6(5, true, false, 0, false, 0),
        5 -> Tuple6(6, true, false, 0, false, 0),
        6 -> Tuple6(7, true, true, 3, true, 1),
        7 -> Tuple6(8, true, true, 3, true, 1),
        8 -> Tuple6(9, true, true, 4, true, 2),
        9 -> Tuple6(10, true, true, 4, true, 2),
        10 -> Tuple6(12, true, true, 5, true, 3)
      ),
    "Istanbul" -> Map(
        1 -> Tuple6(0, false, false, 0, false, 0),
        2 -> Tuple6(2, true, false, 0, false, 0),
        3 -> Tuple6(4, true, false, 0, false, 0),
        4 -> Tuple6(5, true, false, 0, false, 0),
        5 -> Tuple6(6, true, true, 4, true, 1),
        6 -> Tuple6(7, true, true, 4, true, 1),
        7 -> Tuple6(8, true, true, 5, true, 2),
        8 -> Tuple6(9, true, true, 5, true, 2),
        9 -> Tuple6(10, true, true, 6, true, 3),
        10 -> Tuple6(12, true, true, 6, true, 3)
      ),
    "Doha" -> Map(
        1 -> Tuple6(0, true, false, 0, false, 0),
        2 -> Tuple6(2, true, false, 0, false, 0),
        3 -> Tuple6(4, true, false, 0, false, 0),
        4 -> Tuple6(5, true, true, 4, true, 1),
        5 -> Tuple6(6, true, true, 4, true, 1),
        6 -> Tuple6(7, true, true, 5, true, 2),
        7 -> Tuple6(8, true, true, 5, true, 2),
        8 -> Tuple6(9, true, true, 6, true, 3),
        9 -> Tuple6(10, true, true, 6, true, 3),
        10 -> Tuple6(12, true, true, 6, true, 4)
      )
  )
  final val status: Vector[String] = Vector("Gate", "GateReady", "Pushback", "Takeoff", "RunwayTakeoff", "HomeGate", "RunwayLand", "RunwayReady", "Approaching", "RunwayMissed")  // aircraft states that control aircraft movement according to commands given
  final val aircraftDestination: Vector[String] = Vector("Away", "Home")  // "Away" for departing aircraft; "Home" for arriving aircraft
  final val aircraftType: Vector[String] = Vector("Bombardier", "Boeing", "Airbus")  // aircraft types in the game; introduces some difficulty in the game


}
