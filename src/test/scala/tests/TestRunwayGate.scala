package tests
import ATCG._
import org.scalatest.flatspec._
import org.scalatest.matchers._


class TestRunwayGate extends AnyFlatSpec with should.Matchers {

  "Runaway" should "be filled with Tiles" in {
    val runaway = new Runway(3,5,2,4, "R1")
    assume(runaway(0, 1).isInstanceOf[None.type])
    assume(runaway(4, 3).isInstanceOf[Some[Tile]])
    assume(runaway(4, 3).forall(_.isInstanceOf[Tile]))
    println(runaway.location)
  }

  "Gate" should "be filled with Tiles" in {
    val gate = new Gate(1,5,2,2, "1")
    assume(gate(4, 1).isInstanceOf[None.type])
    assume(gate(4, 2).isInstanceOf[Some[Tile]])
    assume(gate(0, 0).forall(_.isInstanceOf[Tile]))
    println(gate.location)
  }


}
