package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("isStanding level 1") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isStanding)
      assert(!Block(Pos(1,1), Pos(2,1)).isStanding)
    }
  }

  test("isLegal level 1") {
    new Level1 {
      assert(Block(Pos(4,7), Pos(4,8)).isLegal)
      assert(!Block(Pos(5,9), Pos(5,9)).isLegal)
    }
  }

  test("neighbours level 1") {
    new Level1 {
      val expectedNeighbours = List(
        (Block(Pos(0,-2), Pos(0,-1)), Left),
        (Block(Pos(0,1), Pos(0,2)), Right),
        (Block(Pos(-2,0), Pos(-1,0)), Up),
        (Block(Pos(1,0), Pos(2,0)), Down)
      )
      assert(Block(Pos(0,0), Pos(0,0)).neighbors == expectedNeighbours)
    }
  }

  test("legal neighbours level 1") {
    new Level1 {
      val expectedLegalNeighbours = List(
        (Block(Pos(0,1), Pos(0,2)), Right),
        (Block(Pos(1,0), Pos(2,0)), Down)
      )
      assert(Block(Pos(0,0), Pos(0,0)).legalNeighbors == expectedLegalNeighbours)
    }
  }

  test("done level 1") {
    new Level1 {
      assert(!done(Block(Pos(4,6), Pos(4,7))))
      assert(done(Block(Pos(4,7), Pos(4,7))))
    }
  }

  test("neighborsWithHistory level 1") {
    new Level1 {
      val block = Block(Pos(1,1),Pos(1,1))
      val history = List(Left,Up)
      val expectedNeighborsWithHistory = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )

      assert(neighborsWithHistory(block, history).toSet == expectedNeighborsWithHistory)
    }
  }

  test("newNeighborsOnly level 1") {
    new Level1 {
      val neighbors = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream
      val history = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      val expectedNeighborsWithHistory = Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)))

      assert(newNeighborsOnly(neighbors, history).toSet == expectedNeighborsWithHistory)
    }
  }

	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
