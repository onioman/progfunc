package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

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

  var level1Vector = Vector(Vector('-', '-'), Vector('S', 'T'), Vector('o', '-'), Vector('o', 'o'), Vector('-', '-'))

  test("no terrain function level vector 1") {
    new Level1 {
      assert(!terrainFunction(level1Vector)(new Pos(0,0)))
    }
  }

  test("terrain function level vector 1") {
    new Level1 {
      assert(terrainFunction(level1Vector)(new Pos(2,0)))
    }
  }

  test("find S in level vector 1") {
    new Level1 {
      assert(findChar('S', level1Vector) == Pos(1,0))
    }
  }

  test("standing block") {
    new Level1 {
      assert(startBlock.isStanding)
    }
  }

  test("neighbours") {
    new Level1 {
      assert(startBlock.neighbors == List(
        (Block(startPos.dy(-2), startPos.dy(-1)), Left),
        (Block(startPos.dy(1), startPos.dy(2)), Right),
        (Block(startPos.dx(-2), startPos.dx(-1)), Up),
        (Block(startPos.dx(1), startPos.dx(2)), Down)
      ))
    }
  }

  test("legal neighbours") {
    new Level1 {
      assert(startBlock.legalNeighbors == List(
        (Block(startPos.dy(1), startPos.dy(2)), Right),
        (Block(startPos.dx(1), startPos.dx(2)), Down)
      ))
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }


  test("solver neighbors with history from start") {
    new Level1 {
      assert(neighborsWithHistory(startBlock, List()) ==
      Stream(
        (Block(startPos.dy(1), startPos.dy(2)), List(Right)),
        (Block(startPos.dx(1), startPos.dx(2)), List(Down)))
      )
    }
  }

  test("solver neighbors with history from pos") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)) ==
        Stream(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)))
      )
    }
  }

  test("solver new neighbors only") {
    new Level1 {
      assert(newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,

        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))) ==
          Set(
            (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
          ).toStream
      )
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("initial paths 5") {
    new Level1 {
      println(pathsFromStart.toList)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      println("the solution is" + solution)
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
