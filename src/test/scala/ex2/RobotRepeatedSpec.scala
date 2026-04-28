package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotRepeatedSpec extends AnyFlatSpec with Matchers:
  "A Repeating Robot" should "turn multiple times" in:
    val robot = new RobotRepeated(new SimpleRobot((0, 0), Direction.North), 2)

    robot.turn(Direction.East)
    robot.direction should be (Direction.East)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

  it should "act multiple times" in:
    val robot = new RobotRepeated(new SimpleRobot((0, 0), Direction.North), 2)

    robot.act()
    robot.position should be ((0, 2))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be ((2, 2))