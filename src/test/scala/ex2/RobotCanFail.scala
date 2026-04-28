package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotCanFailSpec extends AnyFlatSpec with Matchers:
  "A Robot with chance of failure" should "turn when random chance succeeds" in:
    val robot = new RobotCanFail(new SimpleRobot((0, 0), Direction.North), 0.5)
    robot.setSeed(42)

    robot.turn(Direction.East)
    robot.direction should be (Direction.East)

    robot.turn(Direction.West)
    robot.direction should be (Direction.West)

    robot.turn(Direction.South)
    robot.direction should be (Direction.West)

  it should "act when random chance succeeds" in:
    val robot = new RobotCanFail(new SimpleRobot((0, 0), Direction.North), 0.5)
    robot.setSeed(42)

    robot.act()
    robot.position should be ((0, 1))

    robot.act()
    robot.position should be ((0, 2))

    robot.act()
    robot.position should be ((0, 2))