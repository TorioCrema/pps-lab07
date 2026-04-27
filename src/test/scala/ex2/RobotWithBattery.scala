package ex2

import ex2.Direction.{East, North, West}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatterySpec extends AnyFlatSpec with Matchers:
  "A Robot With Battery" should "turn correctly when charged" in:
    val robot = new RobotWithBattery(new SimpleRobot((0, 0), North), 2)

    robot.turn(Direction.East)
    robot.direction should be (Direction.East)

    robot.turn(Direction.South)
    robot.direction should be (Direction.South)

    robot.turn(Direction.West)
    robot.direction should be (Direction.South)

  it should "act correctly when charged" in:
    val robot = new RobotWithBattery(new SimpleRobot((0, 0), North), 2)

    robot.act()
    robot.position should be ((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be ((0, 1))
