package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	ship := Ship{}
	ship.Initialize()
	ship.ApplyInstructionList(file)

	distance := ship.GetManhattanDistance()
	fmt.Printf("The answer for part 2 is %d\n", distance)
}

type Ship struct {
	Position Coordinates
	Waypoint Coordinates
}

func (s *Ship) Initialize() {
	s.Position = Coordinates{0, 0}
	s.Waypoint = Coordinates{1, 10}
}

func (s *Ship) ApplyInstructionList(instructionList io.Reader) {
	scanner := bufio.NewScanner(instructionList)

	for scanner.Scan() {
		instructionType, instructionValue := s.readInstruction(scanner.Text())
		s.executeInstruction(instructionType, instructionValue)
	}
}

func (s *Ship) readInstruction(rawInstruction string) (string, int) {
	instructionType := string(rawInstruction[:1])
	instructionValue, _ := strconv.Atoi(rawInstruction[1:])
	return instructionType, instructionValue
}

func (s *Ship) executeInstruction(instructionType string, instructionValue int) {
	switch instructionType {
	case "N", "S", "E", "W":
		s.moveWaypoint(instructionType, instructionValue)
	case "F":
		s.moveShip(instructionValue)
	case "R", "L":
		s.rotateWaypoint(instructionType, instructionValue)
	}
}

func (s *Ship) moveWaypoint(cardinalPoint string, distance int) {
	s.Waypoint.ApplyVectorNTimes(cardinalVectors[cardinalPoint], distance)
}

var cardinalVectors = map[string]Coordinates{
	"N": Coordinates{1, 0},
	"S": Coordinates{-1, 0},
	"E": Coordinates{0, 1},
	"W": Coordinates{0, -1},
}

func (s *Ship) moveShip(distance int) {
	s.Position.ApplyVectorNTimes(s.Waypoint, distance)
}

func (s *Ship) rotateWaypoint(directionOfRotation string, angle int) {
	numberOfRotationsBy90Degrees := angle / 90

	if directionOfRotation == "R" {
		s.rotateWaypointClockwiseNTimes(numberOfRotationsBy90Degrees)
	} else {
		s.rotateWaypointCounterclockwiseNTimes(numberOfRotationsBy90Degrees)
	}
}

func (s *Ship) rotateWaypointClockwiseNTimes(times int) {
	for i := 1; i <= times; i++ {
		s.Waypoint = Coordinates{
			NS: -s.Waypoint.EW,
			EW: s.Waypoint.NS,
		}
	}
}

func (s *Ship) rotateWaypointCounterclockwiseNTimes(times int) {
	for i := 1; i <= times; i++ {
		s.Waypoint = Coordinates{
			NS: s.Waypoint.EW,
			EW: -s.Waypoint.NS,
		}
	}
}

func (s *Ship) GetManhattanDistance() int {
	return Abs(s.Position.NS) + Abs(s.Position.EW)
}

func Abs(x int) int {
	if x < 0 {
		return -x
	} else {
		return x
	}
}

// Norch is positive, South is negative
// East is positive, West is negative
type Coordinates struct {
	NS int
	EW int
}

func (c *Coordinates) ApplyVectorNTimes(vector Coordinates, factor int) {
	c.NS += factor * vector.NS
	c.EW += factor * vector.EW
}
