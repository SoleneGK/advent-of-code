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
	fmt.Printf("The answer for part 1 is %d\n", distance)
}

type Ship struct {
	Position  Coordinates
	Direction string
}

func (s *Ship) Initialize() {
	s.Position = Coordinates{0, 0}
	s.Direction = "E"
}

func (s *Ship) ApplyInstructionList(instructionList io.Reader) {
	scanner := bufio.NewScanner(instructionList)

	for scanner.Scan() {
		instructionType, distance := s.readInstruction(scanner.Text())
		s.executeInstruction(instructionType, distance)
	}
}

func (s *Ship) readInstruction(rawInstruction string) (string, int) {
	instructionType := string(rawInstruction[:1])
	distance, _ := strconv.Atoi(rawInstruction[1:])
	return instructionType, distance
}

func (s *Ship) executeInstruction(instructionType string, distance int) {
	switch instructionType {
	case "N", "S", "E", "W":
		s.moveInCardinalDirection(instructionType, distance)
	case "R", "L":
		s.rotate(instructionType, distance)
	case "F":
		s.moveInShipDirection(distance)
	}
}

func (s *Ship) moveInCardinalDirection(cardinalDirection string, distance int) {
	s.Position.ApplyVectorNTimes(cardinalVectors[cardinalDirection], distance)
}

func (s *Ship) rotate(rotationDirection string, totalAngle int) {
	currentDirection := s.Direction

	for angle := totalAngle; angle > 0; angle = angle - 90 {
		currentDirection = s.getNewDirection(currentDirection, rotationDirection)
	}

	s.Direction = currentDirection
}

func (s *Ship) getNewDirection(currentDirection, rotationDirection string) (newDirection string) {
	if rotationDirection == "R" {
		switch currentDirection {
		case "N":
			newDirection = "E"
		case "E":
			newDirection = "S"
		case "S":
			newDirection = "W"
		case "W":
			newDirection = "N"
		}
	} else {
		switch currentDirection {
		case "N":
			newDirection = "W"
		case "W":
			newDirection = "S"
		case "S":
			newDirection = "E"
		case "E":
			newDirection = "N"
		}
	}

	return
}

func (s *Ship) moveInShipDirection(distance int) {
	s.Position.ApplyVectorNTimes(cardinalVectors[s.Direction], distance)
}

var cardinalVectors = map[string]Coordinates{
	"N": Coordinates{1, 0},
	"S": Coordinates{-1, 0},
	"E": Coordinates{0, 1},
	"W": Coordinates{0, -1},
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
