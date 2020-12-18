package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
	"unicode/utf8"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	startingState := getStartingState(file)
	finalState := getFinalState(startingState)

	fmt.Printf("There are %d cubes in active state after the sixth cycle\n", finalState.GetNumberOfActiveCubes())
}

var (
	active, _   = utf8.DecodeRuneInString("#")
	inactive, _ = utf8.DecodeRuneInString(".")
)

func getStartingState(reader io.Reader) CubeList {
	scanner := bufio.NewScanner(reader)

	startingState := CubeList{}
	x := 0

	for scanner.Scan() {
		line := scanner.Text()

		for y := 0; y < len(line); y++ {
			state := rune(line[y])

			if state == active {
				startingState.AddCubeWithCoordinates(x, y, 0, active)
			}
		}

		x++
	}

	return startingState
}

func getFinalState(startingState CubeList) CubeList {
	for i := 1; i <= 6; i++ {
		startingState = getNextState(startingState)
	}

	return startingState
}

func getNextState(initialState CubeList) CubeList {
	newCubeListState := CubeList{}
	inactiveCubeToTest := CubeList{}

	for key := range initialState {
		newState, inactiveNeighbours := initialState.getNewStateForActiveCube(key)

		if newState == active {
			newCubeListState.AddCubeWithKey(key, active)
		}

		for _, neighbourKey := range inactiveNeighbours {
			inactiveCubeToTest.AddCubeWithKey(neighbourKey, inactive)
		}
	}

	for key := range inactiveCubeToTest {
		newState := initialState.getNewStateForInactiveCube(key)

		if newState == active {
			newCubeListState.AddCubeWithKey(key, active)
		}
	}

	return newCubeListState
}

// Format for position: x,y,z
type CubeList map[string]rune

func (c CubeList) AddCubeWithCoordinates(x, y, z int, state rune) {
	c[getKey(x, y, z)] = state
}

func (c CubeList) AddCubeWithKey(key string, state rune) {
	c[key] = state
}

func (c CubeList) getNewStateForActiveCube(key string) (newState rune, inactiveNeighbours []string) {
	neighboursKeys := getNeighboursKeys(key)
	inactiveNeighbours = c.getInactiveNeighbours(neighboursKeys)
	newState = getNewState(active, 26-len(inactiveNeighbours))

	return
}

func (c CubeList) getNewStateForInactiveCube(key string) (newState rune) {
	neighboursKeys := getNeighboursKeys(key)
	numberOfActiveNeighbours := c.getNumberOfActiveNeighbours(neighboursKeys)
	return getNewState(inactive, numberOfActiveNeighbours)
}

func getNeighboursKeys(key string) (neighboursKeys []string) {
	baseX, baseY, baseZ := getCoordinates(key)

	for x := baseX - 1; x <= baseX+1; x++ {
		for y := baseY - 1; y <= baseY+1; y++ {
			for z := baseZ - 1; z <= baseZ+1; z++ {
				if x != baseX || y != baseY || z != baseZ {
					neighboursKeys = append(neighboursKeys, getKey(x, y, z))
				}
			}
		}
	}

	return
}

func (c CubeList) getInactiveNeighbours(neighboursKeys []string) (inactiveNeighbours []string) {
	for _, key := range neighboursKeys {
		_, exists := c[key]

		if !exists {
			inactiveNeighbours = append(inactiveNeighbours, key)
		}
	}

	return
}

func (c CubeList) getNumberOfActiveNeighbours(neighboursKeys []string) (number int) {
	for _, key := range neighboursKeys {
		_, exists := c[key]

		if exists {
			number++
		}
	}

	return
}

func getNewState(initialState rune, numberOfActiveNeighbours int) rune {
	if initialState == active {
		if numberOfActiveNeighbours == 2 || numberOfActiveNeighbours == 3 {
			return active
		} else {
			return inactive
		}
	} else {
		if numberOfActiveNeighbours == 3 {
			return active
		} else {
			return inactive
		}
	}
}

func (c CubeList) GetNumberOfActiveCubes() int {
	return len(c)
}

func getKey(x, y, z int) string {
	return fmt.Sprintf("%d,%d,%d", x, y, z)
}

func getCoordinates(key string) (x, y, z int) {
	valuesAsString := strings.Split(key, ",")

	x, _ = strconv.Atoi(valuesAsString[0])
	y, _ = strconv.Atoi(valuesAsString[1])
	z, _ = strconv.Atoi(valuesAsString[2])

	return
}
