package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

// Getting the byte code of used symbols
var tree = []byte("#")[0]
var openGround = []byte(".")[0]

type Coordinates struct {
	X int
	Y int
}

func main() {
	// Get data from file and store it in a double-entry table
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	areaMap := getAreaMap(file)

	slopeList := []Coordinates{
		Coordinates{1, 1},
		Coordinates{3, 1},
		Coordinates{5, 1},
		Coordinates{7, 1},
		Coordinates{1, 2},
	}

	numberOfTreeList := []int{}

	// How many tree with each slope?
	for _, slope := range slopeList {
		startPosition := Coordinates{0, 0}

		numberOfTree := getTreeNumber(areaMap, startPosition, slope)
		fmt.Printf("With slope %v, you would encounter %d trees\n", slope, numberOfTree)

		numberOfTreeList = append(numberOfTreeList, numberOfTree)
	}

	// Let's calculate the product
	product := 1

	for _, value := range numberOfTreeList {
		product *= value
	}

	fmt.Printf("The product is %v\n", product)

}

// Putting data from file into a double-entry slice
func getAreaMap(handle io.Reader) (areaMap [][]byte) {
	scanner := bufio.NewScanner(handle)

	for scanner.Scan() {
		line := []byte(scanner.Text())
		areaMap = append(areaMap, line)
	}

	return
}

func getTreeNumber(areaMap [][]byte, startPosition, slope Coordinates) int {
	numberOfTree := 0
	mapHeight, mapWidth := getMapSize(areaMap)
	position := getNewPosition(startPosition, slope, mapWidth)

	for position.Y < mapHeight {
		if hasATreeAtPosition(areaMap, position) {
			numberOfTree++
		}
		position = getNewPosition(position, slope, mapWidth)
	}

	return numberOfTree
}

func getMapSize(areaMap [][]byte) (height, width int) {
	height = len(areaMap)

	if height == 0 {
		width = 0
	} else {
		width = len(areaMap[0])
	}

	return
}

func hasATreeAtPosition(areaMap [][]byte, position Coordinates) bool {
	return isATree(areaMap[position.Y][position.X])
}

func isATree(spotToCheck byte) bool {
	return spotToCheck == tree
}

func getNewPosition(position, slope Coordinates, mapWidth int) Coordinates {
	position.X = (position.X + slope.X) % mapWidth
	position.Y += slope.Y
	return position
}
