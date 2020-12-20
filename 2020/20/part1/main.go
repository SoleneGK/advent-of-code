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

	answer := getAnswerPart1(file)
	fmt.Printf("The answer is %d\n", answer)
}

const tileSize = 10

func getAnswerPart1(file io.Reader) int {
	tileList := getTileList(file)
	tileList.associateTiles()
	cornerIds := tileList.getCornerIds()
	return getProduct(cornerIds)
}

func getTileList(file io.Reader) (tileList TileList) {
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		tileList = append(tileList, getNewTile(scanner))
	}

	return
}

func getNewTile(scanner *bufio.Scanner) *Tile {
	if scanner.Text() == "" {
		scanner.Scan()
	}

	line := scanner.Text()

	newTile := &Tile{}
	newTile.Id = extractId(line)
	newTile.Borders = extractBorders(scanner)

	return newTile
}

func extractId(line string) int {
	idAsString := line[5:9]
	id, _ := strconv.Atoi(idAsString)
	return id
}

func extractBorders(scanner *bufio.Scanner) (borderList []*Border) {
	tileContent := extractTileContent(scanner)

	topBorder := tileContent[0]
	borderList = append(borderList, newBorder(topBorder))

	bottomBorder := tileContent[tileSize-1]
	borderList = append(borderList, newBorder(bottomBorder))

	leftBorder := getColumn(tileContent, 0)
	borderList = append(borderList, newBorder(leftBorder))

	rightBorder := getColumn(tileContent, tileSize-1)
	borderList = append(borderList, newBorder(rightBorder))

	return
}

func extractTileContent(scanner *bufio.Scanner) (tileContent []string) {
	for i := 1; i <= tileSize; i++ {
		scanner.Scan()
		tileContent = append(tileContent, scanner.Text())
	}
	return
}

func getColumn(stringTable []string, index int) string {
	column := []byte{}

	for i := 0; i < len(stringTable); i++ {
		column = append(column, stringTable[i][index])
	}

	return string(column)
}

func getProduct(numberList []int) int {
	product := 1

	for _, number := range numberList {
		product *= number
	}

	return product
}

type TileList []*Tile

func (t TileList) associateTiles() {
	tilesToAssociate := make(TileList, len(t))
	copy(tilesToAssociate, t)

	for _, tile := range tilesToAssociate {
		tilesToAssociate = tilesToAssociate[1:]

		for _, currentTile := range tilesToAssociate {
			tile.AssociateBorders(currentTile)
		}
	}
}

func (t TileList) getCornerIds() (cornerIds []int) {
	for _, tile := range t {
		if tile.getNumberOfSharedBorders() == 2 {
			cornerIds = append(cornerIds, tile.Id)
		}
	}
	return
}

type Tile struct {
	Id      int
	Borders []*Border
}

func (t *Tile) AssociateBorders(tile2 *Tile) {
	for _, tBorder := range t.Borders {
		if tBorder.Associated {
			continue
		}

		for _, tile2Border := range tile2.Borders {
			if tBorder.Content == tile2Border.Content || tBorder.Content == tile2Border.ReversedContent {
				tBorder.Associated = true
				tile2Border.Associated = true
			}
		}
	}

}

func (t Tile) getNumberOfSharedBorders() (number int) {
	for _, border := range t.Borders {
		if border.Associated {
			number++
		}
	}
	return
}

type Border struct {
	Content         string
	ReversedContent string
	Associated      bool
}

func newBorder(content string) *Border {
	border := &Border{Content: content}
	border.ReversedContent = reverse(content)
	return border
}

func reverse(input string) string {
	stringToReverse := []byte(input)
	reversedString := []byte{}

	for i := len(stringToReverse) - 1; i >= 0; i-- {
		reversedString = append(reversedString, stringToReverse[i])
	}

	return string(reversedString)
}
