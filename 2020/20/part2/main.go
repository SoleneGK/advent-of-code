package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	answer := getAnswerPart2(file)
	fmt.Printf("The answer is %d\n", answer)
}

const tileSize = 10

func getAnswerPart2(file io.Reader) int {
	tileList := getTileList(file)
	// Hey program, i'm gonna teach you how to do a puzzle
	seaMap := tileList.getMap()
	return getRoughness(seaMap)
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

	title := scanner.Text()

	newTile := &Tile{}
	newTile.Id = extractId(title)
	tileFullContent := extractFullContent(scanner)
	newTile.setContent(tileFullContent)
	newTile.setBorders(tileFullContent)

	return newTile
}

func extractId(line string) int {
	idAsString := line[5:9]
	id, _ := strconv.Atoi(idAsString)
	return id
}

func extractFullContent(scanner *bufio.Scanner) (content []string) {
	for i := 1; i <= tileSize; i++ {
		scanner.Scan()
		content = append(content, scanner.Text())
	}
	return
}

func getRoughness(seaMap []string) int {
	numberOfHashtags := getNumberOfHash(seaMap)
	numberOfSeaMonsters := getNumberOfSeaMonsters(seaMap)
	return numberOfHashtags - getNumberOfHash(seaMonster)*numberOfSeaMonsters
}

func getNumberOfHash(seaMap []string) (number int) {
	for _, line := range seaMap {
		number += strings.Count(line, "#")
	}
	return
}

func getNumberOfSeaMonsters(seaMap []string) int {
	testedString := seaMap
	numberOfSeaMonsters := lookForSeaMonsters(testedString)
	if numberOfSeaMonsters > 0 {
		return numberOfSeaMonsters
	}

	for i := 1; i <= 3; i++ {
		testedString = rotateClockwise(testedString)
		numberOfSeaMonsters = lookForSeaMonsters(testedString)
		if numberOfSeaMonsters > 0 {
			return numberOfSeaMonsters
		}
	}

	testedString = flipTopBottom(seaMap)
	numberOfSeaMonsters = lookForSeaMonsters(testedString)
	if numberOfSeaMonsters > 0 {
		return numberOfSeaMonsters
	}

	for i := 1; i <= 3; i++ {
		testedString = rotateClockwise(testedString)
		numberOfSeaMonsters = lookForSeaMonsters(testedString)
		if numberOfSeaMonsters > 0 {
			return numberOfSeaMonsters
		}
	}

	return 0
}

func lookForSeaMonsters(seaMap []string) (number int) {
	lineLength := len(seaMap[0])

	// Looking for the # on first line of the sea monster
	// Hic sunt dracones
	for col := 18; col < lineLength-2; col++ {
		for row := 0; row < len(seaMap)-2; row++ {
			// Could that be the eye of a sea monster ? Let's look !
			if seaMap[row][col] == hash {
				scannedArea := extractArea(row, col, seaMap)

				if containsASeaMonster(scannedArea) {
					// I've got one !
					number++
				}
			}
		}
	}

	return
}

func extractArea(eyeRow, eyeCol int, seaMap []string) []string {
	return []string{
		seaMap[eyeRow+1][eyeCol-18 : eyeCol+2],
		seaMap[eyeRow+2][eyeCol-18 : eyeCol+2],
	}
}

var hash = []byte("#")[0]

// I'm keeping this var because the monster is cute
var seaMonster = []string{
	"                  # ",
	"#    ##    ##    ###",
	" #  #  #  #  #  #   ",
}
var seaMonsterBody = []string{
	seaMonster[1],
	seaMonster[2],
}

// I've already found the eye, i'm looking now for the body
func containsASeaMonster(scannedArea []string) bool {
	for row := 0; row <= 1; row++ {
		for col := 0; col < len(seaMonsterBody[0]); col++ {
			if seaMonsterBody[row][col] == hash && scannedArea[row][col] != hash {
				// No sea monster :'(
				return false
			}
		}
	}
	return true
}

type TileList []*Tile

func (t *TileList) getMap() (seaMap []string) {
	t.associateTiles()
	return t.generateMap()
}

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

func (t *TileList) generateMap() (seaMap []string) {
	firstCorner := t.getCorner()
	firstCorner.putCornerTopLeft()

	currentFirstTileOfLine := firstCorner

	for currentFirstTileOfLine != nil {
		seaMap = addNewLinesToMap(seaMap, currentFirstTileOfLine)

		currentTile := currentFirstTileOfLine

		for currentTile != nil {
			if !currentTile.Borders["right"].Associated {
				currentTile = nil
			} else {
				tileToAdd := t.getTile(currentTile.Borders["right"].Association.TileId)
				tileToAdd.positionTileToAddItAtTheRight(currentTile.Borders["right"].Association)
				seaMap = addContentAtTheRight(seaMap, tileToAdd)
				currentTile = tileToAdd
			}
		}

		if !currentFirstTileOfLine.Borders["bottom"].Associated {
			currentFirstTileOfLine = nil
		} else {
			tileToAdd := t.getTile(currentFirstTileOfLine.Borders["bottom"].Association.TileId)
			tileToAdd.positionTileToAddItAtTheBottom(currentFirstTileOfLine.Borders["bottom"].Association)
			currentFirstTileOfLine = tileToAdd
		}
	}

	return
}

func (t TileList) getCorner() *Tile {
	for _, tile := range t {
		if tile.getNumberOfSharedBorders() == 2 {
			return tile
		}
	}

	return &Tile{}
}

func addNewLinesToMap(seaMap []string, tile *Tile) []string {
	seaMap = append(seaMap, tile.Content...)
	return seaMap
}

func addContentAtTheRight(seaMap []string, tile *Tile) []string {
	seaMapStartIndex := len(seaMap) - len(tile.Content)

	for i := 0; i < len(tile.Content); i++ {
		seaMap[i+seaMapStartIndex] += tile.Content[i]
	}

	return seaMap
}

func (t TileList) getTile(id int) *Tile {
	for _, tile := range t {
		if tile.Id == id {
			return tile
		}
	}

	return nil
}

type Tile struct {
	Id      int
	Borders map[string]*Border
	Content []string
}

func (t *Tile) setContent(fullContent []string) {
	for i := 1; i < tileSize-1; i++ {
		line := fullContent[i][1 : tileSize-1]
		t.Content = append(t.Content, line)
	}
}

func (t *Tile) setBorders(fullContent []string) {
	t.Borders = map[string]*Border{}
	t.Borders["top"] = newBorder(fullContent[0])
	t.Borders["bottom"] = newBorder(fullContent[tileSize-1])
	t.Borders["left"] = newBorder(getColumn(fullContent, 0))
	t.Borders["right"] = newBorder(getColumn(fullContent, tileSize-1))
}

func getColumn(stringTable []string, index int) string {
	column := []byte{}

	for i := 0; i < len(stringTable); i++ {
		column = append(column, stringTable[i][index])
	}

	return string(column)
}

func (t *Tile) AssociateBorders(tile2 *Tile) {
	for tPosition, tBorder := range t.Borders {
		if tBorder.Associated {
			continue
		}

		for tile2Position, tile2Border := range tile2.Borders {
			if tBorder.Content == tile2Border.Content {
				tBorder.setAssociation(tile2.Id, tile2Position, false)
				tile2Border.setAssociation(t.Id, tPosition, false)
			} else if tBorder.Content == tile2Border.ReversedContent {
				tBorder.setAssociation(tile2.Id, tile2Position, true)
				tile2Border.setAssociation(t.Id, tPosition, true)
			}
		}
	}

}

func (t *Tile) getNumberOfSharedBorders() (number int) {
	for _, border := range t.Borders {
		if border.Associated {
			number++
		}
	}
	return
}

func (t *Tile) putCornerTopLeft() {
	if t.Borders["top"].Associated {
		t.applySymetryTopBottom()
	}

	if t.Borders["left"].Associated {
		t.applySymetryLeftRight()
	}
}

func (t *Tile) positionTileToAddItAtTheRight(association Association) {
	borderToPutLeft := association.BorderPosition

	if borderToPutLeft == "left" {
		if association.IsFlipped {
			t.applySymetryTopBottom()
		}
	} else if borderToPutLeft == "top" {
		t.applyCounterclockwiseRotation()

		if !association.IsFlipped {
			t.applySymetryTopBottom()
		}
	} else if borderToPutLeft == "right" {
		if association.IsFlipped {
			t.applyCentralSymetry()
		} else {
			t.applySymetryLeftRight()
		}
	} else {
		t.applyClockwiseRotation()

		if association.IsFlipped {
			t.applySymetryTopBottom()
		}
	}
}

func (t *Tile) positionTileToAddItAtTheBottom(association Association) {
	borderToPutTop := association.BorderPosition

	if borderToPutTop == "top" {
		if association.IsFlipped {
			t.applySymetryLeftRight()
		}
	} else if borderToPutTop == "right" {
		t.applyCounterclockwiseRotation()

		if association.IsFlipped {
			t.applySymetryLeftRight()
		}
	} else if borderToPutTop == "bottom" {
		if association.IsFlipped {
			t.applyCentralSymetry()
		} else {
			t.applySymetryTopBottom()
		}
	} else {
		t.applyClockwiseRotation()

		if !association.IsFlipped {
			t.applySymetryLeftRight()
		}
	}
}

func (t *Tile) applySymetryTopBottom() {
	t.Content = flipTopBottom(t.Content)
	t.applySymetryTopBottomToBorders()
}

func (t *Tile) applySymetryTopBottomToBorders() {
	temp := t.Borders["top"]
	t.Borders["top"] = t.Borders["bottom"]
	t.Borders["bottom"] = temp

	t.invertBorderFlip("left")
	t.invertBorderFlip("right")
}

func (t *Tile) applySymetryLeftRight() {
	for i, line := range t.Content {
		t.Content[i] = reverse(line)
	}
	t.applySymetryLeftRightToBorders()
}

func (t *Tile) applySymetryLeftRightToBorders() {
	temp := t.Borders["left"]
	t.Borders["left"] = t.Borders["right"]
	t.Borders["right"] = temp

	t.invertBorderFlip("top")
	t.invertBorderFlip("bottom")
}

func (t *Tile) applyClockwiseRotation() {
	t.Content = rotateClockwise(t.Content)
	t.applyClockwiseRotationToBorders()
}

func (t *Tile) applyClockwiseRotationToBorders() {
	tmp := t.Borders["left"]
	t.Borders["left"] = t.Borders["bottom"]
	t.Borders["bottom"] = t.Borders["right"]
	t.Borders["right"] = t.Borders["top"]
	t.Borders["top"] = tmp

	t.invertBorderFlip("top")
	t.invertBorderFlip("bottom")
}

func (t *Tile) applyCentralSymetry() {
	t.applyClockwiseRotation()
	t.applyClockwiseRotation()
}

func (t *Tile) applyCounterclockwiseRotation() {
	t.Content = rotateConterclockwise(t.Content)
	t.applyCounterclockwiseRotationToBorders()
}

func (t *Tile) applyCounterclockwiseRotationToBorders() {
	tmp := t.Borders["left"]
	t.Borders["left"] = t.Borders["top"]
	t.Borders["top"] = t.Borders["right"]
	t.Borders["right"] = t.Borders["bottom"]
	t.Borders["bottom"] = tmp

	t.invertBorderFlip("left")
	t.invertBorderFlip("right")
}

func (t *Tile) invertBorderFlip(position string) {
	if t.Borders[position].Associated {
		t.Borders[position].Association.IsFlipped = !t.Borders[position].Association.IsFlipped
	}
}

type Border struct {
	Content         string
	ReversedContent string
	Associated      bool
	Association     Association
}

func newBorder(content string) *Border {
	border := &Border{Content: content}
	border.ReversedContent = reverse(content)
	return border
}

func (b *Border) setAssociation(tileId int, position string, isFlipped bool) {
	b.Association = Association{
		TileId:         tileId,
		BorderPosition: position,
		IsFlipped:      isFlipped,
	}

	b.Associated = true
}

type Association struct {
	TileId         int
	BorderPosition string
	IsFlipped      bool
}

func reverse(input string) string {
	stringToReverse := []byte(input)
	reversedString := []byte{}

	for i := len(stringToReverse) - 1; i >= 0; i-- {
		reversedString = append(reversedString, stringToReverse[i])
	}

	return string(reversedString)
}

func rotateClockwise(input []string) []string {
	outputAsByte := getByteTable(len(input))

	for i := len(input) - 1; i >= 0; i-- {
		for j := 0; j < len(input); j++ {
			outputAsByte[j] = append(outputAsByte[j], input[i][j])
		}
	}

	return convertToStringSlice(outputAsByte)
}

func rotateConterclockwise(input []string) []string {
	outputAsByte := getByteTable(len(input))

	for i := 0; i < len(input); i++ {
		for j := 0; j < len(input); j++ {
			outputAsByte[j] = append(outputAsByte[j], input[i][len(input)-1-j])
		}
	}

	return convertToStringSlice(outputAsByte)
}

func getByteTable(numberOfLines int) [][]byte {
	byteTable := [][]byte{}

	for i := 0; i < numberOfLines; i++ {
		byteTable = append(byteTable, []byte{})
	}

	return byteTable
}

func convertToStringSlice(byteSlice [][]byte) (stringSlice []string) {
	for _, line := range byteSlice {
		stringSlice = append(stringSlice, string(line))
	}
	return
}

func flipTopBottom(input []string) (output []string) {
	for i := len(input) - 1; i >= 0; i-- {
		output = append(output, input[i])
	}
	return
}
