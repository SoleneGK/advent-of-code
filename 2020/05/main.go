package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"os"
	"sort"
)

var (
	back  = []byte("B")[0]
	right = []byte("R")[0]
)

/* This problem is quite easy to solve once you see
 * that the letters are a binary code
 */
func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	highestID := getHighestSeatID(file)
	fmt.Printf("The highest seat ID is %d\n", highestID)

	// Can't read twice from the same reader, could use TeeReader
	file, err = os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	mySeatID := getMySeat(file)
	fmt.Printf("My seat ID is %d\n", mySeatID)
}

func extractRowAndColStrings(seatCode string) (row, col string) {
	return seatCode[:7], seatCode[7:]
}

func getRowNumber(rowString string) int {
	return decodeBinaryNumber(rowString, back)
}

func getColNumber(colString string) int {
	return decodeBinaryNumber(colString, right)
}

func decodeBinaryNumber(numberAsString string, codeForOne byte) (value int) {
	digitNumber := len(numberAsString) - 1

	for i := digitNumber; i >= 0; i-- {
		if numberAsString[i] == codeForOne {
			value += powInt(2, digitNumber-i)
		}
	}

	return
}

func powInt(x, y int) int {
	return int(math.Pow(float64(x), float64(y)))
}

func getSeatIDWithColAndRow(row, col int) int {
	return 8*row + col
}

func getSeatID(seatCode string) int {
	row, col := extractRowAndColStrings(seatCode)
	rowNumber := getRowNumber(row)
	colNumber := getColNumber(col)
	return getSeatIDWithColAndRow(rowNumber, colNumber)
}

func getHighestSeatID(seatList io.Reader) (maxID int) {
	scanner := bufio.NewScanner(seatList)

	for scanner.Scan() {
		id := getSeatID(scanner.Text())

		if id > maxID {
			maxID = id
		}
	}

	return
}

func getMySeat(seatList io.Reader) int {
	seatIDList := getSeatIDList(seatList)
	sort.Ints(seatIDList)

	numberOfID := len(seatIDList)

	for i := 0; i < numberOfID; i++ {
		if seatIDList[i+1]-seatIDList[i] == 2 {
			return seatIDList[i] + 1
		}
	}

	return 0
}

func getSeatIDList(seatList io.Reader) (seatIDList []int) {
	scanner := bufio.NewScanner(seatList)

	for scanner.Scan() {
		id := getSeatID(scanner.Text())
		seatIDList = append(seatIDList, id)
	}

	return
}
