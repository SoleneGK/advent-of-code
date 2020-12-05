package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"os"
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
