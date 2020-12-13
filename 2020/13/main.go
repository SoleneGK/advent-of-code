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

	myTimestamp, busTimetable := ParseFile(file)
	busLine, waitingTime := busTimetable.GetFirstBus(myTimestamp)
	fmt.Printf("The first bus to come is line n°%d and you wait it for %d minutes. The answer is then %d.\n", busLine, waitingTime, busLine*waitingTime)

}

func ParseFile(file io.Reader) (myTimestamp int, busTimetable BusTimetable) {
	scanner := bufio.NewScanner(file)

	scanner.Scan()
	myTimestamp, _ = strconv.Atoi(scanner.Text())

	scanner.Scan()
	busTimetable.SetBusLines(scanner.Text())

	return
}

type BusTimetable struct {
	Lines []int
}

func (b *BusTimetable) SetBusLines(rawData string) {
	linesList := strings.Split(rawData, ",")

	for _, line := range linesList {
		if line != "x" {
			lineNumber, _ := strconv.Atoi(line)
			b.Lines = append(b.Lines, lineNumber)
		}
	}
}

func (b *BusTimetable) GetFirstBus(timestamp int) (busLine, waitingTime int) {
	timeBeforeNextPassage := map[int]int{}

	for _, lineNumber := range b.Lines {
		timeBeforeNextPassage[lineNumber] = getTimeBeforeNextPassageOfLine(lineNumber, timestamp)
	}

	nextBus := getNextBus(timeBeforeNextPassage)

	return nextBus, timeBeforeNextPassage[nextBus]
}

func getTimeBeforeNextPassageOfLine(lineNumber, timestamp int) int {
	return (lineNumber - timestamp%lineNumber) % lineNumber
}

func getNextBus(waitingTimes map[int]int) int {
	fastestLine := 0

	for lineNumber, timeToWait := range waitingTimes {
		if fastestLine == 0 || timeToWait < waitingTimes[fastestLine] {
			fastestLine = lineNumber
		}
	}

	return fastestLine
}
