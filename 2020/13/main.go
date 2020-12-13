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

	answerPart2 := busTimetable.GetAnswerPart2()
	fmt.Printf("The answer to part 2 is %d\n", answerPart2)
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
	Lines          []int
	LinesWithIndex map[int]int
}

func (b *BusTimetable) SetBusLines(rawData string) {
	b.LinesWithIndex = map[int]int{}
	linesList := strings.Split(rawData, ",")

	for index, line := range linesList {
		if line != "x" {
			lineNumber, _ := strconv.Atoi(line)
			b.Lines = append(b.Lines, lineNumber)
			b.LinesWithIndex[lineNumber] = index
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

func (b *BusTimetable) GetAnswerPart2() int {
	offsets := b.getOffsets()

	currentStep := b.Lines[0]
	currentOffset := b.LinesWithIndex[currentStep]

	for _, lineNumber := range b.Lines[1:] {
		fmt.Printf("working on line n°%d, offset %d\n", lineNumber, offsets[lineNumber])
		if currentStep > lineNumber {
			currentStep, currentOffset = addNewConstraint(currentStep, currentOffset, lineNumber, offsets[lineNumber])
		} else {
			currentStep, currentOffset = addNewConstraint(lineNumber, offsets[lineNumber], currentStep, currentOffset)
		}
	}

	return currentOffset
}

func (b *BusTimetable) getOffsets() map[int]int {
	offsets := map[int]int{}

	for _, lineNumber := range b.Lines {
		offsets[lineNumber] = lineNumber - (b.LinesWithIndex[lineNumber] % lineNumber)
	}

	return offsets
}

// Required: step1 > step2
func addNewConstraint(step1, offset1, step2, offset2 int) (newStep, newOffset int) {
	i := 0

	for {
		i++

		bigValue := step1*i + offset1
		smallValue := ((bigValue-offset2)/step2)*step2 + offset2

		if bigValue == smallValue {
			break
		}
	}

	return step1 * step2, step1*i + offset1
}
