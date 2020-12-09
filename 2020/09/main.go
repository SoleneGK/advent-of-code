package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"sort"
	"strconv"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	xmas := XmasData{}
	xmas.AddInput(file, 25)

	answerPart1 := xmas.GetFirstInvalidValue()
	fmt.Printf("The first invalid value is %d\n", answerPart1)

	answerPart2 := xmas.FindContiguousNumbersWithSum()
	fmt.Printf("The encryption weakness is %d\n", answerPart2)
}

type XmasData struct {
	Preamble        []int
	Data            []int
	previousNumbers []int
	completeData    []int
}

func (x *XmasData) AddInput(input io.Reader, preambleLength int) {
	inputAsIntSlice := x.convertInput(input)
	x.Preamble = inputAsIntSlice[:preambleLength]
	x.Data = inputAsIntSlice[preambleLength:]
}

func (x *XmasData) convertInput(input io.Reader) (convertedInput []int) {
	scanner := bufio.NewScanner(input)

	for scanner.Scan() {
		number, _ := strconv.Atoi(scanner.Text())
		convertedInput = append(convertedInput, number)
	}

	return
}

// I use the algo from day 1
func (x *XmasData) IsValid(index int) bool {
	for i := 0; i < len(x.previousNumbers)-1; i++ {
		for j := i + 1; j < len(x.previousNumbers); j++ {
			if x.previousNumbers[i]+x.previousNumbers[j] == x.Data[index] {
				return true
			}
		}
	}
	return false
}

func (x *XmasData) GetFirstInvalidValue() int {
	x.previousNumbers = x.Preamble

	for i := 0; i < len(x.Data); i++ {
		if !x.IsValid(i) {
			return x.Data[i]
		} else {
			x.previousNumbers = append(x.previousNumbers[1:], x.Data[i])
		}
	}

	return 0
}

func (x *XmasData) FindContiguousNumbersWithSum() int {
	x.completeData = append(x.Preamble, x.Data...)
	searchedValue := x.GetFirstInvalidValue()
	var currentList []int
	var total int
	i := 0

	for total != searchedValue && i < len(x.completeData)-1 {
		currentList = []int{x.completeData[i]}
		total = x.completeData[i]
		j := i + 1

		for total < searchedValue && j < len(x.completeData) {
			currentList = append(currentList, x.completeData[j])
			total += x.completeData[j]
			j++
		}

		i++
	}

	sort.Ints(currentList)
	return currentList[0] + currentList[len(currentList)-1]
}
