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

	xmas := XmasData{}
	xmas.AddInput(file, 25)

	answerPart1 := xmas.GetFirstInvalidValue()
	fmt.Printf("The first invalid value is %d\n", answerPart1)
}

type XmasData struct {
	Preamble        []int
	Data            []int
	previousNumbers []int
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
