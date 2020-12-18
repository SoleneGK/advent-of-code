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

	answerPart1 := getAnswerPart1(file)
	fmt.Printf("The sum is %d\n", answerPart1)
}

func getAnswerPart1(reader io.Reader) (sum int) {
	scanner := bufio.NewScanner(reader)

	for scanner.Scan() {
		sum += calculate(scanner.Text())
	}

	return
}

var (
	plus               = []byte("+")[0]
	closingParenthesis = []byte(")")[0]
	openingParenthesis = []byte("(")[0]
)

func calculate(input string) (result int) {
	// The last element is either a number or an expression between parenthesis
	result, lastElementLength := getLastElement(input)

	// Don't forget spaces !
	pointer := len(input) - 2 - lastElementLength

	// If there is more to parse
	// Next last element is either + or *
	// Followed by a space to remove
	if pointer > 0 {
		if input[pointer] == plus {
			result += calculate(input[:pointer-1])
		} else {
			result *= calculate(input[:pointer-1])
		}
	}

	return
}

func getLastElement(input string) (result, lastElementLength int) {
	pointer := len(input) - 1

	if input[pointer] == closingParenthesis {
		subInput := getParenthesisContent(input)
		return calculate(subInput), len(subInput) + 2
	} else {
		subInput := getLastNumber(input)
		return getInt(subInput), len(subInput)
	}
}

func getInt(valueAsString string) (value int) {
	value, _ = strconv.Atoi(valueAsString)
	return
}

func getParenthesisContent(input string) string {
	inputToProcess := input[:len(input)-1]
	pointer := len(inputToProcess) - 1
	subString := []byte{}
	numberOfOpenParenthesis := 1

	for pointer > 0 {
		character := inputToProcess[pointer]

		if character == closingParenthesis {
			numberOfOpenParenthesis++
		} else if character == openingParenthesis {
			numberOfOpenParenthesis--
		}

		if numberOfOpenParenthesis == 0 {
			break
		}

		subString = append([]byte{character}, subString...)
		pointer--
	}

	return string(subString)
}

func getLastNumber(input string) string {
	pointer := len(input) - 1
	lastNumber := []byte{}

	for pointer >= 0 {
		if string(input[pointer]) == " " {
			break
		}

		lastNumber = append([]byte{input[pointer]}, lastNumber...)
		pointer--
	}

	return string(lastNumber)
}
