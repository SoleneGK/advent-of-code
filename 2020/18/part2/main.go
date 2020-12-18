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

	answerPart2 := getAnswerPart2(file)
	fmt.Printf("The sum is %d\n", answerPart2)
}

func getAnswerPart2(reader io.Reader) (sum int) {
	scanner := bufio.NewScanner(reader)

	for scanner.Scan() {
		sum += calculate(scanner.Text())
	}

	return
}

var (
	openingParenthesis = []byte("(")[0]
	closingParenthesis = []byte(")")[0]
)

func calculate(input string) int {
	// First, are there parenthesis ?
	if strings.Contains(input, string(openingParenthesis)) {
		input = calculateExpressionInParenthesis(input)
	}

	additionList := strings.Split(input, " * ")
	result := 1

	for _, additionElement := range additionList {
		values := strings.Split(additionElement, " + ")
		sum := 0

		for _, value := range values {
			sum += getInt(value)
		}

		result *= sum
	}

	return result
}

func getInt(valueAsString string) (value int) {
	value, _ = strconv.Atoi(valueAsString)
	return
}

func calculateExpressionInParenthesis(input string) string {
	finalInput := ""

	openingParenthesisIndex := strings.Index(input, string(openingParenthesis))

	for openingParenthesisIndex != -1 {
		// Get data before parenthesis
		finalInput += input[:openingParenthesisIndex]

		closingParenthesisIndex := getClosingParenthesisIndex(input[openingParenthesisIndex+1:], openingParenthesisIndex)

		valueOfParenthesis := calculate(input[openingParenthesisIndex+1 : closingParenthesisIndex])
		finalInput += fmt.Sprint(valueOfParenthesis)

		// Remove what have been used
		input = input[closingParenthesisIndex+1:]

		openingParenthesisIndex = strings.Index(input, string(openingParenthesis))
	}

	finalInput += input

	return finalInput
}

func getClosingParenthesisIndex(input string, openingParenthesisIndex int) int {
	numberOfOpenParenthesis := 1

	for i := 0; i < len(input); i++ {
		character := input[i]

		if character == openingParenthesis {
			numberOfOpenParenthesis++
		} else if character == closingParenthesis {
			numberOfOpenParenthesis--
		}

		if numberOfOpenParenthesis == 0 {
			return i + openingParenthesisIndex + 1
		}
	}

	return -1
}
