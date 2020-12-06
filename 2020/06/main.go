package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	answersLists := getAllAnswerLists(file)
	total := 0

	for _, list := range answersLists {
		total += getNumberOfYesAnswers(list)
	}

	fmt.Printf("The sum of counts is %d\n", total)

}

const numberOnQuestions = 26

var unicodeValueOfa = int([]rune("a")[0])

type AnswerList [numberOnQuestions]bool

func (a *AnswerList) AddAnswers(answers string) {
	for i := 0; i < len(answers); i++ {
		unicodeValueOfChar := a.getUnicodeValueOfChar(answers[i])
		index := unicodeValueOfChar - unicodeValueOfa
		a[index] = true
	}
}

func (a *AnswerList) getUnicodeValueOfChar(char byte) int {
	return int(rune(char))
}

func getNumberOfYesAnswers(answers [numberOnQuestions]bool) (number int) {
	for i := 0; i < numberOnQuestions; i++ {
		if answers[i] {
			number++
		}
	}

	return
}

func getAllAnswerLists(input io.Reader) []AnswerList {
	allAnswerLists := []AnswerList{}
	currentAnswerList := AnswerList{}

	scanner := bufio.NewScanner(input)

	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			allAnswerLists = append(allAnswerLists, currentAnswerList)
			currentAnswerList = AnswerList{}
		} else {
			currentAnswerList.AddAnswers(line)
		}
	}

	allAnswerLists = append(allAnswerLists, currentAnswerList)

	return allAnswerLists
}
