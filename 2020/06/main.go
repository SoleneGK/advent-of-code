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
		total += list.GetNumberOfYesAnswers()
	}

	fmt.Printf("The sum of counts is %d\n", total)

}

const numberOnQuestions = 26

type AnswerList [numberOnQuestions]bool

func (a *AnswerList) GetNumberOfYesAnswers() (number int) {
	for i := 0; i < numberOnQuestions; i++ {
		if a[i] {
			number++
		}
	}

	return
}

// refaire
func getAllAnswerLists(input io.Reader) []AnswerList {
	allAnswerLists := []AnswerList{}
	currentGroupAnswerList := []AnswerList{}

	scanner := bufio.NewScanner(input)

	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			groupAnswers := getGroupAnswersPart1(currentGroupAnswerList...)
			allAnswerLists = append(allAnswerLists, groupAnswers)
			currentGroupAnswerList = []AnswerList{}
		} else {
			passengerAnswers := getPassengerAnswers(line)
			currentGroupAnswerList = append(currentGroupAnswerList, passengerAnswers)
		}
	}

	groupAnswers := getGroupAnswersPart1(currentGroupAnswerList...)
	allAnswerLists = append(allAnswerLists, groupAnswers)

	return allAnswerLists
}

func getGroupAnswersPart1(passengerAnswers ...AnswerList) AnswerList {
	groupAnswers := AnswerList{}

	for _, answers := range passengerAnswers {
		for i := range answers {
			groupAnswers[i] = groupAnswers[i] || answers[i]
		}
	}

	return groupAnswers
}

func getPassengerAnswers(input string) (answer AnswerList) {
	for i := 0; i < len(input); i++ {
		unicodeValueOfChar := getUnicodeValueOfChar(input[i])
		index := unicodeValueOfChar - unicodeValueOfa
		answer[index] = true
	}
	return
}

var unicodeValueOfa = int([]rune("a")[0])

func getUnicodeValueOfChar(char byte) int {
	return int(rune(char))
}
