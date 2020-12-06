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

	answerListPart1, answerListPart2 := getAllAnswerLists(file)
	totalPart1 := 0
	totalPart2 := 0

	for _, list := range answerListPart1 {
		totalPart1 += list.GetNumberOfYesAnswers()
	}

	for _, list := range answerListPart2 {
		totalPart2 += list.GetNumberOfYesAnswers()
	}

	fmt.Printf("The sum of counts for part 1 is %d\n", totalPart1)
	fmt.Printf("The sum of counts for part 2 is %d\n", totalPart2)

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

func getAllAnswerLists(input io.Reader) (answersPart1, answersPart2 []AnswerList) {
	currentGroupAnswerList := []AnswerList{}

	scanner := bufio.NewScanner(input)

	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			answersPart1, answersPart2 = addNewGroupAnswers(currentGroupAnswerList, answersPart1, answersPart2)
			currentGroupAnswerList = []AnswerList{}
		} else {
			passengerAnswers := getPassengerAnswers(line)
			currentGroupAnswerList = append(currentGroupAnswerList, passengerAnswers)
		}
	}

	answersPart1, answersPart2 = addNewGroupAnswers(currentGroupAnswerList, answersPart1, answersPart2)

	return
}

func addNewGroupAnswers(groupAnswersToAdd, answersPart1, answersPart2 []AnswerList) ([]AnswerList, []AnswerList) {
	groupAnswersPart1 := getGroupAnswersPart1(groupAnswersToAdd...)
	groupAnswersPart2 := getGroupAnswersPart2(groupAnswersToAdd...)

	answersPart1 = append(answersPart1, groupAnswersPart1)
	answersPart2 = append(answersPart2, groupAnswersPart2)

	return answersPart1, answersPart2
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

func getGroupAnswersPart2(passengerAnswers ...AnswerList) AnswerList {
	groupAnswer := getAnswerListAtTrue()

	for _, answers := range passengerAnswers {
		for i := range answers {
			groupAnswer[i] = groupAnswer[i] && answers[i]
		}
	}

	return groupAnswer
}

func getAnswerListAtTrue() (list AnswerList) {
	for i := range list {
		list[i] = true
	}
	return
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
