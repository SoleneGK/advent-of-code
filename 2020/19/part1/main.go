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

	answerPart1 := getAnswerPart1(file)
	fmt.Printf("There are %d messages that completely match rule 0\n", answerPart1)
}

func getAnswerPart1(data io.Reader) int {
	scanner := bufio.NewScanner(data)

	ruleList := readRules(scanner)
	ruleList.processRule(0)

	return ruleList.getNumberOfValidMessages(scanner)
}

func readRules(scanner *bufio.Scanner) RuleList {
	ruleList := RuleList{}

	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			break
		}

		ruleNumber, ruleContent := getRuleElementsFromString(line)

		ruleList[ruleNumber] = &Rule{RawData: ruleContent}
	}

	return ruleList
}

func getRuleElementsFromString(ruleString string) (ruleNumber int, ruleContent string) {
	elements := strings.Split(ruleString, ": ")
	ruleNumber = getInt(elements[0])
	ruleContent = elements[1]
	return
}

type RuleList map[int]*Rule

func (r RuleList) processRule(index int) {
	if !r[index].HasBeenProcessed {
		r[index].MatchingStrings = r.findMatchingStrings(r[index].RawData)
		r[index].HasBeenProcessed = true
	}
}

func (r RuleList) findMatchingStrings(rawData string) (matchingStrings []string) {
	if strings.Contains(rawData, `"`) {
		matchingStrings = []string{strings.Trim(rawData, `"`)}
	} else if strings.Contains(rawData, "|") {
		combinations := strings.Split(rawData, " | ")
		matchingStrings = r.combineRules(combinations[0])
		matchingStrings = append(matchingStrings, r.combineRules(combinations[1])...)

	} else {
		matchingStrings = r.combineRules(rawData)
	}

	return
}

func (r RuleList) combineRules(combinationInstruction string) []string {
	rulesIndexes := getIndexList(combinationInstruction)

	matchingStrings := [][]string{}

	for _, index := range rulesIndexes {
		r.processRule(index)
		matchingStrings = append(matchingStrings, r[index].MatchingStrings)
	}

	return combineMatchingStrings(matchingStrings)
}

func getIndexList(unprocessedList string) (indexList []int) {
	rulesIndexesAsStrings := strings.Split(unprocessedList, " ")

	for _, indexAsString := range rulesIndexesAsStrings {
		indexList = append(indexList, getInt(indexAsString))
	}

	return
}

func combineMatchingStrings(matchingStringsList [][]string) (combinedStrings []string) {
	if len(matchingStringsList) == 1 {
		return matchingStringsList[0]
	}

	secondPartStrings := combineMatchingStrings(matchingStringsList[1:])

	for _, firstPartString := range matchingStringsList[0] {
		for _, secondPartString := range secondPartStrings {
			combinedStrings = append(combinedStrings, firstPartString+secondPartString)
		}
	}

	return
}

func (r RuleList) getNumberOfValidMessages(scanner *bufio.Scanner) (number int) {
	for scanner.Scan() {
		if r[0].isValid(scanner.Text()) {
			number++
		}
	}

	return
}

type Rule struct {
	RawData          string
	MatchingStrings  []string
	HasBeenProcessed bool
}

func (r *Rule) isValid(message string) bool {
	for _, validString := range r.MatchingStrings {
		if message == validString {
			return true
		}
	}

	return false
}

func getInt(valueAsString string) int {
	value, _ := strconv.Atoi(valueAsString)
	return value
}
