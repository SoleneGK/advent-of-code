package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	answer := getAnswer(file)
	fmt.Printf("There are %d messages that completely match rule 0\n", answer)
}

func getAnswer(data io.Reader) int {
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

func getInt(valueAsString string) int {
	value, _ := strconv.Atoi(valueAsString)
	return value
}

type RuleList map[int]*Rule

func (r RuleList) processRule(index int) {
	// Let's treat rules 8 and 11 aside
	// New rule 8: 42 | 42 8
	if index == 8 {
		r.processRule(42)
		r[8].Regexp = fmt.Sprintf("(%s)+", r[42].Regexp)
		r[8].HasBeenProcessed = true
		return
	}

	// New rule 11: 42 31 | 42 11 31
	if index == 11 {
		r.processRule(42)
		r.processRule(31)
		r[11].Regexp = fmt.Sprintf("%s(%s(%s(%s(%s%s)?%s)?%s)?%s)?%s",
			r[42].Regexp,
			r[42].Regexp,
			r[42].Regexp,
			r[42].Regexp,
			r[42].Regexp,
			r[31].Regexp,
			r[31].Regexp,
			r[31].Regexp,
			r[31].Regexp,
			r[31].Regexp,
		)

		r[11].HasBeenProcessed = true
		return
	}

	rule := r[index]

	if !rule.HasBeenProcessed {
		rule.Regexp = r.findRegexp(rule.RawData)
		rule.HasBeenProcessed = true
	}
}

func (r RuleList) findRegexp(rawData string) string {
	if rawData == `"a"` {
		return "a"
	} else if rawData == `"b"` {
		return "b"
	} else if strings.Contains(rawData, "|") {
		combinations := strings.Split(rawData, " | ")
		return fmt.Sprintf("(%s|%s)", r.concatenateRules(combinations[0]), r.concatenateRules(combinations[1]))
	} else {
		return r.concatenateRules(rawData)
	}
}

func (r RuleList) concatenateRules(combinationInstruction string) (concatenatedRules string) {
	rulesIndexes := getIndexList(combinationInstruction)

	for _, index := range rulesIndexes {
		r.processRule(index)
		concatenatedRules += r[index].Regexp
	}

	return
}

func getIndexList(unprocessedList string) (indexList []int) {
	ruleIndexesAsStrings := strings.Split(unprocessedList, " ")

	for _, indexAsString := range ruleIndexesAsStrings {
		indexList = append(indexList, getInt(indexAsString))
	}

	return
}

func (r RuleList) getNumberOfValidMessages(scanner *bufio.Scanner) (number int) {
	pattern := regexp.MustCompile("^" + r[0].Regexp + "$")

	for scanner.Scan() {
		if pattern.MatchString(scanner.Text()) {
			number++
		}
	}

	return
}

type Rule struct {
	RawData          string
	Regexp           string
	HasBeenProcessed bool
}
