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

	answerPart2 := getAnswerPart2(file)
	fmt.Printf("The answer for part 2 is %d\n", answerPart2)
}

func getAnswerPart2(file io.Reader) int {
	fieldRules, myTicket, nearTickets := extractData(file)

	validTickets := getValidTickets(nearTickets, fieldRules)
	fieldsByIndex := associateFieldsWithIndexes(fieldRules, validTickets)

	return calculateAnswerPart2(fieldsByIndex, myTicket)
}

func extractData(reader io.Reader) (fieldRules map[string]Bounds, myTicket []int, nearTickets [][]int) {
	scanner := bufio.NewScanner(reader)

	fieldRules = getFieldRules(scanner)
	skipLine(scanner)
	myTicket = getMyTicket(scanner)
	skipLine(scanner)
	skipLine(scanner)
	nearTickets = getNearTickets(scanner)

	return
}

func getFieldRules(scanner *bufio.Scanner) map[string]Bounds {
	fieldRules := map[string]Bounds{}

	pattern := regexp.MustCompile(`^([a-z ]+): (\d{1,3})-(\d{1,3}) or (\d{1,3})-(\d{1,3})$`)

	for scanner.Scan() {
		line := scanner.Text()

		if len(line) == 0 {
			break
		}

		data := pattern.FindStringSubmatch(line)
		fieldRules[data[1]] = Bounds{
			Min1: getInt(data[2]),
			Max1: getInt(data[3]),
			Min2: getInt(data[4]),
			Max2: getInt(data[5]),
		}
	}

	return fieldRules
}

func getInt(intAsString string) int {
	value, _ := strconv.Atoi(intAsString)
	return value
}

func skipLine(scanner *bufio.Scanner) {
	scanner.Scan()
}

func getMyTicket(scanner *bufio.Scanner) []int {
	scanner.Scan()
	return getTicketValues(scanner.Text())
}

func getNearTickets(scanner *bufio.Scanner) (nearTickets [][]int) {
	for scanner.Scan() {
		nearTickets = append(nearTickets, getTicketValues(scanner.Text()))
	}
	return
}

func getTicketValues(ticket string) (ticketValues []int) {
	dataAsString := strings.Split(ticket, ",")

	for _, data := range dataAsString {
		value, _ := strconv.Atoi(data)
		ticketValues = append(ticketValues, value)
	}

	return
}

func getValidTickets(ticketList [][]int, fieldRules map[string]Bounds) (validTickets [][]int) {
	for _, ticketToCheck := range ticketList {
		if isValidTicket(ticketToCheck, fieldRules) {
			validTickets = append(validTickets, ticketToCheck)
		}
	}
	return
}

func isValidTicket(ticket []int, fieldRules map[string]Bounds) bool {
	for _, value := range ticket {
		if !isValidValueForAnyField(value, fieldRules) {
			return false
		}
	}
	return true
}

func isValidValueForAnyField(value int, fieldRules map[string]Bounds) bool {
	for _, bounds := range fieldRules {
		if (value >= bounds.Min1 && value <= bounds.Max1) ||
			(value >= bounds.Min2 && value <= bounds.Max2) {
			return true
		}
	}
	return false
}

func associateFieldsWithIndexes(fieldRules map[string]Bounds, ticketList [][]int) map[string]int {
	fieldsByIndex := map[string]int{}
	fieldsToAttribute := getFieldList(fieldRules)
	indexesToAttribute := getIndexList(fieldRules)

	possibleCombinations := findPossibleCombinations(fieldsToAttribute, indexesToAttribute, fieldRules, ticketList)

	for len(indexesToAttribute) > 0 {
		validatedCombinations := getValidatedCombinations(possibleCombinations, fieldsToAttribute, indexesToAttribute)

		for field, index := range validatedCombinations {
			fieldsByIndex[field] = index

			fieldsToAttribute = removeField(fieldsToAttribute, field)
			indexesToAttribute = removeIndex(indexesToAttribute, index)
		}
	}

	return fieldsByIndex
}

func getFieldList(fieldRules map[string]Bounds) (fieldList []string) {
	for field := range fieldRules {
		fieldList = append(fieldList, field)
	}
	return
}

func getIndexList(fieldRules map[string]Bounds) (indexList []int) {
	for i := 0; i < len(fieldRules); i++ {
		indexList = append(indexList, i)
	}
	return
}

func findPossibleCombinations(fieldsToAttribute []string, indexesToAttribute []int, fieldRules map[string]Bounds, ticketList [][]int) map[string]map[int]bool {
	possibleCombinations := initializeCombinationMatrix(fieldsToAttribute, indexesToAttribute)

	for _, ticket := range ticketList {
		for _, field := range fieldsToAttribute {
			for _, index := range indexesToAttribute {
				possibleCombinations[field][index] = possibleCombinations[field][index] && isValidFieldForIndex(ticket, index, fieldRules[field])
			}
		}
	}

	return possibleCombinations
}

func initializeCombinationMatrix(fieldList []string, indexList []int) map[string]map[int]bool {
	matrix := map[string]map[int]bool{}

	for _, field := range fieldList {
		matrix[field] = map[int]bool{}

		for _, index := range indexList {
			matrix[field][index] = true
		}
	}

	return matrix
}

func isValidFieldForIndex(ticket []int, index int, bounds Bounds) bool {
	value := ticket[index]

	return (value >= bounds.Min1 && value <= bounds.Max1) ||
		(value >= bounds.Min2 && value <= bounds.Max2)
}

func getValidatedCombinations(possibleCombinations map[string]map[int]bool, fieldsToAttribute []string, indexesToAttribute []int) map[string]int {
	validatedCombinations := map[string]int{}

	for _, field := range fieldsToAttribute {
		numberOfPossibleIndexes, index := analyzePossibleCombinationData(possibleCombinations[field], indexesToAttribute)

		if numberOfPossibleIndexes == 1 {
			validatedCombinations[field] = index

			removeField(fieldsToAttribute, field)
			removeIndex(indexesToAttribute, index)
		}
	}

	return validatedCombinations
}

func analyzePossibleCombinationData(possibleCombinations map[int]bool, indexesToAttribute []int) (numberOfPossibleIndexes int, possibleIndex int) {
	for _, index := range indexesToAttribute {
		if possibleCombinations[index] {
			possibleIndex = index
			numberOfPossibleIndexes++
		}
	}
	return
}

func removeField(fieldSlice []string, field string) (newFieldSlice []string) {
	for _, value := range fieldSlice {
		if value != field {
			newFieldSlice = append(newFieldSlice, value)
		}
	}
	return
}

func removeIndex(indexList []int, index int) (newIndexList []int) {
	for _, value := range indexList {
		if value != index {
			newIndexList = append(newIndexList, value)
		}
	}
	return
}

func calculateAnswerPart2(fieldsByIndex map[string]int, myTicket []int) int {
	return myTicket[fieldsByIndex["departure location"]] *
		myTicket[fieldsByIndex["departure station"]] *
		myTicket[fieldsByIndex["departure platform"]] *
		myTicket[fieldsByIndex["departure track"]] *
		myTicket[fieldsByIndex["departure date"]] *
		myTicket[fieldsByIndex["departure time"]]
}

type Bounds struct {
	Min1 int
	Max1 int
	Min2 int
	Max2 int
}
