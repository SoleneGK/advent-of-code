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

	answerPart1 := getAnswerPart1(file)
	fmt.Printf("The ticket scanning error rate is %d\n", answerPart1)
}

func getAnswerPart1(file io.Reader) int {
	scanner := bufio.NewScanner(file)

	fieldValidation := getFieldValidation(scanner)

	skipYourTicketLines(scanner)

	ticketScanningErrorRate := getTicketScanningErrorRate(scanner, fieldValidation)

	return ticketScanningErrorRate
}

func getFieldValidation(scanner *bufio.Scanner) map[string]Bounds {
	fieldValidation := map[string]Bounds{}

	pattern := regexp.MustCompile(`^([a-z ]+): (\d{1,3})-(\d{1,3}) or (\d{1,3})-(\d{1,3})$`)

	for scanner.Scan() {
		line := scanner.Text()

		if len(line) == 0 {
			break
		}

		data := pattern.FindStringSubmatch(line)
		fieldValidation[data[1]] = Bounds{
			Min1: getInt(data[2]),
			Max1: getInt(data[3]),
			Min2: getInt(data[4]),
			Max2: getInt(data[5]),
		}
	}

	return fieldValidation
}

func getInt(intAsString string) int {
	value, _ := strconv.Atoi(intAsString)
	return value
}

func skipYourTicketLines(scanner *bufio.Scanner) {
	for scanner.Scan() {
		if scanner.Text() == "" {
			break
		}
	}
}

func getTicketScanningErrorRate(scanner *bufio.Scanner, fieldValidation map[string]Bounds) int {
	// remove "near tickets" line
	scanner.Scan()

	ticketScanningErrorRate := 0

	for scanner.Scan() {
		ticketScanningErrorRate += getTicketErrorRate(scanner.Text(), fieldValidation)
	}

	return ticketScanningErrorRate
}

func getTicketErrorRate(ticket string, fieldValidation map[string]Bounds) int {
	ticketFields := getTicketValues(ticket)

	errorRate := 0

	for _, fieldValue := range ticketFields {
		if !isValid(fieldValue, fieldValidation) {
			errorRate += fieldValue
		}
	}

	return errorRate
}

func getTicketValues(ticket string) (ticketValues []int) {
	dataAsString := strings.Split(ticket, ",")

	for _, data := range dataAsString {
		value, _ := strconv.Atoi(data)
		ticketValues = append(ticketValues, value)
	}

	return
}

func isValid(value int, fieldValidation map[string]Bounds) bool {
	for _, bounds := range fieldValidation {
		if (value >= bounds.Min1 && value <= bounds.Max1) || (value >= bounds.Min2 && value <= bounds.Max2) {
			return true
		}
	}

	return false
}

type Bounds struct {
	Min1 int
	Max1 int
	Min2 int
	Max2 int
}
