package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	// File opening
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	/* I've found it quite simple this time
	 * For each line in the file, i extract the data with a regex
	 * Then i check if the requirements are met
	 */
	pattern := regexp.MustCompile(`^(\d+)-(\d+) ([a-z]): ([a-z]+)$`)
	numberOfCorrectPasswordsForPart1 := 0
	numberOfCorrectPasswordsForPart2 := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		rawData := scanner.Text()
		firstValue, secondValue, char, password := extractData(pattern, rawData)

		if isValidPasswordWithPart1Rules(firstValue, secondValue, char, password) {
			numberOfCorrectPasswordsForPart1++
		}

		if isValidPasswordWithPart2Rules(firstValue, secondValue, char, password) {
			numberOfCorrectPasswordsForPart2++
		}
	}

	fmt.Printf("The data contains %d valid passwords according to part 1 rules\n", numberOfCorrectPasswordsForPart1)
	fmt.Printf("The data contains %d valid passwords according to part 2 rules\n", numberOfCorrectPasswordsForPart2)
}

// Extract data from the string
func extractData(pattern *regexp.Regexp, rawData string) (int1, int2 int, string1, string2 string) {
	extraction := pattern.FindStringSubmatch(rawData)

	int1 = getInt(extraction[1])
	int2 = getInt(extraction[2])

	return int1, int2, extraction[3], extraction[4]
}

func getInt(input string) int {
	value, _ := strconv.Atoi(input)
	return value
}

// It verifies if the number on wanted character is between min and max
func isValidPasswordWithPart1Rules(min, max int, char, password string) bool {
	occurenceNumber := strings.Count(password, char)
	return occurenceNumber >= min && occurenceNumber <= max
}

// It verifies if the char is on first xor second position
func isValidPasswordWithPart2Rules(firstPosition, secondPosition int, char, password string) bool {
	isCharInFirstPosition := isCharAtPosition(char, password, firstPosition-1)
	isCharInSecondPosition := isCharAtPosition(char, password, secondPosition-1)

	return (isCharInFirstPosition || isCharInSecondPosition) && !(isCharInFirstPosition && isCharInSecondPosition)
}

// First, verifying if the password string is long enough
// In not, the character cannot be in this position
// Else, simple comparison
func isCharAtPosition(char, password string, position int) bool {
	if position >= len(password) {
		return false
	}

	charInPassword := string(password[position])
	return char == charInPassword
}
