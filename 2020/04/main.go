package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"reflect"
	"regexp"
	"strconv"
	"strings"
)

type Passport struct {
	BirthYear      string
	IssueYear      string
	ExpirationYear string
	Height         string
	HairColor      string
	EyeColor       string
	PassportID     string
	CountryID      string
}

// I have done two IsValid methods because i want to keep getting the answer to the first part
func (p *Passport) IsValidPart1() bool {
	return p.BirthYear != "" &&
		p.IssueYear != "" &&
		p.ExpirationYear != "" &&
		p.Height != "" &&
		p.HairColor != "" &&
		p.EyeColor != "" &&
		p.PassportID != ""
}

func (p *Passport) IsValidPart2() bool {
	return p.isValidBirthYear() &&
		p.isValidIssueYear() &&
		p.isValidExpirationYear() &&
		p.isValidHeight() &&
		p.isValidHairColor() &&
		p.isValidEyeColor() &&
		p.isValidPassportID()
}

func (p *Passport) isValidBirthYear() bool {
	if p.BirthYear == "" {
		return false
	}

	value, err := strconv.Atoi(p.BirthYear)

	return err == nil && value >= 1920 && value <= 2002
}

func (p *Passport) isValidIssueYear() bool {
	if p.IssueYear == "" {
		return false
	}

	value, err := strconv.Atoi(p.IssueYear)

	return err == nil && value >= 2010 && value <= 2020
}

func (p *Passport) isValidExpirationYear() bool {
	if p.ExpirationYear == "" {
		return false
	}

	value, err := strconv.Atoi(p.ExpirationYear)

	return err == nil && value >= 2020 && value <= 2030
}

func (p *Passport) isValidHeight() bool {
	if p.Height == "" {
		return false
	}

	sizeAsString := p.Height[:len(p.Height)-2]
	size, err := strconv.Atoi(sizeAsString)

	if err != nil {
		return false
	}

	unit := p.Height[len(p.Height)-2:]

	if unit == "cm" {
		return size >= 150 && size <= 193
	} else {
		return size >= 59 && size <= 76
	}
}

func (p *Passport) isValidHairColor() bool {
	if p.HairColor == "" {
		return false
	}

	pattern := regexp.MustCompile(`^#[0-9a-f]{6}$`)

	return pattern.FindString(p.HairColor) != ""
}
func (p *Passport) isValidEyeColor() bool {
	if p.EyeColor == "" {
		return false
	}

	for _, color := range validEyeColor {
		if color == p.EyeColor {
			return true
		}
	}

	return false
}

func (p *Passport) isValidPassportID() bool {
	if p.PassportID == "" {
		return false
	}

	_, err := strconv.Atoi(p.PassportID)

	return err == nil && len(p.PassportID) == 9
}

func (p *Passport) Hydrate(data map[string]string) {
	for key, value := range data {
		reflect.ValueOf(p).Elem().FieldByName(correspondance[key]).SetString(value)
	}
}

var correspondance = map[string]string{
	"byr": "BirthYear",
	"iyr": "IssueYear",
	"eyr": "ExpirationYear",
	"hgt": "Height",
	"hcl": "HairColor",
	"ecl": "EyeColor",
	"pid": "PassportID",
	"cid": "CountryID",
}

var validEyeColor = []string{"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	passportList := extractPassportData(file)
	numberOfValidPassportForPart1 := 0
	numberOfValidPassportForPart2 := 0

	for _, passport := range passportList {
		if passport.IsValidPart1() {
			numberOfValidPassportForPart1++
		}

		if passport.IsValidPart2() {
			numberOfValidPassportForPart2++
		}
	}

	fmt.Printf("There are %d valid passport with rules for part 1\n", numberOfValidPassportForPart1)
	fmt.Printf("There are %d valid passport with rules for part 2\n", numberOfValidPassportForPart2)
}

func extractPassportData(rawData io.Reader) []Passport {
	passportList := []Passport{}
	scanner := bufio.NewScanner(rawData)

	passport := Passport{}

	for scanner.Scan() {
		line := scanner.Text()

		// Empty line means new passport
		// Non-empty line means new data on current passport
		if line == "" {
			passportList = append(passportList, passport)
			passport = Passport{}
		} else {
			extractedData := extractDataFromLine(scanner.Text())
			passport.Hydrate(extractedData)
		}
	}

	// Let's not forget the last passport
	passportList = append(passportList, passport)

	return passportList
}

func extractDataFromLine(line string) map[string]string {
	extractedData := map[string]string{}

	if line != "" {
		// Separate each element
		rawData := strings.Split(line, " ")

		// Extract components of each element
		for _, element := range rawData {
			pieces := strings.Split(element, ":")
			extractedData[pieces[0]] = pieces[1]
		}

	}

	return extractedData
}
