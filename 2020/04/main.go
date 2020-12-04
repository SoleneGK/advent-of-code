package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"reflect"
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

func (p *Passport) IsValidPart1() bool {
	return p.BirthYear != "" && p.IssueYear != "" && p.ExpirationYear != "" && p.Height != "" && p.HairColor != "" && p.EyeColor != "" && p.PassportID != ""
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

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	passportList := extractPassportData(file)
	numberOfValidPassport := 0

	for _, passport := range passportList {
		if passport.IsValidPart1() {
			numberOfValidPassport++
		}
	}

	fmt.Printf("There are %d valid passport\n", numberOfValidPassport)
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
