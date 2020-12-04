package main

import (
	"reflect"
	"strings"
	"testing"
)

func TestIsValidPart1(t *testing.T) {
	testTable := []struct {
		name     string
		passport Passport
		want     bool
	}{
		{
			name: "Valid passport",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "#7092be",
				PassportID:     "854123",
			},
			want: true,
		},
		{
			name: "Missing BirthYear",
			passport: Passport{
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "#7092be",
				PassportID:     "854123",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Missing IssueYear",
			passport: Passport{
				BirthYear:      "1989",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "#7092be",
				PassportID:     "854123",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Missing ExpirationYear",
			passport: Passport{
				BirthYear:  "1989",
				IssueYear:  "2020",
				Height:     "164cm",
				HairColor:  "#ed1c24",
				EyeColor:   "#7092be",
				PassportID: "854123",
				CountryID:  "98",
			},
			want: false,
		},
		{
			name: "Missing Height",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				HairColor:      "#ed1c24",
				EyeColor:       "#7092be",
				PassportID:     "854123",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Missing HairColor",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				EyeColor:       "#7092be",
				PassportID:     "854123",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Missing EyeColor",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				PassportID:     "854123",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Missing PassportID",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "#7092be",
				CountryID:      "98",
			},
			want: false,
		},
	}

	for _, tt := range testTable {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.passport.IsValidPart1()

			if got != tt.want {
				t.Errorf("Incorrect passport validity: got %t, want %t", got, tt.want)
			}
		})
	}
}

func TestExtractDataFromLine(t *testing.T) {
	testTable := []struct {
		name       string
		lineToRead string
		want       map[string]string
	}{
		{
			name:       "Empty line",
			lineToRead: "",
			want:       map[string]string{},
		},
		{
			name:       "One key",
			lineToRead: "cid:98",
			want: map[string]string{
				"cid": "98",
			},
		},
		{
			name:       "Several keys",
			lineToRead: "hgt:164cm eyr:2019 hcl:#fffffd",
			want: map[string]string{
				"hgt": "164cm",
				"eyr": "2019",
				"hcl": "#fffffd",
			},
		},
	}

	for _, tt := range testTable {
		t.Run(tt.name, func(t *testing.T) {
			got := extractDataFromLine(tt.lineToRead)

			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Incorrect extracted data: got %v, want %v", got, tt.want)
			}
		})
	}
}

func TestHydratePassport(t *testing.T) {
	passport := Passport{}
	data := map[string]string{
		"byr": "1989",
		"iyr": "2020",
		"eyr": "2025",
		"hgt": "164cm",
		"hcl": "#ed1c24",
		"ecl": "#7092be",
		"pid": "854123",
		"cid": "98",
	}

	passport.Hydrate(data)
	want := Passport{
		BirthYear:      "1989",
		IssueYear:      "2020",
		ExpirationYear: "2025",
		Height:         "164cm",
		HairColor:      "#ed1c24",
		EyeColor:       "#7092be",
		PassportID:     "854123",
		CountryID:      "98",
	}

	if !reflect.DeepEqual(passport, want) {
		t.Errorf("Incorrect passport hydration: got %v, want %v", passport, want)
	}
}

func TestExtractPassportData(t *testing.T) {
	testTable := []struct {
		name    string
		rawData string
		want    []Passport
	}{
		{
			name:    "One passport on one line",
			rawData: "eyr:2024",
			want: []Passport{
				Passport{ExpirationYear: "2024"},
			},
		},
		{
			name:    "One passport on several lines",
			rawData: "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm",
			want: []Passport{
				Passport{
					BirthYear:      "1931",
					IssueYear:      "2013",
					ExpirationYear: "2024",
					Height:         "179cm",
					HairColor:      "#ae17e1",
					EyeColor:       "brn",
					PassportID:     "760753108",
				},
			},
		},
		{
			name:    "Several passports",
			rawData: "hcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024",
			want: []Passport{
				Passport{
					HairColor: "#cfa07d",
					BirthYear: "1929",
				},
				Passport{
					HairColor:      "#ae17e1",
					IssueYear:      "2013",
					ExpirationYear: "2024",
				},
			},
		},
	}

	for _, tt := range testTable {
		t.Run(tt.name, func(t *testing.T) {
			got := extractPassportData(strings.NewReader(tt.rawData))

			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Incorrect passport: got %v want %v\n", got, tt.want)
			}
		})
	}
}

// This test is a copy-past of TestIsValidPart1
// I know, DRY, but it's for formal practice
func TestIsValidPart2(t *testing.T) {
	testTable := []struct {
		name     string
		passport Passport
		want     bool
	}{
		{
			name: "Valid passport with size in cm",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
			},
			want: true,
		},
		{
			name: "Valid passport with size in in",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "64in",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
			},
			want: true,
		},
		{
			name: "Missing BirthYear",
			passport: Passport{
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Missing IssueYear",
			passport: Passport{
				BirthYear:      "1989",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Missing ExpirationYear",
			passport: Passport{
				BirthYear:  "1989",
				IssueYear:  "2020",
				Height:     "164cm",
				HairColor:  "#ed1c24",
				EyeColor:   "blu",
				PassportID: "123456789",
				CountryID:  "98",
			},
			want: false,
		},
		{
			name: "Missing Height",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Missing HairColor",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Missing EyeColor",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Missing PassportID",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "BirthYear too low",
			passport: Passport{
				BirthYear:      "1919",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "BirthYear too high",
			passport: Passport{
				BirthYear:      "2003",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "IssueYear too low",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2009",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "IssueYear too high",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2021",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "ExpirationYear too low",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2019",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "ExpirationYear too high",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2031",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Heigh in cm too low",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "149cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Heigh in cm too high",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "194cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Heigh in in too low",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "58in",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Heigh in in too high",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "77in",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Incorrect hair color, no #",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "ed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Incorrect hair color, more than 6 characters after #",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#eed1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Incorrect hair color, less than 6 characters after #",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#d1c24",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Incorrect hair color, unauthorized characters",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ghijkl",
				EyeColor:       "blu",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Incorrect eye color",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blue",
				PassportID:     "123456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Incorrect PassportID, not a number",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "a23456789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Incorrect PassportID, not enough digits",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "2346789",
				CountryID:      "98",
			},
			want: false,
		},
		{
			name: "Incorrect PassportID, too many digits",
			passport: Passport{
				BirthYear:      "1989",
				IssueYear:      "2020",
				ExpirationYear: "2025",
				Height:         "164cm",
				HairColor:      "#ed1c24",
				EyeColor:       "blu",
				PassportID:     "1123456789",
				CountryID:      "98",
			},
			want: false,
		},
	}

	for _, tt := range testTable {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.passport.IsValidPart2()

			if got != tt.want {
				t.Errorf("Incorrect passport validity: got %t, want %t", got, tt.want)
			}
		})
	}
}
