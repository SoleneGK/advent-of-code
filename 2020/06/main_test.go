package main

import (
	"reflect"
	"strings"
	"testing"
)

func TestGetNumberOfYesAnswers(t *testing.T) {
	answers := AnswerList{5: true, 7: true}

	got := answers.GetNumberOfYesAnswers()
	want := 2

	assertInt(t, got, want)
}

func TestGetUnicodeValueOfChar(t *testing.T) {
	char := []byte("e")[0]

	got := getUnicodeValueOfChar(char)
	want := 101

	assertInt(t, got, want)
}

func TestGetPassengerAnswers(t *testing.T) {
	rawData := "aek"

	got := getPassengerAnswers(rawData)
	want := AnswerList{0: true, 4: true, 10: true}

	assertAnswerList(t, got, want)
}

func TestGetGroupAnswersPart1(t *testing.T) {
	answers1 := AnswerList{0: true, 5: true}
	answers2 := AnswerList{5: true, 15: true}

	got := getGroupAnswersPart1(answers1, answers2)
	want := AnswerList{0: true, 5: true, 15: true}

	assertAnswerList(t, got, want)
}

func TestGetAllAnswerLists(t *testing.T) {
	input := `abc

a
d
f

ag
ao`

	gotPart1, gotPart2 := getAllAnswerLists(strings.NewReader(input))

	wantPart1 := []AnswerList{
		AnswerList{0: true, 1: true, 2: true},
		AnswerList{0: true, 3: true, 5: true},
		AnswerList{0: true, 6: true, 14: true},
	}
	wantPart2 := []AnswerList{
		AnswerList{0: true, 1: true, 2: true},
		AnswerList{},
		AnswerList{0: true},
	}

	assertAnswerListSlice(t, gotPart1, wantPart1)
	assertAnswerListSlice(t, gotPart2, wantPart2)
}

func TestGetGroupAnswersPart2(t *testing.T) {
	answers1 := AnswerList{0: true, 5: true}
	answers2 := AnswerList{5: true, 15: true}

	got := getGroupAnswersPart2(answers1, answers2)
	want := AnswerList{5: true}

	assertAnswerList(t, got, want)
}

func assertInt(t *testing.T, got, want int) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect number: got %v, want %v\n", got, want)
	}
}

func assertAnswerList(t *testing.T, got, want AnswerList) {
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect answer list: got %v, want %v\n", got, want)
	}
}

func assertAnswerListSlice(t *testing.T, got, want []AnswerList) {
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect AnswerList slice: got %v, want %v", got, want)
	}
}
