package main

import (
	"reflect"
	"strings"
	"testing"
)

func TestGetNumberOfYesAnswers(t *testing.T) {
	answers := [numberOnQuestions]bool{}
	answers[5] = true
	answers[7] = true

	got := getNumberOfYesAnswers(answers)
	want := 2

	assertInt(t, got, want)
}

// It's a private method, I shouldn't test it
// Since it's a practice problem, i decided to do it anyway
func TestGetUnicodeValueOfChar(t *testing.T) {
	answers := AnswerList{}
	char := []byte("e")[0]

	got := answers.getUnicodeValueOfChar(char)
	want := 101

	assertInt(t, got, want)
}

func TestAddAnswers(t *testing.T) {
	rawData := "aek"
	answers := AnswerList{}

	answers.AddAnswers(rawData)

	want := AnswerList{}
	want[0] = true
	want[4] = true
	want[10] = true

	if !reflect.DeepEqual(answers, want) {
		t.Errorf("Incorrect answer table: got %v, want %v\n", answers, want)
	}
}

func assertInt(t *testing.T, got, want int) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect number: got %v, want %v\n", got, want)
	}
}

func TestGetAllAnswerLists(t *testing.T) {
	input := `abc

a
d
f

kd
mo`

	got := getAllAnswerLists(strings.NewReader(input))

	answerList1 := AnswerList{}
	answerList1[0] = true
	answerList1[1] = true
	answerList1[2] = true

	answerList2 := AnswerList{}
	answerList2[0] = true
	answerList2[3] = true
	answerList2[5] = true

	answerList3 := AnswerList{}
	answerList3[10] = true
	answerList3[3] = true
	answerList3[12] = true
	answerList3[14] = true

	want := []AnswerList{answerList1, answerList2, answerList3}

	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect lists: got %v, want %v", got, want)
	}
}
