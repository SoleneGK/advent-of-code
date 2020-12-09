package main

import (
	"reflect"
	"strings"
	"testing"
)

func TestAddInput(t *testing.T) {
	xmas := XmasData{}
	input := strings.NewReader(`2
7
5
6
15
87
29`)
	preambleLength := 3

	xmas.AddInput(input, preambleLength)
	want := XmasData{
		Preamble: []int{2, 7, 5},
		Data:     []int{6, 15, 87, 29},
	}

	assertXmasData(t, xmas, want)
}

func TestIsValid(t *testing.T) {
	xmas := XmasData{
		previousNumbers: []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
		Data:            []int{13, 42, 20},
	}

	testTable := []struct {
		name  string
		index int
		want  bool
	}{
		{name: "Valid value", index: 0, want: true},
		{name: "Invalid value", index: 1, want: false},
		{name: "Edge case: invalid event if double of a preamble int", index: 2, want: false},
	}

	for _, tt := range testTable {
		t.Run(tt.name, func(t *testing.T) {
			got := xmas.IsValid(tt.index)

			if got != tt.want {
				t.Errorf("Incorect boolean: got %t, want %t", got, tt.want)
			}
		})
	}
}

func TestGetFirstInvalidValue(t *testing.T) {
	xmas := XmasData{
		Preamble: []int{35, 20, 15, 25, 47},
		Data:     []int{40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 209, 576},
	}

	got := xmas.GetFirstInvalidValue()
	want := 127

	if got != want {
		t.Errorf("Incorrect number: got %d, want %d", got, want)
	}
}

// Helpers
func assertXmasData(t *testing.T, got, want XmasData) {
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect XmasData: got %v, want %v\n", got, want)
	}
}
