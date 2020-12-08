package main

import (
	"reflect"
	"testing"
)

func assertBool(t *testing.T, got, want bool) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect boolean: got %t, want %t", got, want)
	}
}

func assertColor(t *testing.T, got, want *Color) {
	t.Helper()

	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect Color: got %v, want %v", got, want)
	}
}

func assertColorSlice(t *testing.T, got, want []*Color) {
	t.Helper()

	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect []Color: got %v, want %v", got, want)
	}
}

func assertColorSliceContains(t *testing.T, colorSlice []*Color, colorName string) {
	t.Helper()

	contains := false

	for _, element := range colorSlice {
		if element.Name == colorName {
			contains = true
			break
		}
	}

	if !contains {
		t.Errorf("The color %s is not in slice", colorName)
	}
}

func assertInt(t *testing.T, got, want int) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect number: got %d, want %d", got, want)
	}
}

func assertNumberOfColors(t *testing.T, got []*Color, want int) {
	t.Helper()

	if len(got) != want {
		t.Errorf("Incorrect number of colors: got %d, want %d", len(got), want)
	}
}

func assertRule(t *testing.T, got, want Rule) {
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect rule: got %v, want %v", got, want)
	}
}
