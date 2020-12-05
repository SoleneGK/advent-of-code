package main

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
)

func TestExtractRowAndColStrings(t *testing.T) {
	line := "FBFBBFFRLR"

	gotRow, gotCol := extractRowAndColStrings(line)
	wantRow := "FBFBBFF"
	wantCol := "RLR"

	if gotRow != wantRow {
		t.Errorf("Incorrect row: got %v, want %v", gotRow, wantRow)
	}

	if gotCol != wantCol {
		t.Errorf("Incorrect col: got %v, want %v", gotCol, wantCol)
	}
}

func TestGetRowNumber(t *testing.T) {
	rowString := "FBFBBFF"

	got := getRowNumber(rowString)
	want := 44

	assertInt(t, got, want)
}

func TestGetColNumber(t *testing.T) {
	colString := "RLR"

	got := getColNumber(colString)
	want := 5

	assertInt(t, got, want)
}

func TestGetSeatIDWithColAndRow(t *testing.T) {
	row := 44
	col := 5

	got := getSeatIDWithColAndRow(row, col)
	want := 357

	assertInt(t, got, want)
}

func TestGetSeatID(t *testing.T) {
	seat := "FBFBBFFRLR"

	got := getSeatID(seat)
	want := 357

	assertInt(t, got, want)
}

func TestGetHighestSeatID(t *testing.T) {
	seatList := "FBFBBFFRLR\nBFFFBBFRRR\nBBFFBBFRLL\nFFFBBBFRRR"

	got := getHighestSeatID(strings.NewReader(seatList))
	want := 820

	assertInt(t, got, want)
}

func TestGetSeatIDList(t *testing.T) {
	seatList := "FBFBBFFRLR\nBFFFBBFRRR\nBBFFBBFRLL\nFFFBBBFRRR"

	got := getSeatIDList(strings.NewReader(seatList))
	want := []int{357, 567, 820, 119}

	if !reflect.DeepEqual(got, want) {
		fmt.Printf("Incorrect int list: got %v, want %v", got, want)
	}
}

func assertInt(t *testing.T, got, want int) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect value: got %d, want %d", got, want)
	}
}
