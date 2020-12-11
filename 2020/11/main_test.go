package main

import (
	"reflect"
	"strings"
	"testing"
)

func TestInitializeSeating(t *testing.T) {
	t.Run("One line", func(t *testing.T) {
		data := strings.NewReader("L.LL.LL.LL")
		layout := Layout{}

		layout.InitializeSeating(data)
		want := [][]byte{
			[]byte("L.LL.LL.LL"),
		}

		assertByteTable(t, layout.Seating, want)
	})

	t.Run("Several lines", func(t *testing.T) {
		data := strings.NewReader("L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL")
		layout := Layout{}

		layout.InitializeSeating(data)
		want := [][]byte{
			[]byte("L.LL.LL.LL"),
			[]byte("LLLLLLL.LL"),
			[]byte("L.L.L..L.."),
			[]byte("LLLL.LL.LL"),
		}

		assertByteTable(t, layout.Seating, want)
	})
}

func TestGetValueMethodOfSeating(t *testing.T) {
	seating := Seating{
		[]byte("uiecds"),
		[]byte("cdrjkg"),
	}
	seat := newSeat(1, 3)

	got := seating.GetValue(seat)
	want := []byte("e")[0]

	assertByte(t, got, want)
}

func TestSetValueMethodOfSeating(t *testing.T) {
	seating := Seating{
		[]byte("uiecds"),
		[]byte("cdrjkg"),
	}
	seat := newSeat(2, 4)

	seating.SetValue(seat, 114)

	assertByte(t, seating[1][3], 114)
}

func TestGetNumberOfOccupiedAdjacentSeats(t *testing.T) {
	seating := Seating{
		[]byte{floor, occupiedSeat, emptySeat},
		[]byte{occupiedSeat, emptySeat, occupiedSeat},
		[]byte{occupiedSeat, floor, emptySeat},
	}

	testTable := []struct {
		name string
		seat Seat
		want int
	}{
		{
			name: "8 adjacent emplacements",
			seat: newSeat(2, 2),
			want: 4,
		},
		{
			name: "No row above",
			seat: newSeat(1, 2),
			want: 2,
		},
		{
			name: "No row under",
			seat: newSeat(3, 2),
			want: 3,
		},
		{
			name: "No col at the left",
			seat: newSeat(3, 1),
			want: 1,
		},
		{
			name: "No col at the right",
			seat: newSeat(2, 3),
			want: 1,
		},
	}

	for _, tt := range testTable {
		t.Run(tt.name, func(t *testing.T) {
			got := seating.GetNumberOfOccupiedAdjacentSeats(tt.seat)
			assertInt(t, got, tt.want)
		})
	}
}

// I feel like i'm not doing enough test for a production code
func TestNumberOfOccupiedSeats(t *testing.T) {
	layout := Layout{
		Seating{
			[]byte{floor, occupiedSeat, emptySeat, emptySeat},
			[]byte{occupiedSeat, emptySeat, floor, floor},
		},
	}

	got := layout.GetNumberOfOccupiedSeats()
	assertInt(t, got, 2)
}

// Helpers
func assertByte(t *testing.T, got, want byte) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect byte: got %d, want %d", got, want)
	}
}

func assertByteTable(t *testing.T, got, want [][]byte) {
	t.Helper()

	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect seating: got %v, want %v", got, want)
	}
}

func assertInt(t *testing.T, got, want int) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect value: got %d, want %d", got, want)
	}
}

// Tools
func newSeat(row, col int) Seat {
	return Seat{Row: row, Col: col}
}
