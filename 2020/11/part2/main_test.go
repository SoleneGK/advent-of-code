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

		assertByteTable(t, layout.currentSeating, want)
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

		assertByteTable(t, layout.currentSeating, want)
	})
}

// func TestCopySeating(t *testing.T) {
// 	seating := Seating{[]byte{floor}}
// 	layout := Layout{currentSeating: seating}

// 	got := layout.copySeating()
// 	want := Seating{[]byte{floor}}

// 	if !reflect.DeepEqual(got, want) {
// 		t.Errorf("Incorrect copy: got %v, want %v", got, seating)
// 	}
// }

func TestNumberOfOccupiedSeats(t *testing.T) {
	layout := Layout{
		currentSeating: Seating{
			[]byte{floor, occupiedSeat, emptySeat, emptySeat},
			[]byte{occupiedSeat, emptySeat, floor, floor},
		},
	}

	got := layout.GetNumberOfOccupiedSeats()
	assertInt(t, got, 2)
}

func TestGetValue(t *testing.T) {
	seating := Seating{
		[]byte("uiecds"),
		[]byte("cdrjkg"),
	}
	spot := Spot{0, 2}

	got := seating.GetValue(spot)
	want := []byte("e")[0]

	assertByte(t, got, want)
}

func TestSetValue(t *testing.T) {
	seating := Seating{
		[]byte("uiecds"),
		[]byte("cdrjkg"),
	}
	spot := Spot{1, 3}

	seating.SetValue(spot, 114)

	assertByte(t, seating[1][3], 114)
}

// Yeah, incomplete… was too late, not motivated enough
func TestGetFirstVisibleSeats(t *testing.T) {
	testTable := []struct {
		name              string
		seating           Seating
		spot              Spot
		firstVisibleSeats []Spot
	}{
		{
			name:              "No visible seats",
			seating:           Seating{[]byte{occupiedSeat}},
			spot:              Spot{0, 0},
			firstVisibleSeats: []Spot{},
		},
	}

	for _, tt := range testTable {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.seating.getFirstVisibleSeats(tt.spot)
			assertSpotSlice(t, got, tt.firstVisibleSeats)
		})
	}
}

func TestSwitchSpotState(t *testing.T) {
	seating := Seating{[]byte{occupiedSeat, emptySeat}}

	t.Run("Empty seat becomes occuped", func(t *testing.T) {
		seating.switchState(Spot{0, 1})
		assertByte(t, seating[0][1], occupiedSeat)
	})

	t.Run("Occupied seat becomes empty", func(t *testing.T) {
		seating.switchState(Spot{0, 0})
		assertByte(t, seating[0][0], emptySeat)
	})
}

//Helpers
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

func assertSpotSlice(t *testing.T, got, want []Spot) {
	t.Helper()

	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect Spot slice: got %v, want %v", got, want)
	}
}
