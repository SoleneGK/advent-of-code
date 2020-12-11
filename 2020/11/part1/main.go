package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"reflect"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	layout := Layout{}
	layout.InitializeSeating(file)
	layout.ApplySeatingRules()

	answerPart1 := layout.GetNumberOfOccupiedSeats()
	fmt.Printf("After stabilization, there are %d occupied seats\n", answerPart1)

}

var (
	emptySeat    = []byte("L")[0]
	occupiedSeat = []byte("#")[0]
	floor        = []byte(".")[0]
)

type Layout struct {
	Seating Seating
}

func (l *Layout) InitializeSeating(data io.Reader) {
	scanner := bufio.NewScanner(data)

	for scanner.Scan() {
		l.Seating = append(l.Seating, []byte(scanner.Text()))
	}
}

// I stopped writing tests because i'm loosing patience
// Code is ugly
// Will try to do better in part 2
func (l *Layout) ApplySeatingRules() {
	wasSeatingModified := true

	for wasSeatingModified {
		newSeating := Seating{}

		for row, seatCol := range l.Seating {
			newCol := []byte{}

			for col := range seatCol {
				seat := Seat{row + 1, col + 1}

				if l.Seating.GetValue(seat) != floor {
					newSeatState := l.getNewSeatState(seat)
					newCol = append(newCol, newSeatState)
				} else {
					newCol = append(newCol, floor)
				}
			}

			newSeating = append(newSeating, newCol)
		}

		if reflect.DeepEqual(l.Seating, newSeating) {
			wasSeatingModified = false
		}

		l.Seating = newSeating
	}
}

func (l *Layout) getNewSeatState(seat Seat) byte {
	numberOfOccupedAdjacentSeats := l.Seating.GetNumberOfOccupiedAdjacentSeats(seat)

	if numberOfOccupedAdjacentSeats == 0 {
		return occupiedSeat
	} else if numberOfOccupedAdjacentSeats >= 4 {
		return emptySeat
	} else {
		return l.Seating.GetValue(seat)
	}
}

func (l *Layout) GetNumberOfOccupiedSeats() (number int) {
	for i := 1; i <= len(l.Seating); i++ {
		for j := 1; j <= len(l.Seating[i-1]); j++ {
			if l.Seating.GetValue(Seat{Row: i, Col: j}) == occupiedSeat {
				number++
			}
		}
	}
	return
}

type Seating [][]byte

func (s Seating) GetValue(seat Seat) byte {
	return s[seat.Row-1][seat.Col-1]
}

func (s Seating) SetValue(seat Seat, newValue byte) {
	s[seat.Row-1][seat.Col-1] = newValue
}

func (s Seating) GetNumberOfOccupiedAdjacentSeats(seat Seat) (number int) {
	for _, adjacentSeat := range s.getAdjacentSeats(seat) {
		if s.isOccupied(adjacentSeat) {
			number++
		}
	}
	return
}

func (s Seating) getAdjacentSeats(seat Seat) (adjacentSeats []Seat) {
	firstRow, lastRow, firstCol, lastCol := s.getBoundsOfAdjacentSeats(seat)

	for row := firstRow; row <= lastRow; row++ {
		for col := firstCol; col <= lastCol; col++ {
			if row != seat.Row || col != seat.Col {
				seatToAdd := Seat{row, col}
				adjacentSeats = append(adjacentSeats, seatToAdd)
			}
		}
	}

	return
}

func (s Seating) getBoundsOfAdjacentSeats(seat Seat) (firstRow, lastRow, firstCol, lastCol int) {
	firstRow = s.getFirstRow(seat.Row)
	lastRow = s.getLastRow(seat.Row)
	firstCol = s.getFirstCol(seat.Col)
	lastCol = s.getLastCol(seat.Col)
	return
}

func (s Seating) getFirstRow(row int) int {
	if row == 1 {
		return 1
	} else {
		return row - 1
	}
}

func (s Seating) getLastRow(row int) int {
	if row == len(s) {
		return row
	} else {
		return row + 1
	}
}

func (s Seating) getFirstCol(col int) int {
	if col == 1 {
		return 1
	} else {
		return col - 1
	}
}

func (s Seating) getLastCol(col int) int {
	if col == len(s[0]) {
		return col
	} else {
		return col + 1
	}
}

func (s Seating) isOccupied(seat Seat) bool {
	return s.GetValue(seat) == occupiedSeat
}

type Seat struct {
	Row int
	Col int
}
