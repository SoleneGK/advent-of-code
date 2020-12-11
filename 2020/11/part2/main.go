package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
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

	answer := layout.GetNumberOfOccupiedSeats()

	fmt.Printf("After stabilization, there are %d occupied seats\n", answer)
}

var (
	emptySeat    = []byte("L")[0]
	occupiedSeat = []byte("#")[0]
	floor        = []byte(".")[0]
)

type Layout struct {
	currentSeating Seating
	nextSeating    Seating
}

func (l *Layout) InitializeSeating(data io.Reader) {
	scanner := bufio.NewScanner(data)

	for scanner.Scan() {
		l.currentSeating = append(l.currentSeating, []byte(scanner.Text()))
	}
}

func (l *Layout) ApplySeatingRules() {
	l.nextSeating = l.copySeating(l.currentSeating)
	hasBeenModified := true

	for hasBeenModified {
		hasBeenModified = l.executeNextStep()
	}
}

// It's ugly, but i do not have the patience to look for a better solution now
func (l *Layout) copySeating(seatingToCopy Seating) Seating {
	copyOfSeating := Seating{}

	for _, row := range seatingToCopy {
		copyOfSeating = l.copyRowInto(row, copyOfSeating)
	}

	return copyOfSeating
}

func (l *Layout) copyRowInto(row []byte, newSeating Seating) Seating {
	newRow := make([]byte, len(row))
	copy(newRow, row)
	return append(newSeating, newRow)
}

func (l *Layout) executeNextStep() (hasBeenModified bool) {
	for rowNumber, row := range l.currentSeating {
		for colNumber := range row {
			currentSpot := Spot{rowNumber, colNumber}

			madeModification := l.updateSpot(currentSpot)

			if madeModification {
				hasBeenModified = true
			}
		}
	}

	l.currentSeating = l.copySeating(l.nextSeating)

	return
}

func (l *Layout) updateSpot(spot Spot) (hasBeenModified bool) {
	if l.currentSeating.GetValue(spot) == floor {
		return false
	}

	numberOfOccupiedVisibleSeats := l.currentSeating.getNumberOfOccupiedVisibleSeats(spot)

	if l.currentSeating.GetValue(spot) == emptySeat && numberOfOccupiedVisibleSeats == 0 {
		l.nextSeating.switchState(spot)
		return true
	}

	if l.currentSeating.GetValue(spot) == occupiedSeat && numberOfOccupiedVisibleSeats >= 5 {
		l.nextSeating.switchState(spot)
		return true
	}

	return false
}

func (l *Layout) GetNumberOfOccupiedSeats() (number int) {
	for row := 0; row < len(l.currentSeating); row++ {
		for col := 0; col < len(l.currentSeating[row]); col++ {
			if l.currentSeating.GetValue(Spot{row, col}) == occupiedSeat {
				number++
			}
		}
	}
	return
}

type Seating [][]byte

func (s Seating) GetValue(spot Spot) byte {
	return s[spot.Row][spot.Col]
}

func (s Seating) SetValue(spot Spot, newValue byte) {
	s[spot.Row][spot.Col] = newValue
}

func (s Seating) getNumberOfOccupiedVisibleSeats(spot Spot) (number int) {
	visibleSeats := s.getFirstVisibleSeats(spot)

	for _, seat := range visibleSeats {
		if s.GetValue(seat) == occupiedSeat {
			number++
		}
	}

	return
}

func (s Seating) getFirstVisibleSeats(spot Spot) []Spot {
	seats := []Spot{}

	for _, vector := range vectorList {
		visibleSeat, found := s.getFirstVisibleSeatInDirection(spot, vector)
		if found {
			seats = append(seats, visibleSeat)
		}
	}
	return seats
}

var vectorList = []Spot{
	Spot{-1, -1},
	Spot{-1, 0},
	Spot{-1, 1},
	Spot{0, -1},
	Spot{0, 1},
	Spot{1, -1},
	Spot{1, 0},
	Spot{1, 1},
}

func (s Seating) getFirstVisibleSeatInDirection(spot, vector Spot) (visibleSeat Spot, found bool) {
	spotToTest := spot
	spotToTest.Translate(vector)

	for s.isValidSpot(spotToTest) {
		if s.GetValue(spotToTest) != floor {
			return spotToTest, true
		}

		spotToTest.Translate(vector)
	}

	return Spot{}, false
}

func (s Seating) isValidSpot(spot Spot) bool {
	return spot.Row >= 0 &&
		spot.Row < len(s) &&
		spot.Col >= 0 &&
		spot.Col < len(s[0])
}

func (s Seating) switchState(spot Spot) {
	if s.GetValue(spot) == emptySeat {
		s.SetValue(spot, occupiedSeat)
	} else {
		s.SetValue(spot, emptySeat)
	}
}

type Spot struct {
	Row int
	Col int
}

func (s *Spot) Translate(vector Spot) {
	s.Row += vector.Row
	s.Col += vector.Col
}
