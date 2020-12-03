package main

import (
	"reflect"
	"strings"
	"testing"
)

func TestGetAreaMap(t *testing.T) {
	getAreaMapTests := []struct {
		name  string
		input string
		want  [][]byte
	}{
		{
			name:  "A tree",
			input: "#",
			want: [][]byte{
				[]byte{
					tree,
				},
			},
		},
		{
			name:  "An open ground",
			input: ".",
			want: [][]byte{
				[]byte{
					openGround,
				},
			},
		},
		{
			name:  "A whole line",
			input: "..##.......",
			want: [][]byte{
				[]byte{
					openGround,
					openGround,
					tree,
					tree,
					openGround,
					openGround,
					openGround,
					openGround,
					openGround,
					openGround,
					openGround,
				},
			},
		},
		{
			name:  "Several lines",
			input: "#...#...#..\n.#....#..#.",
			want: [][]byte{
				[]byte{
					tree,
					openGround,
					openGround,
					openGround,
					tree,
					openGround,
					openGround,
					openGround,
					tree,
					openGround,
					openGround,
				},
				[]byte{
					openGround,
					tree,
					openGround,
					openGround,
					openGround,
					openGround,
					tree,
					openGround,
					openGround,
					tree,
					openGround,
				},
			},
		},
	}

	for _, tt := range getAreaMapTests {
		t.Run(tt.name, func(t *testing.T) {
			got := getAreaMap(strings.NewReader(tt.input))

			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Uncorrect map, got\n%v\nwant%v\n", got, tt.want)
			}
		})
	}
}

func TestGetMapSize(t *testing.T) {
	getMapsTests := []struct {
		name         string
		areaMap      [][]byte
		wantedHeight int
		wantedWidth  int
	}{
		{
			name:         "Empty map",
			areaMap:      [][]byte{},
			wantedHeight: 0,
			wantedWidth:  0,
		},
		{
			name: "One line",
			areaMap: [][]byte{
				[]byte{},
			},
			wantedHeight: 1,
			wantedWidth:  0,
		},
		{
			name: "Full map",
			areaMap: [][]byte{
				[]byte{
					tree,
					openGround,
					tree,
				},
				[]byte{
					openGround,
					openGround,
					tree,
				},
			},
			wantedHeight: 2,
			wantedWidth:  3,
		},
	}

	for _, test := range getMapsTests {
		t.Run(test.name, func(t *testing.T) {
			height, width := getMapSize(test.areaMap)

			if height != test.wantedHeight {
				t.Fatalf("Incorrect map height: got %d, want %d", height, test.wantedHeight)
			}

			if width != test.wantedWidth {
				t.Errorf("Incorrect map width: got %d, want %d", width, test.wantedWidth)
			}
		})
	}
}

func TestIsATree(t *testing.T) {
	t.Run("returns true when it's a tree", func(t *testing.T) {
		got := isATree(tree)
		want := true

		assertBool(t, got, want)
	})

	t.Run("returns false when it's not a tree", func(t *testing.T) {
		got := isATree(openGround)
		want := false

		assertBool(t, got, want)
	})
}

func TestMove(t *testing.T) {
	assertPosition := func(t *testing.T, got, want Coordinates) {
		t.Helper()

		if !reflect.DeepEqual(got, want) {
			t.Errorf("Uncorrect position: got %v, want %v", got, want)
		}
	}

	t.Run("move should return new coordinates", func(t *testing.T) {
		position := Coordinates{0, 0}
		slope := Coordinates{3, 1}
		mapWidth := 7

		got := getNewPosition(position, slope, mapWidth)
		want := Coordinates{3, 1}

		if !reflect.DeepEqual(got, want) {
			t.Errorf("Uncorrect position: got %v, want %v", got, want)
		}
	})

	t.Run("X coordinates should not be higher than mapWidth", func(t *testing.T) {
		position := Coordinates{7, 13}
		slope := Coordinates{2, 3}
		mapWidth := 8

		got := getNewPosition(position, slope, mapWidth)
		want := Coordinates{1, 16}

		assertPosition(t, got, want)
	})
}

func TestHasATreeAtPosition(t *testing.T) {
	areaMap := [][]byte{
		[]byte{
			tree,
			openGround,
			openGround,
		},
		[]byte{
			openGround,
			tree,
			tree,
		},
	}

	t.Run("there is a tree", func(t *testing.T) {
		position := Coordinates{X: 2, Y: 1}

		got := hasATreeAtPosition(areaMap, position)
		want := true

		assertBool(t, got, want)
	})

	t.Run("there is no tree", func(t *testing.T) {
		position := Coordinates{X: 1, Y: 0}

		got := hasATreeAtPosition(areaMap, position)
		want := false

		assertBool(t, got, want)
	})
}

func assertBool(t *testing.T, got, want bool) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect check, got %v, want %v", got, want)
	}
}
