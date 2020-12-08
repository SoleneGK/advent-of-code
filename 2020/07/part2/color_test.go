package main

import (
	"testing"
)

func TestAddColorToContains(t *testing.T) {
	t.Run("Add color to an empty slice", func(t *testing.T) {
		got := NewColor("dotted cyan")
		newColor := NewColor("shiny silver")

		got.AddColorToContains(newColor, 5)
		want := &Color{
			Name: "dotted cyan",
			Contains: map[*Color]int{
				newColor: 5,
			},
		}

		assertColor(t, got, want)
	})

	t.Run("Add new color to a non-empty slice", func(t *testing.T) {
		color1 := NewColor("light red")
		color2 := NewColor("faded black")
		got := &Color{
			Name:     "dotted cyan",
			Contains: map[*Color]int{color1: 6},
		}

		got.AddColorToContains(color2, 1)
		want := &Color{
			Name: "dotted cyan",
			Contains: map[*Color]int{
				color1: 6,
				color2: 1,
			},
		}

		assertColor(t, got, want)
	})
}

func TestGetTotalNumberOfBagsContained(t *testing.T) {
	t.Run("Contains no bags", func(t *testing.T) {
		color := NewColor("sweet lavander")
		got := color.GetTotalNumberOfBagsContained()
		assertInt(t, got, 0)
	})

	t.Run("Contains bags that contain no other bags", func(t *testing.T) {
		color1 := NewColor("subtle pink")
		color2 := NewColor("bright purple")
		color3 := &Color{
			Name: "light white",
			Contains: map[*Color]int{
				color1: 2,
				color2: 8,
			},
		}

		got := color3.GetTotalNumberOfBagsContained()
		assertInt(t, got, 10)
	})

	t.Run("Contains bags that contains other bags", func(t *testing.T) {
		color1 := NewColor("subtle pink")
		color2 := NewColor("bright purple")
		color3 := &Color{
			Name: "light white",
			Contains: map[*Color]int{
				color1: 2,
			},
		}
		color4 := &Color{
			Name: "plaid coral",
			Contains: map[*Color]int{
				color1: 1,
				color2: 4,
			},
		}
		color5 := &Color{
			Name: "dotted teal",
			Contains: map[*Color]int{
				color3: 3,
				color4: 7,
			},
		}

		got := color5.GetTotalNumberOfBagsContained()
		assertInt(t, got, 51)
	})
}
