package main

import (
	"testing"
)

func TestGetColor(t *testing.T) {
	presentColorName1 := "clear turquoise"
	presentColorName2 := "faded gray"
	absentColorName := "dull blue"

	colorRules := ColorRules{
		[]*Color{
			NewColor(presentColorName1),
			NewColor(presentColorName2),
		},
	}

	t.Run("Color is not in slice, retun false and empty color", func(t *testing.T) {
		gotColor, gotBool := colorRules.GetColor(absentColorName)

		assertColor(t, gotColor, NewColor(""))
		assertBool(t, gotBool, false)
	})

	t.Run("Color is in slice, return true and the color", func(t *testing.T) {
		gotColor, gotBool := colorRules.GetColor(presentColorName2)

		assertColor(t, gotColor, NewColor(presentColorName2))
		assertBool(t, gotBool, true)
	})
}

func TestAddColor(t *testing.T) {
	t.Run("New color in empty set", func(t *testing.T) {
		newColorName := "pale chartreuse"
		colorRules := ColorRules{}

		colorRules.AddColor(newColorName)
		want := []*Color{
			NewColor(newColorName),
		}

		assertColorSlice(t, colorRules.Colors, want)
	})

	t.Run("New color in non-empty set", func(t *testing.T) {
		newColorName := "vibrant red"
		oldColorName := "dim fuchsia"
		colorRules := ColorRules{
			[]*Color{
				NewColor(oldColorName),
			},
		}

		colorRules.AddColor(newColorName)
		want := []*Color{
			NewColor(oldColorName),
			NewColor(newColorName),
		}

		assertColorSlice(t, colorRules.Colors, want)
	})

	t.Run("Color already in set", func(t *testing.T) {
		colorName1 := "posh coral"
		colorName2 := "faded gold"
		colorRules := ColorRules{
			[]*Color{
				NewColor(colorName1),
				NewColor(colorName2),
			},
		}

		colorRules.AddColor(colorName1)
		want := []*Color{
			NewColor(colorName1),
			NewColor(colorName2),
		}

		assertColorSlice(t, colorRules.Colors, want)
	})
}

func TestAddRule(t *testing.T) {
	t.Run("New color as container, contains no bag", func(t *testing.T) {
		rule := Rule{}
		rule.Analyze("muted beige bags contain no other bags.")
		colorRules := ColorRules{}

		colorRules.AddRule(rule)

		assertNumberOfColors(t, colorRules.Colors, 1)
		assertColorSliceContains(t, colorRules.Colors, "muted beige")
	})

	t.Run("Color already in slice as container, contains no bag", func(t *testing.T) {
		rule := Rule{}
		rule.Analyze("muted beige bags contain no other bags.")
		colorRules := ColorRules{
			[]*Color{
				NewColor("muted beige"),
				NewColor("pale yellow"),
			},
		}

		colorRules.AddRule(rule)

		assertNumberOfColors(t, colorRules.Colors, 2)
		assertColorSliceContains(t, colorRules.Colors, "muted beige")
	})

	t.Run("Container contains other bags", func(t *testing.T) {
		rule := NewRule("dim crimson bags contain 3 drab turquoise bags, 2 faded crimson bags, 2 plaid chartreuse bags.")
		colorRules := ColorRules{
			[]*Color{
				NewColor("drab turquoise"),
				NewColor("wavy magenta"),
			},
		}

		colorRules.AddRule(rule)

		assertNumberOfColors(t, colorRules.Colors, 5)
		assertColorSliceContains(t, colorRules.Colors, "dim crimson")
		assertColorSliceContains(t, colorRules.Colors, "drab turquoise")
		assertColorSliceContains(t, colorRules.Colors, "faded crimson")
		assertColorSliceContains(t, colorRules.Colors, "plaid chartreuse")
		assertColorSliceContains(t, colorRules.Colors, "wavy magenta")
	})
}
