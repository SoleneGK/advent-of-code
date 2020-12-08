package main

import (
	"reflect"
	"testing"
)

func TestGetContainerColorFromRule(t *testing.T) {
	rule := "light red bags contain 1 bright white bag, 2 muted yellow bags."
	got := getContainerColorFromRule(rule)
	assertStrings(t, got, "light red")
}

func TestRuleContainsOtherBags(t *testing.T) {
	t.Run("should return false", func(t *testing.T) {
		rule := "faded blue bags contain no other bags."
		got := ruleContainsOtherBags(rule)
		assertBool(t, got, false)
	})

	t.Run("should return true", func(t *testing.T) {
		rule := "light red bags contain 1 bright white bag, 2 muted yellow bags."
		got := ruleContainsOtherBags(rule)
		assertBool(t, got, true)
	})
}

func TestGetContainedColorsFromRule(t *testing.T) {
	t.Run("one color", func(t *testing.T) {
		rule := "bright white bags contain 1 shiny gold bag."

		got := getContainedColorsFromRule(rule)
		want := []string{
			"shiny gold",
		}

		assertStringSlice(t, got, want)
	})

	t.Run("several colors", func(t *testing.T) {
		rule := "clear lavender bags contain 4 faded magenta bags, 1 mirrored gray bag, 4 wavy yellow bags, 1 dotted cyan bag."

		got := getContainedColorsFromRule(rule)
		want := []string{
			"faded magenta",
			"mirrored gray",
			"wavy yellow",
			"dotted cyan",
		}

		assertStringSlice(t, got, want)
	})
}

// Tests on Color methods
func TestAddColorToContains(t *testing.T) {
	t.Run("Add a color to an empty slice", func(t *testing.T) {
		got := createColor("dotted cyan")
		newColor := createColor("shiny gold")

		got.AddColorToContains(newColor)
		want := &Color{Name: "dotted cyan", Contains: []*Color{newColor}}

		assertColor(t, got, want)
	})

	t.Run("Add a new color to a non-empty slice", func(t *testing.T) {
		color1 := createColor("light red")
		color2 := createColor("faded black")
		got := &Color{Name: "dotted cyan", Contains: []*Color{color1}}

		got.AddColorToContains(color2)
		want := &Color{
			Name:     "dotted cyan",
			Contains: []*Color{color1, color2},
		}

		assertColor(t, got, want)
	})

	t.Run("Add a color already in the slice", func(t *testing.T) {
		color1 := createColor("light red")
		got := &Color{Name: "dotted cyan", Contains: []*Color{color1}}

		got.AddColorToContains(color1)
		want := &Color{
			Name:     "dotted cyan",
			Contains: []*Color{color1},
		}

		assertColor(t, got, want)
	})
}

func TestAddColorToCanBeContainedIn(t *testing.T) {
	t.Run("Add a color to an empty slice", func(t *testing.T) {
		got := createColor("dotted cyan")
		newColor := createColor("shiny gold")

		got.AddColorToCanBeContainedIn(newColor)
		want := &Color{Name: "dotted cyan", CanBeContainedIn: []*Color{newColor}}

		assertColor(t, got, want)
	})

	t.Run("Add a new color to a non-empty slice", func(t *testing.T) {
		color1 := createColor("light red")
		color2 := createColor("faded black")
		got := &Color{Name: "dotted cyan", CanBeContainedIn: []*Color{color1}}

		got.AddColorToCanBeContainedIn(color2)
		want := &Color{
			Name:             "dotted cyan",
			CanBeContainedIn: []*Color{color1, color2},
		}

		assertColor(t, got, want)
	})

	t.Run("Add a color already in the slice", func(t *testing.T) {
		color1 := createColor("light red")
		got := &Color{Name: "dotted cyan", CanBeContainedIn: []*Color{color1}}

		got.AddColorToCanBeContainedIn(color1)
		want := &Color{
			Name:             "dotted cyan",
			CanBeContainedIn: []*Color{color1},
		}

		assertColor(t, got, want)
	})
}

func TestGetNumberOfPossibleContainers(t *testing.T) {
	t.Run("A color with no container", func(t *testing.T) {
		color := createColor("light red")

		got := color.GetNumberOfPossibleContainers()
		assertInt(t, got, 0)
	})

	t.Run("A color contained in colors with no container", func(t *testing.T) {
		color1 := createColor("dotted yellow")
		color2 := createColor("dim brown")
		color3 := &Color{Name: "posh coral", CanBeContainedIn: []*Color{color1, color2}}

		got := color3.GetNumberOfPossibleContainers()
		assertInt(t, got, 2)
	})

	t.Run("A color contained in colors with containers", func(t *testing.T) {
		color1 := createColor("dotted yellow")
		color2 := createColor("dim brown")
		color3 := &Color{Name: "mirrored olive", CanBeContainedIn: []*Color{color1, color2}}
		color4 := createColor("drap green")
		color5 := Color{Name: "posh coral", CanBeContainedIn: []*Color{color3, color4}}

		got := color5.GetNumberOfPossibleContainers()
		assertInt(t, got, 4)
	})

	t.Run("A color contained in colors contained in same color", func(t *testing.T) {
		color1 := createColor("dotted yellow")
		color2 := &Color{Name: "dim brown", CanBeContainedIn: []*Color{color1}}
		color3 := &Color{Name: "mirrored olive", CanBeContainedIn: []*Color{color1}}
		color4 := &Color{Name: "dash green", CanBeContainedIn: []*Color{color2, color3}}

		got := color4.GetNumberOfPossibleContainers()
		assertInt(t, got, 3)
	})
}

// Tests on ColorRules methods
func TestAddColor(t *testing.T) {
	t.Run("Add new color to an empty slice", func(t *testing.T) {
		newColorName := "dotted yellow"
		colorRules := ColorRules{}

		colorRules.AddColor(newColorName)
		want := []*Color{
			createColor(newColorName),
		}

		assertColorSlice(t, colorRules.Colors, want)
	})

	t.Run("Add new color to a non-empty slice", func(t *testing.T) {
		newColorName := "dotted yellow"
		oldColor := createColor("vibrant magenta")
		colorRules := ColorRules{[]*Color{oldColor}}

		colorRules.AddColor(newColorName)
		want := []*Color{
			oldColor,
			createColor(newColorName),
		}

		assertColorSlice(t, colorRules.Colors, want)
	})
}

func TestGetColor(t *testing.T) {
	presentColorName1 := "pale tan"
	presentColorName2 := "striped plum"
	absentColorName := "clear aqua"

	colorRules := ColorRules{
		[]*Color{
			createColor(presentColorName1),
			createColor(presentColorName2),
		},
	}

	t.Run("Returns an empty color and false when the color is not in the slice", func(t *testing.T) {
		gotColor, gotBool := colorRules.GetColor(absentColorName)

		assertColor(t, gotColor, &Color{})
		assertBool(t, gotBool, false)
	})

	t.Run("Return a pointer to the color and true when the color in in the slice", func(t *testing.T) {
		gotColor, gotBool := colorRules.GetColor(presentColorName1)

		assertColor(t, gotColor, createColor(presentColorName1))
		assertBool(t, gotBool, true)
	})
}

func TestAddRule(t *testing.T) {
	t.Run("All colors in the rule are new", func(t *testing.T) {
		rule := "light red bags contain 1 bright white bag, 2 muted yellow bags."
		colorRules := ColorRules{}

		colorRules.AddRule(rule)

		assertNumberOfColors(t, colorRules.Colors, 3)
		assertColorSliceContainsColor(t, colorRules.Colors, "light red")
		assertColorSliceContainsColor(t, colorRules.Colors, "bright white")
		assertColorSliceContainsColor(t, colorRules.Colors, "muted yellow")
	})

	t.Run("Container color in the rule is not new", func(t *testing.T) {
		colorName := "faded blue"
		color := createColor(colorName)
		rule := colorName + " bags contain no other bags."
		colorRules := ColorRules{[]*Color{color}}

		colorRules.AddRule(rule)

		assertNumberOfColors(t, colorRules.Colors, 1)
	})

	t.Run("One contained color in the rule is not new, one is", func(t *testing.T) {
		rule := "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."

		oldColor1 := createColor("shiny gold")
		oldColor2 := createColor("vibrant plum")

		colorRules := ColorRules{[]*Color{oldColor1, oldColor2}}
		colorRules.AddRule(rule)

		assertNumberOfColors(t, colorRules.Colors, 3)
		assertColorSliceContainsColor(t, colorRules.Colors, "dark olive")
	})

	t.Run("Container color should be added to contained colors", func(t *testing.T) {
		rule := "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
		colorRules := ColorRules{}

		colorRules.AddRule(rule)

		assertNumberOfColors(t, colorRules.Colors, 3)

		darkOlive, _ := colorRules.GetColor("dark olive")
		vibrantPlum, _ := colorRules.GetColor("vibrant plum")

		assertColorSliceContainsColor(t, darkOlive.CanBeContainedIn, "shiny gold")
		assertColorSliceContainsColor(t, vibrantPlum.CanBeContainedIn, "shiny gold")
	})

	t.Run("Contained colors should be added to container color", func(t *testing.T) {
		rule := "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
		colorRules := ColorRules{}

		colorRules.AddRule(rule)

		assertNumberOfColors(t, colorRules.Colors, 3)

		shinyGold, _ := colorRules.GetColor("shiny gold")

		assertColorSliceContainsColor(t, shinyGold.Contains, "dark olive")
		assertColorSliceContainsColor(t, shinyGold.Contains, "vibrant plum")
	})
}

// Helpers (alphabetic order)
func assertBool(t *testing.T, got, want bool) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect boolean: got %t, want %t", got, want)
	}
}

func assertColor(t *testing.T, got, want *Color) {
	t.Helper()

	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect []Color: got %v, want %v", got, want)
	}
}

func assertColorSlice(t *testing.T, got, want []*Color) {
	t.Helper()

	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect []Color: got %v, want %v", got, want)
	}
}

func assertColorSliceContainsColor(t *testing.T, colorSlice []*Color, color string) {
	t.Helper()

	contains := false

	for _, element := range colorSlice {
		if element.Name == color {
			contains = true
		}
	}

	if !contains {
		t.Errorf("[]Color should have color %s but doesn't", color)
	}
}

func assertInt(t *testing.T, got, want int) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect int: got %d, want %d", got, want)
	}
}

func assertNumberOfColors(t *testing.T, got []*Color, want int) {
	t.Helper()

	if len(got) != want {
		t.Fatalf("Incorrect color number: got %d, want %d", len(got), want)
	}
}

func assertStrings(t *testing.T, got, want string) {
	t.Helper()

	if got != want {
		t.Errorf("Incorrect string: got %s, want %s", got, want)
	}
}

func assertStringSlice(t *testing.T, got, want []string) {
	t.Helper()

	if !reflect.DeepEqual(got, want) {
		t.Errorf("Incorrect []string: got %v, want %v", got, want)
	}
}

// Tools
func createColor(name string) *Color {
	return &Color{Name: name}
}
