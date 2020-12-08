package main

import (
	"bufio"
	"io"
)

type ColorRules struct {
	Colors []*Color
}

func (c *ColorRules) AddColor(newColorName string) {
	newColor := &Color{Name: newColorName}
	c.Colors = append(c.Colors, newColor)
}

func (c *ColorRules) GetColor(searchedColorName string) (*Color, bool) {
	for _, color := range c.Colors {
		if color.Name == searchedColorName {
			return color, true
		}
	}

	return &Color{}, false
}

func (c *ColorRules) AddRule(rule string) {
	containerColorName := getContainerColorFromRule(rule)
	c.AddColorIfNew(containerColorName)
	containerColor, _ := c.GetColor(containerColorName)

	if ruleContainsOtherBags(rule) {
		colorList := getContainedColorsFromRule(rule)

		for _, color := range colorList {
			c.AddColorIfNew(color)
			containedColor, _ := c.GetColor(color)

			containedColor.AddColorToCanBeContainedIn(containerColor)
			containerColor.AddColorToContains(containedColor)
		}
	}

}

func (c *ColorRules) AddColorIfNew(colorName string) {
	if _, exists := c.GetColor(colorName); !exists {
		c.AddColor(colorName)
	}
}

func (c *ColorRules) AddRuleSet(ruleSet io.Reader) {
	scanner := bufio.NewScanner(ruleSet)

	for scanner.Scan() {
		c.AddRule(scanner.Text())
	}
}

func (c *ColorRules) GetNumberOfPossibleContainersOfColor(colorName string) int {
	color, exists := c.GetColor(colorName)

	if exists {
		return color.GetNumberOfPossibleContainers()
	} else {
		return 0
	}
}
