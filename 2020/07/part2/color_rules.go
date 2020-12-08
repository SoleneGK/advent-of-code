package main

import (
	"bufio"
	"io"
)

type ColorRules struct {
	Colors []*Color
}

func (c *ColorRules) GetColor(colorName string) (color *Color, exists bool) {
	for _, element := range c.Colors {
		if element.Name == colorName {
			return element, true
		}
	}

	return NewColor(""), false
}

func (c *ColorRules) AddColor(colorName string) {
	if !c.hasColor(colorName) {
		newColor := NewColor(colorName)
		c.Colors = append(c.Colors, newColor)
	}
}

func (c *ColorRules) hasColor(colorName string) bool {
	_, exists := c.GetColor(colorName)
	return exists
}

func (c *ColorRules) AddRule(rule Rule) {
	c.AddColor(rule.ContainerColor)
	containerColor, _ := c.GetColor(rule.ContainerColor)

	for currentColorName, quantity := range rule.Content {
		c.AddColor(currentColorName)

		currentColor, _ := c.GetColor(currentColorName)

		containerColor.AddColorToContains(currentColor, quantity)
	}
}

func (c *ColorRules) AddRuleSet(ruleSet io.Reader) {
	scanner := bufio.NewScanner(ruleSet)

	for scanner.Scan() {
		c.AddRule(NewRule(scanner.Text()))
	}
}

func (c *ColorRules) GetTotalNumberOfBagsContainedForColor(colorName string) int {
	color, exists := c.GetColor(colorName)

	if exists {
		return color.GetTotalNumberOfBagsContained()
	}

	return 0
}
