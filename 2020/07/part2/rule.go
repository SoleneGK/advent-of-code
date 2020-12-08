package main

import (
	"fmt"
	"strconv"
	"strings"
)

func NewRule(ruleAsString string) Rule {
	rule := Rule{}
	rule.Analyze(ruleAsString)
	return rule
}

type Rule struct {
	ContainerColor string
	Content        map[string]int
}

func (r *Rule) Analyze(rawRule string) {
	elements := strings.Split(rawRule, " bags contain ")
	r.ContainerColor = elements[0]

	r.Content = map[string]int{}

	if elements[1] != "no other bags." {
		r.extractContent(elements[1])
	}
}

func (r *Rule) extractContent(rawContent string) {
	rawContentWithoutFinalPoint := strings.TrimSuffix(rawContent, ".")

	contentList := strings.Split(rawContentWithoutFinalPoint, ", ")

	for _, contentElement := range contentList {
		quantity, color := r.extractContentData(contentElement)
		r.Content[color] = quantity
	}
}

func (r *Rule) extractContentData(contentElement string) (quantity int, colorName string) {
	data := strings.Split(contentElement, " ")
	quantity = r.getInt(data[0])
	colorName = fmt.Sprintf("%s %s", data[1], data[2])
	return
}

func (r *Rule) getInt(intAsString string) int {
	value, _ := strconv.Atoi(intAsString)
	return value
}
