package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	colorRules := ColorRules{}
	colorRules.AddRuleSet(file)
	answerPart1 := colorRules.GetNumberOfPossibleContainersOfColor("shiny gold")
	fmt.Printf("A shiny gold bag can be contained in %d different bags\n", answerPart1)
}

func getContainerColorFromRule(rule string) string {
	return strings.Split(string(rule), " bags contain")[0]
}

func ruleContainsOtherBags(rule string) bool {
	return !strings.Contains(rule, "no other bags")
}

func getContainedColorsFromRule(rule string) (colorList []string) {
	colorListAsString := strings.Split(rule, "bags contain ")[1]
	colorListAsStringWithoutFinalPoint := strings.TrimSuffix(colorListAsString, ".")
	rawColorList := strings.Split(colorListAsStringWithoutFinalPoint, ", ")

	for _, colorLine := range rawColorList {
		elements := strings.Split(colorLine, " ")
		color := fmt.Sprintf("%v %v", elements[1], elements[2])

		colorList = append(colorList, color)
	}

	return colorList
}
