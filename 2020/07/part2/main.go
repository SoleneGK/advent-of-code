package main

import (
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	colorRules := ColorRules{}
	colorRules.AddRuleSet(file)
	answerPart2 := colorRules.GetTotalNumberOfBagsContainedForColor("shiny gold")
	fmt.Printf("A shiny gold bag contain %d bags\n", answerPart2)
}
