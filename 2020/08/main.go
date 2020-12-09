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

	program := Program{}
	program.AddInstructionSet(file)
	value := program.GetAccumulatorValueBeforeLoop()

	fmt.Printf("Last accumulator value before loop is %d\n", value)
}
