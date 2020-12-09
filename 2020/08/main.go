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

	valuePart1 := program.GetAccumulatorValueBeforeLoop()
	fmt.Printf("Last accumulator value before loop is %d\n", valuePart1)
	program.Reset()

	valuePart2 := program.GetAccumulatorValueWithCorrectedProgram()
	fmt.Printf("End accumulator value in corrected program is %d\n", valuePart2)
}
