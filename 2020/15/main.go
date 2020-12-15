package main

import "fmt"

// The input is so short i see no point to put it in a different file
var input = []int{9, 3, 1, 0, 8, 4}

func main() {
	positionPart1 := 2020
	answerPart1 := getNthValue(input, positionPart1)
	fmt.Printf("The %dth number spoken will be %d\n", positionPart1, answerPart1)
}

func getNthValue(input []int, position int) int {
	// To keep the last index a value appeared
	memory := initializeMemory(input)
	currentList := input
	i := len(input) - 1

	for i < position {
		value := currentList[i]

		lastIndex, alreadySpoken := memory[value]

		if alreadySpoken {
			currentList = append(currentList, i-lastIndex)
		} else {
			currentList = append(currentList, 0)
		}

		memory[value] = i

		i++
	}

	return currentList[position-1]
}

func initializeMemory(input []int) map[int]int {
	memory := map[int]int{}

	for index, value := range input {
		memory[value] = index
	}

	return memory
}
