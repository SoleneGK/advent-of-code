package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"sort"
	"strconv"
)

var data []int
var goal = 2020

func main() {
	// Get values from file
	// First, opening file
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	// Reading all lines and adding values to data slice
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		data = append(data, getInt(scanner.Text()))
	}

	// And now it's time to solve the problem
	// First, the slice is sorted
	sort.Ints(data)

	// And next, looking for the solution
	value1, value2, _ := getTwoValuesThatSumUpTo(data, goal)
	fmt.Printf("The two values are %d and %d and their product is %d\n", value1, value2, value1*value2)

	value1, value2, value3 := getThreeValuesThatSumUpToTotal(data, goal)
	fmt.Printf("The three values are %d, %d and %d and their product is %d\n", value1, value2, value3, value1*value2*value3)
}

func getInt(input string) int {
	value, _ := strconv.Atoi(input)
	return value
}

/* Basically, i'm taking each value and searching for its complement
 * I use two pointers to go through the slice only once
 * I take the lower untested value and calculate the complement
 * Then i compare the complement to the highest value possible
 * If it's equal, i have my solution
 * If the high value is higher than the complement, then i discard the high value
 * Since i have eliminated all lower values before, there is no more value that
 * could be the complement to this high value
 * If the high value is lower than the complement, then i discard the low value
 * Same idea, since i have eliminated all higher values before, there is no more
 * value that could be the complement to this low value
 * If the pointers have the same value at some point, it means there is no
 * combination of int that solve the problem
 */
func getTwoValuesThatSumUpTo(data []int, total int) (int, int, error) {
	startPointer := 0
	endPointer := len(data) - 1

	for startPointer < endPointer {
		complement := total - data[startPointer]

		if complement == data[endPointer] {
			return data[startPointer], data[endPointer], nil
		} else if complement < data[endPointer] {
			endPointer--
		} else {
			startPointer++
		}
	}

	return 0, 0, errors.New("")
}

/* Now we're looking for three values
 * For each value, i calculate the complement
 * Then i search with the previous function for a couple of numbers whose sum
 * is equal to the complement
 */
func getThreeValuesThatSumUpToTotal(data []int, total int) (int, int, int) {
	pointer := 0

	for pointer < len(data) {
		value1 := data[pointer]
		complementToFind := total - value1

		value2, value3, error := getTwoValuesThatSumUpTo(data, complementToFind)

		if error == nil {
			return value1, value2, value3
		} else {
			pointer++
		}
	}

	return 0, 0, 0
}
