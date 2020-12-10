package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"sort"
	"strconv"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	adapterList := AdapterList{}
	adapterList.GetInput(file)
	numberOfDifferences := adapterList.CountDifferences()
	answer := numberOfDifferences[1] * numberOfDifferences[3]
	fmt.Printf("There are %d 1-jolt differences and %d 3-jolt differences, the product is %d\n", numberOfDifferences[1], numberOfDifferences[3], answer)
}

type AdapterList struct {
	adapters []int
}

func (a *AdapterList) GetInput(file io.Reader) {
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		value, _ := strconv.Atoi(scanner.Text())
		a.adapters = append(a.adapters, value)
	}

	a.addStartValue()
	sort.Ints(a.adapters)
	a.addEndValue()
	fmt.Printf("%v\n", a.adapters)
}

func (a *AdapterList) addStartValue() {
	a.adapters = append(a.adapters, 0)
}

func (a *AdapterList) addEndValue() {
	endValue := a.adapters[len(a.adapters)-1] + 3
	a.adapters = append(a.adapters, endValue)
}

// Output: map length of difference => number of occurrences
func (a *AdapterList) CountDifferences() map[int]int {
	numberOfDifferences := map[int]int{}

	for i := 0; i < len(a.adapters)-1; i++ {
		difference := a.adapters[i+1] - a.adapters[i]
		numberOfDifferences[difference]++
	}
	return numberOfDifferences
}
