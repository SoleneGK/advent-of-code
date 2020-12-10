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
	answerPart1 := numberOfDifferences[1] * numberOfDifferences[3]
	fmt.Printf("There are %d 1-jolt differences and %d 3-jolt differences, the product is %d\n", numberOfDifferences[1], numberOfDifferences[3], answerPart1)

	answerPart2 := adapterList.GetCombinaisonNumber()
	fmt.Printf("There are %d possible combinations\n", answerPart2)
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

/* Algo detail:
 * I starting with the end: i have 1 possibility for the last adapter
 * Then i go back: for each adapter, i take the higher adapters it is compatible with
 * The combination number of this adapter is the sum of the combinations of
 * the higher adapters
 * To get the total number of combination, i take the value for the lowest adapter
 */
func (a *AdapterList) GetCombinaisonNumber() int {
	// For each adapter, store the number of possible combination starting with it
	combinationNumber := a.initializeCombinationNumberMap()

	for i := len(a.adapters) - 2; i >= 0; i-- {
		currentAdapterRating := a.adapters[i]

		compatibleHigherAdapters := a.getCompatibleHigherAdapters(i)

		for _, higherAdapter := range compatibleHigherAdapters {
			combinationNumber[currentAdapterRating] += combinationNumber[higherAdapter]
		}
	}

	return combinationNumber[0]
}

func (a *AdapterList) initializeCombinationNumberMap() map[int]int {
	newMap := map[int]int{}

	for _, adapter := range a.adapters {
		newMap[adapter] = 0
	}

	// Value to higest adapter rating starts at 1
	newMap[a.adapters[len(a.adapters)-1]] = 1

	return newMap
}

func (a *AdapterList) getCompatibleHigherAdapters(index int) (adapterRatings []int) {
	maxRating := a.adapters[index] + 3

	for i := index + 1; i < len(a.adapters); i++ {
		if a.adapters[i] <= maxRating {
			adapterRatings = append(adapterRatings, a.adapters[i])
		} else {
			break
		}
	}

	return
}
