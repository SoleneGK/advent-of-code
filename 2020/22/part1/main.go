package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	answerPart1 := getAnswerPart1(file)
	fmt.Printf("The answer for part 1 is %d\n", answerPart1)
}

func getAnswerPart1(file io.Reader) int {
	deck1, deck2 := getDecks(file)
	winningDeck := getWinningDeck(deck1, deck2)
	return calculateAnswerPart1(winningDeck)
}

func getDecks(file io.Reader) (deck1, deck2 []int) {
	scanner := bufio.NewScanner(file)

	skipOneLine(scanner)
	deck1 = extractDeck(scanner)
	skipOneLine(scanner)
	deck2 = extractDeck(scanner)

	return
}

func skipOneLine(scanner *bufio.Scanner) {
	scanner.Scan()
}

func extractDeck(scanner *bufio.Scanner) (deck []int) {
	for scanner.Scan() {
		if scanner.Text() == "" {
			break
		}

		deck = append(deck, getInt(scanner.Text()))
	}
	return
}

func getInt(valueAsString string) int {
	value, _ := strconv.Atoi(valueAsString)
	return value
}

func getWinningDeck(deck1, deck2 []int) []int {
	for {
		deck1, deck2 = playNextRound(deck1, deck2)

		if len(deck1) == 0 {
			return deck2
		}

		if len(deck2) == 0 {
			return deck1
		}
	}
}

func playNextRound(deck1, deck2 []int) (newDeck1, newDeck2 []int) {
	if deck1[0] > deck2[0] {
		newDeck1, newDeck2 = moveCards(deck1, deck2)
	} else {
		newDeck1, newDeck2 = moveCards(deck2, deck1)
	}
	return
}

func moveCards(winningDeck, losingDeck []int) (newWinningDeck, newLosingDeck []int) {
	newWinningDeck = append(winningDeck, winningDeck[0], losingDeck[0])
	newWinningDeck = newWinningDeck[1:]
	newLosingDeck = losingDeck[1:]
	return
}

func calculateAnswerPart1(winningDeck []int) (answer int) {
	for i, value := range winningDeck {
		answer += value * (len(winningDeck) - i)
	}
	return
}
