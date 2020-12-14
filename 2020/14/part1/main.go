package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"os"
	"strconv"
	"strings"
	"unicode/utf8"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	memory := Memory{}
	memory.ProcessData(file)

	answerPart1 := memory.GetValueSum()
	fmt.Printf("The answer for part 1 is %d\n", answerPart1)
}

type Memory map[int]int

func (m Memory) ProcessData(data io.Reader) {
	mask := map[int]int{}
	scanner := bufio.NewScanner(data)

	for scanner.Scan() {
		if scanner.Text()[:4] == "mask" {
			mask = getNewMask(scanner.Text())
		} else {
			m.setMemoryValue(scanner.Text(), mask)
		}
	}
}

var (
	one, _  = utf8.DecodeRuneInString("1")
	zero, _ = utf8.DecodeRuneInString("0")
)

func getNewMask(maskData string) map[int]int {
	newMask := map[int]int{}

	bitList := strings.TrimPrefix(maskData, "mask = ")

	for index, char := range []byte(bitList) {
		if rune(char) == one {
			newMask[index] = 1
		} else if rune(char) == zero {
			newMask[index] = 0
		}
	}

	return newMask
}

func (m Memory) setMemoryValue(input string, mask map[int]int) {
	index, decimalValue := extractIndexAndValue(input)
	m.setValue(index, decimalValue)
	m.applyMask(index, mask)
}

func extractIndexAndValue(input string) (index, value int) {
	parts := strings.Split(input, " ")

	indexAsBytes := parts[0][4 : len(parts[0])-1]
	index, _ = strconv.Atoi(indexAsBytes)

	value, _ = strconv.Atoi(parts[2])

	return
}

func (m Memory) setValue(index, value int) {
	m[index] = value
}

func (m Memory) applyMask(index int, mask map[int]int) {
	binaryValue := getBinaryRepresentation(m[index])
	binaryValue.applyMask(mask)
	m.setValue(index, binaryValue.getDecimalValue())
}

func getBinaryRepresentation(decimalValue int) (binary Binary) {
	currentValue := decimalValue

	for i := 0; i < binarySize && currentValue > 0; i++ {
		binaryDigitValue := powInt(2, binarySize-1-i)

		if currentValue >= binaryDigitValue {
			binary[i] = true
			currentValue -= binaryDigitValue
		}
	}

	return
}

const binarySize = 36

type Binary [binarySize]bool

func (b *Binary) applyMask(mask map[int]int) {
	for index, value := range mask {
		if value == 1 {
			b[index] = true
		} else {
			b[index] = false
		}
	}
}

func (b Binary) getDecimalValue() (value int) {
	for i := 0; i < binarySize; i++ {
		if b[i] {
			value += powInt(2, binarySize-1-i)
		}
	}

	return
}

func powInt(number, exponant int) int {
	return int(math.Pow(float64(number), float64(exponant)))
}

func (m Memory) GetValueSum() (sum int) {
	for _, value := range m {
		sum += value
	}

	return
}
