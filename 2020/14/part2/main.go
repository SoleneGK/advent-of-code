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

	answerPart2 := memory.GetValueSum()
	fmt.Printf("The answer for part 2 is %d\n", answerPart2)
}

type Memory map[int]int

func (m Memory) ProcessData(data io.Reader) {
	mask := map[int]rune{}
	scanner := bufio.NewScanner(data)

	for scanner.Scan() {
		if scanner.Text()[:4] == "mask" {
			mask = getNewMask(scanner.Text())
		} else {
			m.setMemoryValues(scanner.Text(), mask)
		}
	}
}

func getNewMask(maskData string) map[int]rune {
	newMask := map[int]rune{}

	bitList := strings.TrimPrefix(maskData, "mask = ")

	for index, char := range []byte(bitList) {
		newMask[index] = rune(char)
	}

	return newMask
}

func (m Memory) setMemoryValues(input string, mask map[int]rune) {
	baseAddress, value := extractIndexAndValue(input)

	addressList := getAddressList(baseAddress, mask)

	for _, address := range addressList {
		m.setValue(address, value)
	}
}

func extractIndexAndValue(input string) (index, value int) {
	parts := strings.Split(input, " ")

	indexAsBytes := parts[0][4 : len(parts[0])-1]
	index, _ = strconv.Atoi(indexAsBytes)

	value, _ = strconv.Atoi(parts[2])

	return
}

var (
	one, _      = utf8.DecodeRuneInString("1")
	floating, _ = utf8.DecodeRuneInString("X")
)

func getAddressList(baseAddress int, mask map[int]rune) (addressList []int) {
	baseAddressAsBinary := getBinaryRepresentation(baseAddress)
	floatingBitList := []int{}

	for index, value := range mask {
		if value == one {
			baseAddressAsBinary[index] = true
		} else if value == floating {
			floatingBitList = append(floatingBitList, index)
		}
	}

	addressList = applyFloating(&baseAddressAsBinary, floatingBitList)

	return
}

func applyFloating(baseAddress *Binary, floatingBitList []int) (addressList []int) {
	bitStates := map[int]bool{}

	// initialization
	for _, value := range floatingBitList {
		bitStates[value] = false
	}

	combinaisonNumber := powTwo(len(floatingBitList))

	// génération des adresses
	for i := 0; i < combinaisonNumber; i++ {
		baseAddress.applyMask(bitStates)
		addressList = append(addressList, baseAddress.getDecimalValue())
		increment(bitStates, floatingBitList)
	}

	return
}

func increment(boolMap map[int]bool, indexList []int) {
	carry := true

	for i := len(indexList) - 1; i >= 0 && carry; i-- {
		index := indexList[i]

		carry = boolMap[index]
		boolMap[index] = !boolMap[index]
	}
}

func (m Memory) setValue(index, value int) {
	m[index] = value
}

const binarySize = 36

type Binary [binarySize]bool

func (b Binary) getDecimalValue() (value int) {
	for i := 0; i < binarySize; i++ {
		if b[i] {
			value += powTwo(binarySize - 1 - i)
		}
	}

	return
}

func (b *Binary) applyMask(mask map[int]bool) {
	for index, value := range mask {
		b[index] = value
	}
}

func getBinaryRepresentation(decimalValue int) (binary Binary) {
	currentValue := decimalValue

	for i := 0; i < binarySize && currentValue > 0; i++ {
		binaryDigitValue := powTwo(binarySize - 1 - i)

		if currentValue >= binaryDigitValue {
			binary[i] = true
			currentValue -= binaryDigitValue
		}
	}

	return
}

func powTwo(exponant int) int {
	return int(math.Pow(float64(2), float64(exponant)))
}

func (m Memory) GetValueSum() (sum int) {
	for _, value := range m {
		sum += value
	}

	return
}
