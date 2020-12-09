package main

import (
	"strconv"
	"strings"
)

func NewInstruction(rawInstruction string) *Instruction {
	instruction := &Instruction{}
	instruction.Analyze(rawInstruction)
	return instruction
}

type Instruction struct {
	Type            string
	Value           int
	HasBeenExecuted bool
}

func (i *Instruction) Analyze(rawInstruction string) {
	data := strings.Split(rawInstruction, " ")

	i.Type = data[0]
	i.Value = i.getInt(data[1])
}

func (i *Instruction) getInt(intAsString string) int {
	value, _ := strconv.Atoi(intAsString)
	return value
}

func (i *Instruction) SwitchType() {
	switch i.Type {
	case "nop":
		i.Type = "jmp"
	case "jmp":
		i.Type = "nop"
	}
}
