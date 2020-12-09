package main

import (
	"bufio"
	"io"
)

type Program struct {
	Accumulator     int
	Pointer         int
	InstructionList []*Instruction
}

func (p *Program) ExecuteInstruction(instruction *Instruction) {
	switch instruction.Type {
	case "nop":
		p.Pointer++
	case "acc":
		p.Pointer++
		p.Accumulator += instruction.Value
	case "jmp":
		p.Pointer += instruction.Value
	}
	instruction.HasBeenExecuted = true
}

func (p *Program) GetAccumulatorValueBeforeLoop() int {
	currentInstruction := p.InstructionList[p.Pointer]

	for !currentInstruction.HasBeenExecuted {
		p.ExecuteInstruction(currentInstruction)
		currentInstruction = p.InstructionList[p.Pointer]
	}

	return p.Accumulator
}

func (p *Program) AddInstruction(instructionAsString string) {
	p.InstructionList = append(p.InstructionList, NewInstruction(instructionAsString))
}

func (p *Program) AddInstructionSet(instructionSet io.Reader) {
	scanner := bufio.NewScanner(instructionSet)

	for scanner.Scan() {
		p.AddInstruction(scanner.Text())
	}
}
