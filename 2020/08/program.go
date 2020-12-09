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

func (p *Program) GetAccumulatorValueWithCorrectedProgram() int {
	for i := 0; i < len(p.InstructionList); i++ {
		success := p.TestCorrection(i)

		if success {
			return p.Accumulator
		}

		p.Reset()
	}

	return 0
}

func (p *Program) TestCorrection(instructionNumber int) (success bool) {
	modifiedInstruction := p.InstructionList[instructionNumber]

	if modifiedInstruction.Type == "acc" {
		return false
	}

	modifiedInstruction.SwitchType()
	defer modifiedInstruction.SwitchType()

	return !p.HasALoop()
}

func (p *Program) HasALoop() bool {
	for p.Pointer < len(p.InstructionList) {
		currentInstruction := p.InstructionList[p.Pointer]

		if !currentInstruction.HasBeenExecuted {
			p.ExecuteInstruction(currentInstruction)
		} else {
			return true
		}
	}

	return false
}

func (p *Program) Reset() {
	p.Accumulator = 0
	p.Pointer = 0

	for _, instruction := range p.InstructionList {
		instruction.HasBeenExecuted = false
	}
}
