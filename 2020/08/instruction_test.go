package main

import (
	"reflect"
	"testing"
)

func TestAnalyze(t *testing.T) {
	testTable := []struct {
		name              string
		instructionString string
		want              Instruction
	}{
		{
			name:              "nop instruction",
			instructionString: "nop +0",
			want: Instruction{
				Type:  "nop",
				Value: 0,
			},
		},
		{
			name:              "acc instruction with positive value",
			instructionString: "acc +3",
			want: Instruction{
				Type:  "acc",
				Value: 3,
			},
		},
		{
			name:              "jmp value with negative value",
			instructionString: "jmp -4",
			want: Instruction{
				Type:  "jmp",
				Value: -4,
			},
		},
	}

	for _, tt := range testTable {
		t.Run(tt.name, func(t *testing.T) {
			instruction := Instruction{}
			instruction.Analyze(tt.instructionString)

			if !reflect.DeepEqual(instruction, tt.want) {
				t.Errorf("Incorrect instruction: got %v, want %v", instruction, tt.want)
			}
		})
	}
}
