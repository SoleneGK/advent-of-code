<?php

class Stack
{
    private array $stack = [];

    public function __construct(array $initialCrateLoad = [])
    {
        foreach ($initialCrateLoad as $crate) {
            $this->add($crate);
        }
    }

    public function add(string $crate): void
    {
        array_unshift($this->stack, $crate);
    }

    public function remove(): string
    {
        return array_shift($this->stack);
    }

    public function moveTo(Stack $stack, int $quantity): void
    {
        for ($i = 0; $i < $quantity; ++$i) {
            $crate = $this->remove();
            $stack->add($crate);
        }
    }

    public function getTopCrate(): string
    {
        return $this->stack[0];
    }

    public function display(): void
    {
        foreach ($this->stack as $crate) {
            echo "$crate\n";
        }

        echo "\n";
    }
}

// Example
/* $stackList = [
    1 => new Stack(['Z', 'N']),
    2 => new Stack(['M', 'C', 'D']),
    3 => new Stack(['P']),
];*/

// Input
$stackList = [
    1 => new Stack(['Q', 'F', 'M', 'R', 'L', 'W', 'C', 'V']),
    2 => new Stack(['D', 'Q', 'L']),
    3 => new Stack(['P', 'S', 'R', 'G', 'W', 'C', 'N', 'B']),
    4 => new Stack(['L', 'C', 'D', 'H', 'B', 'Q', 'G']),
    5 => new Stack(['V', 'G', 'L', 'F', 'Z', 'S']),
    6 => new Stack(['D', 'G', 'N', 'P']),
    7 => new Stack(['D', 'Z', 'P', 'V', 'F', 'C', 'W']),
    8 => new Stack(['C', 'P', 'D', 'M', 'S']),
    9 => new Stack(['Z', 'N', 'W', 'T', 'V', 'M', 'P', 'C']),
];

foreach ($stackList as $stack) {
    $stack->display();
}


$instructionList = fopen('input.txt', 'rb');

while (false !== $instruction = fgets($instructionList)) {
    /**
     * 0 -> 'move'
     * 1 -> crate number
     * 2 -> 'from'
     * 3 -> from stack
     * 4 -> 'to'
     * 5 -> to stack
     */
    $instructionData = explode(' ', $instruction);

    $quantity = $instructionData[1];
    $from = (int) $instructionData[3];
    $to = (int) $instructionData[5];

    $stackList[$from]->moveTo($stackList[$to], $quantity);
}

echo 'At the end, the top crates are ';

foreach ($stackList as $stack) {
    echo $stack->getTopCrate();
}

echo "\n";