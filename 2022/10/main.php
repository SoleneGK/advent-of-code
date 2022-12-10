<?php

class CPU
{
    private const INTERESTING_CYCLES = [20, 60, 100, 140, 180, 220];

    private int $clock = 0;
    private int $register = 1;
    private array $interestingSignalStrengths = [];

    public function getInterestingSignalStrengths(): array
    {
        return $this->interestingSignalStrengths;
    }

    public function getSignalStrength(): int
    {
        return $this->clock * $this->register;
    }

    public function incrementClock(): void
    {
        ++$this->clock;

        if (in_array($this->clock, self::INTERESTING_CYCLES)) {
            $this->interestingSignalStrengths[$this->clock] = $this->getSignalStrength();
        }
    }

    public function executeInstruction(string $instruction): void
    {
        $instruction = trim($instruction);

        if ('noop' === $instruction) {
            $this->incrementClock();
            return;
        }

        $instructionData = explode(' ', $instruction);
        $this->incrementClock();
        $this->incrementClock();
        $this->register += (int) $instructionData[1];
    }
}

$instructionList = fopen('input.txt', 'rb');

$cpu = new CPU();

while (false !== $instruction = fgets($instructionList)) {
    $cpu->executeInstruction($instruction);
}

$answerPart1 = array_sum($cpu->getInterestingSignalStrengths());

echo "The answer to part 1 is $answerPart1\n";