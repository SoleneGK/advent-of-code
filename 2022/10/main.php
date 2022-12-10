<?php

class CPU
{
    private const INTERESTING_CYCLES = [20, 60, 100, 140, 180, 220];

    private int $clock = 0;
    private int $register = 1;
    private array $interestingSignalStrengths = [];

    private CRT $crt;

    public function __construct(CRT $crt)
    {
        $this->crt = $crt;
    }

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
        $this->crt->drawPixel($this->register);

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

class CRT
{
    private const LIT_PIXEL = '#';
    private const DARK_PIXEL = '.';

    private int $currentPixel = 0;
    private array $screen = [];

    private function setNextPixel(): void
    {
        $this->currentPixel = ++$this->currentPixel % 40;
    }

    public function drawPixel(int $spriteMiddlePosition): void
    {
        if (
            $this->currentPixel >= $spriteMiddlePosition - 1
            && $this->currentPixel <= $spriteMiddlePosition + 1
        ) {
            $this->screen[] = self::LIT_PIXEL;
        } else {
            $this->screen[] = self::DARK_PIXEL;
        }

        $this->setNextPixel();
    }

    public function display(): void
    {
        foreach ($this->screen as $key => $pixel) {
            if (0 === $key % 40) {
                echo "\n";
            }

            echo $pixel;
        }

        echo "\n";
    }
}

$instructionList = fopen('input.txt', 'rb');

$crt = new CRT();
$cpu = new CPU($crt);

while (false !== $instruction = fgets($instructionList)) {
    $cpu->executeInstruction($instruction);
}

$answerPart1 = array_sum($cpu->getInterestingSignalStrengths());

echo "The answer to part 1 is $answerPart1\n";

$crt->display();