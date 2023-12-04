<?php

declare(strict_types=1);

class Card
{
    public array $winningNumbers;
    public readonly array $numbersYouHave;
    public readonly int $score;

    public function __construct(string $line)
    {
        $this->initNumbers($line);
        $this->calculateScore();
    }

    private function initNumbers(string $line): void
    {
        [$temp, $numberList] = explode(':', $line);
        unset($temp);

        [$winningNumbers, $numbersYouHave] = explode('|', trim($numberList));

        $numbersYouHave = str_replace('  ', ' ', trim($numbersYouHave));
        $this->numbersYouHave = array_map('intval', explode(' ', $numbersYouHave));

        $this->initWinningNumbers($winningNumbers);
    }

    private function initWinningNumbers(string $rawWinningNumbers): void
    {
        $winningNumbers = str_replace('  ', ' ', trim($rawWinningNumbers));
        $winningNumbers = array_map('intval', explode(' ', $winningNumbers));

        foreach ($winningNumbers as $winningNumber) {
            $this->winningNumbers[$winningNumber] = false;
        }
    }

    private function calculateScore(): void
    {
        $numberOfMatches = 0;

        foreach ($this->numbersYouHave as $number) {
            if (isset($this->winningNumbers[$number])) {
                $this->winningNumbers[$number] = true;
                $numberOfMatches++;
            }
        }

        $this->score = 0 === $numberOfMatches ? 0 : 2 ** ($numberOfMatches - 1);
    }
}