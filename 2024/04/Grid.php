<?php

declare(strict_types=1);

class Grid
{
    protected readonly int $xMax;
    protected readonly int $yMax;
    protected bool $initialized = false;

    public function __construct(
        protected array $grid = []
    ) {
    }

    public function addLine(array $line): void
    {
        $this->grid[] = $line;
    }

    public function getNumberOfXmas(): int
    {
        $this->computeGridDimensions();

        $numberOfApparitions = 0;

        for ($x = 0; $x < $this->xMax; $x++) {
            for ($y = 0; $y < $this->yMax; $y++) {
                if ('X' !== $this->grid[$x][$y]) {
                    continue;
                }

                $possibleWordList = $this->getPossibleWordList($x, $y);

                foreach ($possibleWordList as $possibleWord) {
                    if (count($possibleWord) < 3) {
                        continue;
                    }

                    if (
                        $possibleWord[0] === 'M'
                        && $possibleWord[1] === 'A'
                        && $possibleWord[2] === 'S'
                    ) {
                        $numberOfApparitions++;
                    }
                }
            }
        }

        return $numberOfApparitions;
    }

    protected function computeGridDimensions(): void
    {
        if ($this->initialized) {
            return;
        }

        $this->xMax = count($this->grid);
        $this->yMax = count($this->grid[0]);
        $this->initialized = true;
    }

    protected function getPossibleWordList(int $x, int $y): array
    {
        $possibleWordList = [];

        $vectorList = [
            ['x' => 1, 'y' => 0],
            ['x' => 1, 'y' => 1],
            ['x' => 0, 'y' => 1],
            ['x' => -1, 'y' => 1],
            ['x' => -1, 'y' => 0],
            ['x' => -1, 'y' => -1],
            ['x' => 0, 'y' => -1],
            ['x' => 1, 'y' => -1],
        ];

        foreach ($vectorList as $vector) {
            $possibleWordList[] = $this->getWord($x, $y, $vector);
        }

        return $possibleWordList;
    }

    protected function getWord(int $x, int $y, array $vector): array
    {
        $word = [];

        for ($i = 1; $i <= 3; $i++) {
            $x1 = $x + $i * $vector['x'];
            $y1 = $y + $i * $vector['y'];

            // check for out of bounds
            if ($x1 < 0 || $x1 > $this->xMax - 1 || $y1 < 0 || $y1 > $this->yMax - 1) {
                break;
            }

            $word[] = $this->grid[$x1][$y1];
        }

        return $word;
    }
    public function getNumberOfXOfMas(): int
    {
        $this->computeGridDimensions();

        $numberOfApparitions = 0;

        for ($x = 0; $x < $this->xMax; $x++) {
            for ($y = 0; $y < $this->yMax; $y++) {
                if ('A' !== $this->grid[$x][$y]) {
                    continue;
                }

                if ($this->isXOfMas($x, $y)) {
                    $numberOfApparitions++;
                }
            }
        }

        return $numberOfApparitions;
    }

    protected function isXOfMas(int $x, int $y): bool
    {
        if ($x <= 0 || $x >= $this->xMax -1 || $y <= 0 || $y >= $this->yMax - 1) {
            return false;
        }

        $cornerTL = $this->grid[$x-1][$y-1];
        $cornerTR = $this->grid[$x-1][$y+1];
        $cornerBL = $this->grid[$x+1][$y-1];
        $cornerBR = $this->grid[$x+1][$y+1];

        $target = ['M', 'S'];

        $firstDiagonal = [$cornerTL, $cornerBR];
        sort($firstDiagonal);

        if ($target !== $firstDiagonal) {
            return false;
        }

        $secondDiagonal = [$cornerTR, $cornerBL];
        sort($secondDiagonal);

        return $target === $secondDiagonal;
    }
}
