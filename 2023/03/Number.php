<?php

declare(strict_types=1);

readonly class Number
{
    public function __construct(
        public int $value,
        public int $x,
        public int $minY,
        public int $maxY
    ) {
    }

    public function isPartNumber(array $grid): bool
    {
        foreach ($this->getNeighbors($grid) as $neighbor) {
            if (is_numeric($grid[$neighbor[0]][$neighbor[1]])) {
                continue;
            }

            if ('.' === $grid[$neighbor[0]][$neighbor[1]]) {
                continue;
            }

            return true;
        }

        return false;
    }

    public function getNeighbors(array $grid): array
    {
        $maxX = count($grid) - 1;
        $maxY = count($grid[0]) - 1;

        $neighbors = [];

        for ($y = $this->minY - 1; $y <= $this->maxY + 1; $y++) {
            if ($this->x - 1 >= 0 && $y >= 0 && $y <= $maxY) {
                $neighbors[] = [$this->x - 1, $y];
            }

            if ($this->x + 1 <= $maxX && $y >= 0 && $y <= $maxY) {
                $neighbors[] = [$this->x + 1, $y];
            }
        }

        if ($this->minY > 0) {
            $neighbors[] = [$this->x, $this->minY - 1];
        }

        if ($this->maxY < $maxY) {
            $neighbors[] = [$this->x, $this->maxY + 1];
        }

        return $neighbors;
    }

    public function __toString(): string
    {
        return $this->value . ';' . $this->x . ';' . $this->minY. ';' . $this->maxY;
    }
}