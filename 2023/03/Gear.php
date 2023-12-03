<?php

declare(strict_types=1);

class Gear
{
    public array $adjacentNumberList = [];

    public function __construct(
        public readonly int $x,
        public readonly int $y
    ) {
    }

    public function getAdjacentNumbers(array $datagrid): void
    {
        $adjacentNumberList = [];

        foreach ($this->getNeighbours($datagrid) as $neighbour) {
            if (null !== $neighbour) {
                $adjacentNumberList[] = $neighbour;
            }
        }

        $this->adjacentNumberList = array_unique($adjacentNumberList);
    }

    public function isValidGear(array $datagrid): bool
    {
        return 2 === count($this->adjacentNumberList);
    }

    public function getNeighbours(array $datagrid): array
    {
        $maxX = count($datagrid) - 1;
        $maxY = count($datagrid[0]) - 1;

        $neighbors = [];

        for ($x = $this->x - 1; $x <= $this->x + 1; $x++) {
            for ($y = $this->y - 1; $y <= $this->y + 1; $y++) {
                if ($x < 0 || $x > $maxX || $y < 0 || $y > $maxY) {
                    continue;
                }

                if ($x === $this->x && $y === $this->y) {
                    continue;
                }

                $neighbors[] = $datagrid[$x][$y];
            }
        }

        return $neighbors;
    }

    public function getGearRatio(): int
    {
        $ratio = 1;

        foreach ($this->adjacentNumberList as $adjacentNumber) {
            $ratio *= $adjacentNumber->value;
        }

        return $ratio;
    }
}