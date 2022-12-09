<?php

$instructionList = fopen('input.txt', 'rb');

class Knot
{
    private int $row = 0;
    private int $col = 0;
    private ?Knot $nextKnot = null;

    public function getRow(): int
    {
        return $this->row;
    }

    public function getCol(): int
    {
        return $this->col;
    }

    public function setNextKnot(Knot $nextKnot): void
    {
        $this->nextKnot = $nextKnot;
    }

    public function move(string $direction): void
    {
        switch ($direction) {
            case 'U':
                ++$this->row;
                break;
            case 'D':
                --$this->row;
                break;
            case 'L':
                --$this->col;
                break;
            case 'R':
                ++$this->col;
                break;
        }

        $this->nextKnot?->follow($this);
    }

    /**
     * I've checked the possible movements for the tail, based on its
     * relative position to the head and identified a pattern:
     * I don't have to check for all possible tail positions, only
     * if there is a 2-distance in the col or row direction
     * It is enough to deduce the new tail position
     *
     * For example, if the tail is 2 cols lower than the head, it
     * will move to be just in top of the head, whichever row it
     * was in before
     *
     * T..    ...
     * ... -> .T.
     * .H.    .H.
     *
     * .T.    ...
     * ... -> .T.
     * .H.    .H.
     *
     * ..T    ...
     * ... -> .T.
     * .H.    .H.
     *
     * Head = $knot
     * Tail = $this
     *
     * Part 2: damn, I have to add some cases
     */
    public function follow(Knot $knot): void
    {
        $offset = [
            'row' => $this->row - $knot->getRow(),
            'col' => $this->col - $knot->getCol(),
        ];

        $newOffset = match ($offset) {
            ['row' => -2, 'col' => -2] => ['row' => -1, 'col' => -1],
            ['row' => 2, 'col' => -2] => ['row' => 1, 'col' => -1],
            ['row' => 2, 'col' => 2] => ['row' => 1, 'col' => 1],
            ['row' => -2, 'col' => 2] => ['row' => -1, 'col' => 1],
            ['row' => -2, 'col' => -1],
                ['row' => -2, 'col' => 0],
                ['row' => -2, 'col' => 1]
                => ['row' => -1, 'col' => 0],
            ['row' => 2, 'col' => -1],
                ['row' => 2, 'col' => 0],
                ['row' => 2, 'col' => 1]
                => ['row' => 1, 'col' => 0],
            ['row' => -1, 'col' => -2],
                ['row' => 0, 'col' => -2],
                ['row' => 1, 'col' => -2]
                => ['row' => 0, 'col' => -1],
            ['row' => -1, 'col' => 2],
                ['row' => 0, 'col' => 2],
                ['row' => 1, 'col' => 2]
                => ['row' => 0, 'col' => 1],
            default => $offset,
        };

        $this->row = $knot->getRow() + $newOffset['row'];
        $this->col = $knot->getCol() + $newOffset['col'];

        $this->nextKnot?->follow($this);
    }
}

function countArray(array $array): int
{
    $count = 0;

    foreach ($array as $line) {
        foreach ($line as $item) {
            ++$count;
        }
    }

    return $count;
}

// Set up part 1
$headPart1 = new Knot();
$tailPart1 = new Knot();
$headPart1->setNextKnot($tailPart1);

$positionsVisitedPart1[0][0] = true;

// Set up part 2
$headPart2 = new Knot();
$tailPart2 = $headPart2;

for ($i = 1; $i <= 9; $i++) {
    $newKnot = new Knot();
    $tailPart2->setNextKnot($newKnot);
    $tailPart2 = $newKnot;
}

$positionsVisitedPart2[0][0] = true;

while (false !== $instruction = fgets($instructionList)) {
    [$direction, $numberOfSteps] = explode(' ', $instruction);

    for ($i = 0; $i < $numberOfSteps; ++$i) {
        $headPart1->move($direction);
        $positionsVisitedPart1[$tailPart1->getRow()][$tailPart1->getCol()] = true;

        $headPart2->move($direction);
        $positionsVisitedPart2[$tailPart2->getRow()][$tailPart2->getCol()] = true;
    }
}

echo 'The answer for part 1 is '.countArray($positionsVisitedPart1)."\n";
echo 'The answer for part 2 is '.countArray($positionsVisitedPart2)."\n";

