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
     * I've check the possible movements for the tail, based on its
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
     */
    public function follow(Knot $knot): void
    {
        if ($this->col === $knot->col - 2) {
            $this->col = $knot->col - 1;
            $this->row = $knot->row;
        } else if ($this->col === $knot->col + 2) {
            $this->col = $knot->col + 1;
            $this->row = $knot->row;
        } else if ($this->row === $knot->row - 2) {
            $this->row = $knot->row - 1;
            $this->col = $knot->col;
        } else if ($this->row === $knot->row + 2) {
            $this->row = $knot->row + 1;
            $this->col = $knot->col;
        }

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

$head = new Knot();
$tail = new Knot();
$head->setNextKnot($tail);

$positionsVisitedPart1 = [];

while (false !== $instruction = fgets($instructionList)) {
    [$direction, $numberOfSteps] = explode(' ', $instruction);

    for ($i = 0; $i < $numberOfSteps; ++$i) {
        $head->move($direction);

        $positionsVisitedPart1[$tail->getRow()][$tail->getCol()] = true;
    }
}

echo 'The answer for part 1 is '.countArray($positionsVisitedPart1)."\n";

