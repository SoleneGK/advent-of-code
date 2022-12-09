<?php

$instructionList = fopen('input.txt', 'rb');

function moveHead(int &$rowHead, int &$colHead, string $direction): void
{
    switch ($direction) {
        case 'U':
            ++$rowHead;
            break;
        case 'D':
            --$rowHead;
            break;
        case 'L':
            --$colHead;
            break;
        case 'R':
            ++$colHead;
            break;
    }
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
 */
function moveTail(
    int $rowHead,
    int $colHead,
    int &$rowTail,
    int &$colTail
): void
{
    if ($colTail === $colHead - 2) {
        $colTail = $colHead - 1;
        $rowTail = $rowHead;

        return;
    }

    if ($colTail === $colHead + 2) {
        $colTail = $colHead + 1;
        $rowTail = $rowHead;

        return;
    }

    if ($rowTail === $rowHead - 2) {
        $rowTail = $rowHead - 1;
        $colTail = $colHead;

        return;
    }

    if ($rowTail === $rowHead + 2) {
        $rowTail = $rowHead + 1;
        $colTail = $colHead;
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

function display(array $array): void
{
    foreach ($array as $row => $line) {
        foreach ($line as $col => $item) {
            echo "$row, $col\n";
        }
    }
}

$rowHead = 0;
$colHead = 0;
$rowTail = 0;
$colTail = 0;

$positionsVisited = [];

while (false !== $instruction = fgets($instructionList)) {
    [$direction, $numberOfSteps] = explode(' ', $instruction);

    for ($i = 0; $i < $numberOfSteps; ++$i) {
        moveHead($rowHead, $colHead, $direction);
        moveTail($rowHead, $colHead, $rowTail, $colTail);

        $positionsVisited[$rowTail][$colTail] = true;
    }
}

echo 'The answer for part 1 is '.countArray($positionsVisited)."\n";

