<?php

declare(strict_types=1);

$obstacleList = [];
$guardPosition = [];
$vector = ['x' => -1, 'y' => 0];
$xMax = 0;
$yMax = 0;

$file = fopen('example.txt', 'rb');

while (false !== $line = fgets($file)) {
    $line = str_split(trim($line));

    if (0 === $yMax) {
        $yMax = count($line);
    }

    foreach ($line as $y => $character) {
        if ('^' === $character) {
            $guardPosition = ['x' => $xMax, 'y' => $y];

            continue;
        }

        if ('#' === $character) {
            $obstacleList[] = ['x' => $xMax, 'y' => $y];
        }
    }

    $xMax++;
}

$mappedPositions = [];
add_position($mappedPositions, $guardPosition, $vector);

$numberOfPossibleObstructions = 0;

while (true) {
    move($guardPosition, $obstacleList, $vector);

    // is the guard out of the map?
    if (is_out_of_map($guardPosition, $xMax, $yMax)) {
        break;
    }

    // add current position to mapped positions
    add_position($mappedPositions, $guardPosition, $vector);
}

echo 'There are ' . count_mapped_positions($mappedPositions) . " mapped positions\n";


function add_position(array &$mappedPositions, array $guardPosition, array $vector): void
{
    $x = $guardPosition['x'];
    $y = $guardPosition['y'];

    // is this position has not already been mapped, it's added with current vector
    if (!isset($mappedPositions[$x][$y])) {
        $mappedPositions[$x][$y] = [$vector];

        return;
    }

    // else, if current vector has not been register for this position, it's added
    if (\in_array($vector, $mappedPositions[$x][$y], true)) {
        return;
    }

    $mappedPositions[$x][$y][] = $vector;
}

function move(array &$guardPosition, array $obstacleList, array &$vector): void
{
    // compute new position
    $nextPosition = [
        'x' => $guardPosition['x'] + $vector['x'],
        'y' => $guardPosition['y'] + $vector['y'],
    ];

    // is there an obstacle?
    $isBlocked = \in_array($nextPosition, $obstacleList, true);

    // yes: change direction
    if ($isBlocked) {
        $vector = turn_right($vector);

        return;
    }

    // no: move
    $guardPosition = $nextPosition;
}

function turn_right(array $vector): array
{
    if (-1 === $vector['x']) {
        return ['x' => 0, 'y' => 1];
    }

    if (1 === $vector['y']) {
        return ['x' => 1, 'y' => 0];
    }

    if (1 === $vector['x']) {
        return ['x' => 0, 'y' => -1];
    }

    return ['x' => -1, 'y' => 0];
}

function is_out_of_map(array $guardPosition, int $xMax, int $yMax): bool
{
    if ($guardPosition['x'] < 0) {
        return true;
    }

    if ($guardPosition['y'] < 0) {
        return true;
    }

    if ($guardPosition['x'] >= $xMax) {
        return true;
    }

    if ($guardPosition['y'] >= $yMax) {
        return true;
    }

    return false;
}

function count_mapped_positions(array $mappedPositions): int
{
    $count = 0;

    foreach ($mappedPositions as $rowData) {
        foreach ($rowData as $y) {
            $count++;
        }
    }

    return $count;
}

function display(int $xMax, int $yMax, array $obstaclePositions, array $mappedPositions): void
{
    for ($x = 0; $x < $xMax; $x++) {
        for ($y = 0; $y < $yMax; $y++) {
            $coordinates = ['x' => $x, 'y' => $y];

            if (\in_array($coordinates, $obstaclePositions, true)) {
                echo '#';

                continue;
            }

            if (isset($mappedPositions[$x][$y])) {
                echo 'X';

                continue;
            }

            echo '.';
        }

        echo "\n";
    }

    echo "\n";
}

