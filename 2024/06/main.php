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

$mappedPositions = [$guardPosition];

while (true) {
    move($guardPosition, $obstacleList, $vector);

    // is the guard out of the map?
    if (is_out_of_map($guardPosition, $xMax, $yMax)) {
        break;
    }

    // add current position to mapped positions
    add_position($guardPosition, $mappedPositions);
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
        turn_right($vector);

        return;
    }

    // no: move
    $guardPosition = $nextPosition;
}

function turn_right(array &$vector): void
{
    if (-1 === $vector['x']) {
        $vector['x'] = 0;
        $vector['y'] = 1;

        return;
    }

    if (1 === $vector['y']) {
        $vector['x'] = 1;
        $vector['y'] = 0;

        return;
    }

    if (1 === $vector['x']) {
        $vector['x'] = 0;
        $vector['y'] = -1;

        return;
    }

    $vector['x'] = -1;
    $vector['y'] = 0;
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

function add_position(array $guardPosition, array &$mappedPositions): void
{
    if (\in_array($guardPosition, $mappedPositions, true)) {
        return;
    }

    $mappedPositions[] = $guardPosition;
}

echo 'There are ' . count($mappedPositions) . " mapped positions\n";

function display(int $xMax, int $yMax, array $obstaclePositions, array $mappedPositions): void
{
    for ($x = 0; $x < $xMax; $x++) {
        for ($y = 0; $y < $yMax; $y++) {
            $coordinates = ['x' => $x, 'y' => $y];

            if (\in_array($coordinates, $obstaclePositions, true)) {
                echo '#';

                continue;
            }

            if (\in_array($coordinates, $mappedPositions, true)) {
                echo 'X';

                continue;
            }

            echo '.';
        }

        echo "\n";
    }

    echo "\n";
}

