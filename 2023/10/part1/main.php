<?php

declare(strict_types=1);

require_once 'Point.php';

$file = fopen('input.txt', 'rb');

$grid = [];

while ($line = fgets($file)) {
    $grid[] = str_split(trim($line));
}

$startPoint = get_start_point($grid);

foreach ($grid as $row) {
    foreach ($row as $value) {
        echo $value;
    }

    echo PHP_EOL;
}

$startPointPipe = get_start_point_pipe($grid, $startPoint);
$grid[$startPoint->x][$startPoint->y] = $startPointPipe;
echo $startPointPipe . PHP_EOL;

$loopLength = get_loop_length($grid, $startPoint);

echo 'The number of steps is ' . $loopLength / 2 . PHP_EOL;

function get_start_point(array $grid): Point
{
    foreach ($grid as $x => $row) {
        foreach ($row as $y => $value) {
            if ('S' === $value) {
                return new Point($x, $y);
            }
        }
    }
    throw new InvalidArgumentException('No start point found');
}

function get_start_point_pipe(array $grid, Point $startPoint): string
{
    $connectedPoints = [
        'top'    => false,
        'right'  => false,
        'bottom' => false,
        'left'   => false,
    ];

    $topPoint = new Point($startPoint->x - 1, $startPoint->y);

    if (\in_array($grid[$topPoint->x][$topPoint->y], ['|', 'F', '7'], true)) {
        $connectedPoints['top'] = true;
    }

    $rightPoint = new Point($startPoint->x, $startPoint->y + 1);

    if (\in_array($grid[$rightPoint->x][$rightPoint->y], ['-', 'J', '7'], true)) {
        $connectedPoints['right'] = true;
    }

    $bottomPoint = new Point($startPoint->x + 1, $startPoint->y);

    if (\in_array($grid[$bottomPoint->x][$bottomPoint->y], ['|', 'L', 'J'], true)) {
        $connectedPoints['bottom'] = true;
    }

    $leftPoint = new Point($startPoint->x, $startPoint->y - 1);

    if (\in_array($grid[$leftPoint->x][$leftPoint->y], ['-', 'F', 'L'], true)) {
        $connectedPoints['left'] = true;
    }

    if ($connectedPoints['top'] && $connectedPoints['bottom']) {
        return '|';
    }

    if ($connectedPoints['right'] && $connectedPoints['left']) {
        return '-';
    }

    if ($connectedPoints['top'] && $connectedPoints['right']) {
        return 'L';
    }

    if ($connectedPoints['right'] && $connectedPoints['bottom']) {
        return 'F';
    }

    if ($connectedPoints['bottom'] && $connectedPoints['left']) {
        return '7';
    }

    // left et top
    return 'J';
}

function get_loop_length(array $grid, Point $startPoint): int
{
    $currentPoint = $startPoint;
    $loopLength = 0;

    [$lastPoint, $lastMove] = match ($grid[$currentPoint->x][$currentPoint->y]) {
        '|', 'L', 'J' => [
            new Point($startPoint->x - 1, $startPoint->y),
            [
                'x' => 1,
                'y' => 0,
            ],
        ],
        'F', '7' => [
            new Point($startPoint->x + 1, $startPoint->y),
            [
                'x' => -1,
                'y' => 0,
            ],
        ],
        '-' => [
            new Point($startPoint->x, $startPoint->y - 1),
            [
                'x' => 0,
                'y' => 1,
            ],
        ],
        default => throw new InvalidArgumentException(),
    };

    do {
        ++$loopLength;

        // se déplacer d'une case
        $currentPipe = $grid[$currentPoint->x][$currentPoint->y];

        switch ($currentPipe) {
            case '|':
                $lastMove = [
                    'x' => $currentPoint->x - $lastPoint->x,
                    'y' => 0,
                ];
                break;
            case '-':
                $lastMove = [
                    'x' => 0,
                    'y' => $currentPoint->y - $lastPoint->y,
                ];
                break;
            case '7':
            case 'L':
                $nextMove = [
                    'x' => $lastMove['y'],
                    'y' => $lastMove['x'],
                ];
                $lastMove = $nextMove;
                break;
            case 'F':
            case 'J':
                $nextMove = [
                    'x' => -$lastMove['y'],
                    'y' => -$lastMove['x'],
                ];
                $lastMove = $nextMove;
                break;
        }

        $lastPoint = $currentPoint;
        $currentPoint = new Point($currentPoint->x + $lastMove['x'], $currentPoint->y + $lastMove['y']);

    } while ($currentPoint->x !== $startPoint->x || $currentPoint->y !== $startPoint->y);

    return $loopLength;
}
