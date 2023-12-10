<?php

declare(strict_types=1);

require_once 'Point.php';

$file = fopen('input.txt', 'rb');

$grid = [];

while ($line = fgets($file)) {
    $grid[] = str_split(trim($line));
}

$startPoint = get_start_point($grid);
$startPointPipe = get_start_point_pipe($grid, $startPoint);
$grid[$startPoint->x][$startPoint->y] = $startPointPipe;
$cleanGrid = get_grid_without_pipes_out_of_loop($grid, $startPoint);

$numberOfEnclosedTiles = get_number_of_enclosed_tiles($cleanGrid);
echo "The number of enclosed tiles is " . $numberOfEnclosedTiles . PHP_EOL;

function display_grid(array $grid): void
{
    foreach ($grid as $row) {
        foreach ($row as $value) {
            echo $value;
        }

        echo PHP_EOL;
    }
}

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

    if ($startPoint->x > 0) {
        $topPoint = new Point($startPoint->x - 1, $startPoint->y);

        if (\in_array($grid[$topPoint->x][$topPoint->y], ['|', 'F', '7'], true)) {
            $connectedPoints['top'] = true;
        }
    }

    if ($startPoint->y < count($grid[0]) - 1) {
        $rightPoint = new Point($startPoint->x, $startPoint->y + 1);

        if (\in_array($grid[$rightPoint->x][$rightPoint->y], ['-', 'J', '7'], true)) {
            $connectedPoints['right'] = true;
        }
    }

    if ($startPoint->x < count($grid) - 1) {
        $bottomPoint = new Point($startPoint->x + 1, $startPoint->y);

        if (\in_array($grid[$bottomPoint->x][$bottomPoint->y], ['|', 'L', 'J'], true)) {
            $connectedPoints['bottom'] = true;
        }
    }

    if ($startPoint->y > 0) {
        $leftPoint = new Point($startPoint->x, $startPoint->y - 1);

        if (\in_array($grid[$leftPoint->x][$leftPoint->y], ['-', 'F', 'L'], true)) {
            $connectedPoints['left'] = true;
        }
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

function get_grid_without_pipes_out_of_loop(array $grid, Point $startPoint): array
{
    $cleanGrid = get_empty_grid(count($grid), count($grid[0]), '.');

    $currentPoint = $startPoint;

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
        $cleanGrid[$currentPoint->x][$currentPoint->y] = $grid[$currentPoint->x][$currentPoint->y];

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

    return $cleanGrid;
}

function get_empty_grid(int $height, int $width, $value): array
{
    $grid = [];

    for ($x = 0; $x < $height; ++$x) {
        for ($y = 0; $y < $width; ++$y) {
            $grid[$x][$y] = $value;
        }
    }

    return $grid;
}

function get_number_of_enclosed_tiles(array $grid): int
{
    $parsedGrid = get_empty_grid(count($grid), count($grid[0]), 0);

    // row parsing
    for ($x = 0, $xMax = count($grid); $x < $xMax; ++$x) {
        $isEnclosed = false;
        $segmentStartPipe = null;

        for ($y = 0, $yMax = count($grid[$x]); $y < $yMax; ++$y) {
            $tile = $grid[$x][$y];

            if ('.' === $tile && $isEnclosed) {
                ++$parsedGrid[$x][$y];
            }

            if ('|' === $tile) {
                $isEnclosed = !$isEnclosed;
            }

            if ('L' === $tile || 'F' === $tile) {
                $segmentStartPipe = $tile;
            }

            if ('J' === $tile || '7' === $tile) {
                if (null === $segmentStartPipe) {
                    throw new LogicException();
                }

                if (
                    ('L' === $segmentStartPipe && '7' === $tile)
                    || ('F' === $segmentStartPipe && 'J' === $tile)
                ) {
                    $isEnclosed = !$isEnclosed;
                    $segmentStartPipe = null;
                }
            }
        }
    }

    // col parsing
    for ($y = 0, $yMax = count($grid[0]); $y < $yMax; ++$y) {
        $isEnclosed = false;
        $segmentStartPipe = null;

        for ($x = 0, $xMax = count($grid); $x < $xMax; ++$x) {
            $tile = $grid[$x][$y];

            if ('.' === $tile && $isEnclosed) {
                ++$parsedGrid[$x][$y];
            }

            if ('-' === $tile) {
                $isEnclosed = !$isEnclosed;
            }

            if ('F' === $tile || '7' === $tile) {
                $segmentStartPipe = $tile;
            }

            if ('L' === $tile || 'J' === $tile) {
                if (null === $segmentStartPipe) {
                    throw new LogicException();
                }

                if (
                    ('F' === $segmentStartPipe && 'J' === $tile)
                    || ('7' === $segmentStartPipe && 'L' === $tile)
                ) {
                    $isEnclosed = !$isEnclosed;
                    $segmentStartPipe = null;
                }
            }
        }
    }

    $numberOfEnclosedTiles = 0;

    foreach ($parsedGrid as $row) {
        foreach ($row as $value) {
            if (2 === $value) {
                ++$numberOfEnclosedTiles;
            }
        }
    }

    return $numberOfEnclosedTiles;
}