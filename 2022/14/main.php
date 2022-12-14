<?php

$rockStructureList = fopen('input.txt', 'rb');

// Create list of rocks
$caveMap = [];

while (false !== $rockStructure = fgets($rockStructureList)) {
    $lineList = explode(' -> ', $rockStructure);

    $lineList = array_map(static function ($value) {
        $coordinates = explode(',', $value);

        return [
            'col' => (int) $coordinates[0],
            'row' => (int) $coordinates[1],
        ];
    }, $lineList);

    $iMax = count($lineList) - 1;

    for ($i = 0; $i < $iMax; ++$i) {
        $start = $lineList[$i];
        $end = $lineList[$i+1];

        if ($start['col'] === $end['col']) {
            foreach (range(min($start['row'], $end['row']), max($start['row'], $end['row'])) as $row) {
                $caveMap[$row][$start['col']] = 'rock';
            }

            continue;
        }

        foreach (range(min($start['col'], $end['col']), max($start['col'], $end['col'])) as $col) {
            $caveMap[$start['row']][$col] = 'rock';
        }
    }
}

$maxRockRow = max(array_keys($caveMap));

// Drop sand
$numberOfUnitOfSand = 0;

$sandStartCol = 500;
$sandStartRow = 0;

$sandCol = $sandStartCol;
$sandRow = $sandStartRow;

$answerPart1 = null;

while (true) {
    ++$numberOfUnitOfSand;

    $sandCol = $sandStartCol;
    $sandRow = $sandStartRow;

    while (true) {
        // Keep getting part 1 answer - not working
        if ($sandRow === $maxRockRow) {
            $answerPart1 = $numberOfUnitOfSand - 1;
        }

        if ($maxRockRow + 1 === $sandRow) {
            $caveMap[$sandRow][$sandCol] = 'sand';

            break;
        }

        if (!isset($caveMap[$sandRow+1][$sandCol])) {
            ++$sandRow;

            continue;
        }

        if (!isset($caveMap[$sandRow+1][$sandCol-1])) {
            ++$sandRow;
            --$sandCol;

            continue;
        }

        if (!isset($caveMap[$sandRow+1][$sandCol+1])) {
            ++$sandRow;
            ++$sandCol;

            continue;
        }

        $caveMap[$sandRow][$sandCol] = 'sand';

        break;
    }

    if ($sandCol === $sandStartCol && $sandRow === $sandStartRow) {
        break;
    }
}

// For debug and visualization purpose
function display(array $caveMap): void
{
    $minRow = 0;
    $maxRow = max(array_keys($caveMap));

    $minCol = 500;
    $maxCol = 500;

    {
        foreach ($caveMap as $row) {
            $colList = array_keys($row);

            if (max($colList) > $maxCol) {
                $maxCol = max($colList);
            }

            if (min($colList) < $minCol) {
                $minCol = min($colList);
            }
        }
    }

    for ($row = $minRow - 1; $row <= $maxRow + 1; ++$row) {
        for ($col = $minCol - 1; $col <= $maxCol + 1; ++$col) {
            if (0 === $row && 500 === $col) {
                echo '+';

                continue;
            }

            if (!isset($caveMap[$row][$col])) {
                echo '.';

                continue;
            }

            if ('rock' === $caveMap[$row][$col]) {
                echo '#';

                continue;
            }

            if ('sand' === $caveMap[$row][$col]) {
                echo 'o';
            }
        }

        echo "\n";
    }
}

$answerPart2 = $numberOfUnitOfSand;

echo "The answer for part 1 is $answerPart1\n";
echo "The answer for part 2 is $answerPart2\n";