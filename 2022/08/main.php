<?php

// I'm pretty sure I can do better than all those loops with the same structure
// but I'm supposed to work, so...

function display(array $map): void
{
    $size = count($map);

    foreach ($map as $rowValue) {
        for ($col = 0; $col < $size; ++$col) {
            echo $rowValue[$col] ? '1' : '0';
        }

        echo "\n";
    }
}

$rawMap = explode("\n", file_get_contents('input.txt'));

$map = [];

foreach ($rawMap as $row => $line) {
    $treeList = str_split($line);

    foreach ($treeList as $col => $tree) {
        $map[$row][$col] = (int) $tree;
    }
}

$size = count($map);
$isVisible = [];

for ($row = 0; $row < $size; ++$row) {
    for ($col = 0; $col < $size; ++$col) {
        $isVisible[$row][$col] = false;
    }
}

// Row traversal from the left
for ($row = 0; $row < $size; ++$row) {
    $maxTreeSize = -1;

    $col = 0;

    while ($maxTreeSize < 9 && $col < $size) {
        if ($map[$row][$col] > $maxTreeSize) {
            $isVisible[$row][$col] = true;
            $maxTreeSize = $map[$row][$col];
        }

        ++$col;
    }
}

// Row traversal from the right
for ($row = 0; $row < $size; ++$row) {
    $maxTreeSize = -1;

    $col = $size - 1;

    while ($maxTreeSize < 9 && $col >= 0) {
        if ($map[$row][$col] > $maxTreeSize) {
            $isVisible[$row][$col] = true;
            $maxTreeSize = $map[$row][$col];
        }

        --$col;
    }
}

// Column traversal from the top
for ($col = 0; $col < $size; ++$col) {
    $maxTreeSize = -1;

    $row = 0;

    while ($maxTreeSize < 9 && $row < $size) {
        if ($map[$row][$col] > $maxTreeSize) {
            $isVisible[$row][$col] = true;
            $maxTreeSize = $map[$row][$col];
        }

        ++$row;
    }
}

// Column traversal from the bottom
for ($col = 0; $col < $size; ++$col) {
    $maxTreeSize = -1;

    $row = $size - 1;

    while ($maxTreeSize < 9 && $row >= 0) {
        if ($map[$row][$col] > $maxTreeSize) {
            $isVisible[$row][$col] = true;
            $maxTreeSize = $map[$row][$col];
        }

        --$row;
    }
}

$numberOfVisibleTrees = 0;

for ($row = 0; $row < $size; ++$row) {
    for ($col = 0; $col < $size; ++$col) {
        if ($isVisible[$row][$col]) {
            ++$numberOfVisibleTrees;
        }
    }
}

echo "There are $numberOfVisibleTrees visible trees\n";



function getScenicValue(array $map, int $row, int $col): int
{
    $size = count($map);

    // On an edge
    if (0 === $row || 0 === $col || $size === $row || $size === $col) {
        return 0;
    }

    $scenicValue = 1;

    // To the top
    $numberOfTreesVisibles = 0;

    for ($i = $row - 1; $i >= 0; --$i) {
        ++$numberOfTreesVisibles;

        // Same height or bigger tree : stop counting
        if ($map[$i][$col] >= $map[$row][$col]) {
            break;
        }
    }

    $scenicValue *= $numberOfTreesVisibles;

    // To the top
    $numberOfTreesVisibles = 0;

    for ($i = $row + 1; $i < $size; ++$i) {
        ++$numberOfTreesVisibles;

        // Same height or bigger tree : stop counting
        if ($map[$i][$col] >= $map[$row][$col]) {
            break;
        }
    }

    $scenicValue *= $numberOfTreesVisibles;

    // To the left
    $numberOfTreesVisibles = 0;

    for ($i = $col - 1; $i >= 0; --$i) {
        ++$numberOfTreesVisibles;

        // Same height or bigger tree : stop counting
        if ($map[$row][$i] >= $map[$row][$col]) {
            break;
        }
    }

    $scenicValue *= $numberOfTreesVisibles;

    // To the right
    $numberOfTreesVisibles = 0;

    for ($i = $col + 1; $i < $size; ++$i) {
        ++$numberOfTreesVisibles;

        // Same height or bigger tree : stop counting
        if ($map[$row][$i] >= $map[$row][$col]) {
            break;
        }
    }

    $scenicValue *= $numberOfTreesVisibles;

    return $scenicValue;
}

echo getScenicValue($map, 3, 2)."\n";


$maxScenicValue = 0;

for ($row = 0; $row < $size; ++$row) {
    for ($col = 0; $col < $size; ++$col) {
        $scenicValue = getScenicValue($map, $row, $col);

        if ($scenicValue > $maxScenicValue) {
            $maxScenicValue = $scenicValue;
        }

    }
}

echo "The highest scenic score possible is $maxScenicValue\n";