<?php

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