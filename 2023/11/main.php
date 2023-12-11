<?php

declare(strict_types=1);

require_once 'Galaxy.php';

$image = get_image();
$emptyRows = get_empty_rows($image);
$emptyCols = get_empty_cols($image);

// Part 1
$galaxyList = get_galaxy_list($image, $emptyRows, $emptyCols, 2);
$sumOfDistances = get_sum_of_distances($galaxyList);

echo "The sum of the distances with expansion factor 1 is $sumOfDistances\n";

// Part 2
$galaxyList = get_galaxy_list($image, $emptyRows, $emptyCols, 1000000);
$sumOfDistances = get_sum_of_distances($galaxyList);

echo "The sum of the distances with expansion factor 1000000 is $sumOfDistances\n";

function get_image(): array
{
    $file = fopen('input.txt', 'rb');

    $image = [];

    while ($line = fgets($file)) {
        $image[] = str_split(trim($line));
    }

    fclose($file);

    return $image;
}

function get_empty_rows($image): array
{
    $emptyLines = [];

    foreach ($image as $x => $row) {
        if (0 === count(array_filter($row, static fn(string $value) => $value !== '.'))) {
            $emptyLines[] = $x;
        }
    }

    return $emptyLines;
}

function get_empty_cols($image): array
{
    $emptyCols = [];

    for ($y = 0, $yMax = count($image[0]); $y < $yMax; $y++) {
        $isEmpty = true;

        foreach ($image as $xValue) {
            if ($xValue[$y] !== '.') {
                $isEmpty = false;
            }
        }

        if ($isEmpty) {
            $emptyCols[] = $y;
        }
    }

    return $emptyCols;
}

function get_galaxy_list(array $image, array $emptyRows, array $emptyCols, int $expansionFactor): array
{
    $galaxyList = [];

    foreach ($image as $x => $row) {
        foreach ($row as $y => $value) {
            if ($value !== '#') {
                continue;
            }

            $rowShift = count(array_filter($emptyRows, static function(int $rowIndex) use ($x) {
                return $rowIndex < $x;
            }));

            $colShift = count(array_filter($emptyCols, static function(int $colIndex) use ($y) {
                return $colIndex < $y;
            }));

            $galaxyList[] = new Galaxy(
                $x + $rowShift * ($expansionFactor - 1),
                $y + $colShift * ($expansionFactor - 1)
            );
        }
    }

    return $galaxyList;
}

function get_sum_of_distances(array $galaxyList): int
{
    $sumOfDistances = 0;
    $numberOfGalaxies = count($galaxyList);

    foreach ($galaxyList as $i => $galaxy) {
        for ($j = $i + 1; $j < $numberOfGalaxies; $j++) {
            $sumOfDistances += get_distance($galaxy, $galaxyList[$j]);
        }
    }

    return $sumOfDistances;
}

function get_distance(Galaxy $galaxy1, Galaxy $galaxy2): int
{
    return abs($galaxy1->x - $galaxy2->x) + abs($galaxy1->y - $galaxy2->y);
}
