<?php

declare(strict_types=1);

require_once 'Number.php';
require_once 'Gear.php';

$file = fopen('input.txt', 'rb');

// Transforme les données en tableau
$grid = [];

while (false !== $line = fgets($file)) {
    $grid[] = str_split(trim($line));
}

$numberList = [];
$currentNumberValue = null;
$currentNumberX = null;
$currentNumberMinY = null;
$currentNumberMaxY = null;

for ($x = 0, $xMax = count($grid); $x < $xMax; $x++) {
    for ($y = 0, $yMax = count($grid[$x]); $y < $yMax; $y++) {
        if (!is_numeric($grid[$x][$y])) {
            if (null === $currentNumberValue) {
                continue;
            }

            $numberList[] = new Number($currentNumberValue, $currentNumberX, $currentNumberMinY, $currentNumberMaxY);
            $currentNumberValue = null;
            $currentNumberX = null;
            $currentNumberMinY = null;
            $currentNumberMaxY = null;

            continue;
        }

        $currentNumberValue = ($currentNumberValue ?? 0) * 10 + (int) $grid[$x][$y];
        $currentNumberX = $x;
        $currentNumberMinY = $currentNumberMinY ?? $y;
        $currentNumberMaxY = $y;

        if ($yMax === $y + 1 && null !== $currentNumberValue) {
            $numberList[] = new Number($currentNumberValue, $currentNumberX, $currentNumberMinY, $currentNumberMaxY);
            $currentNumberValue = null;
            $currentNumberX = null;
            $currentNumberMinY = null;
            $currentNumberMaxY = null;
        }
    }
}

$sumOfPartNumbers = 0;

foreach ($numberList as $number) {
    if ($number->isPartNumber($grid)) {
        $sumOfPartNumbers += $number->value;
    }
}

echo "The sum of part numbers is $sumOfPartNumbers\n";

$datagrid = [];
$gearList = [];
$yMax = count($grid[0]);

for ($x = 0, $xMax = count($grid); $x < $xMax; $x++) {
    for ($y = 0; $y < $yMax; $y++) {
        $datagrid[$x][$y] = null;

        if ('*' === $grid[$x][$y]) {
            $gearList[] = new Gear($x, $y);
        }
    }
}

foreach ($numberList as $number) {
    for ($y = $number->minY; $y <= $number->maxY; $y++) {
        $datagrid[$number->x][$y] = $number;
    }
}

$sumOfGearRatio = 0;

foreach ($gearList as $gear) {
    $gear->getAdjacentNumbers($datagrid);

    if ($gear->isValidGear($datagrid)) {
        $sumOfGearRatio += $gear->getGearRatio();
    }
}

echo "The sum of gear ratios is $sumOfGearRatio\n";

fclose($file);