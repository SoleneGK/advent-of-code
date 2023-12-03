<?php

declare(strict_types=1);

require_once 'Number.php';

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

fclose($file);