<?php

declare(strict_types=1);

require_once 'Range.php';

$fileContent = file_get_contents('input.txt');

$data = explode(',', $fileContent);
$sumForPart1 = 0;
$sumForPart2 = 0;

foreach ($data as $rangeData) {
    $range = new Range($rangeData);
    $rangeList = $range;
    $sumForPart1 += $range->getSumOfInvalidIdsForPart1();
    $sumForPart2 += $range->getSumOfInvalidIdsForPart2();
}

echo "The sum of the invalid IDs for part 1 is $sumForPart1\n";
echo "The sum of the invalid IDs for part 2 is $sumForPart2\n";

