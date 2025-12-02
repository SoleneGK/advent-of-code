<?php

declare(strict_types=1);

require_once 'Range.php';

$fileContent = file_get_contents('input.txt');

$data = explode(',', $fileContent);
$sumForPart1 = 0;

foreach ($data as $rangeData) {
    $range = new Range($rangeData);
    $rangeList = $range;
    $sumForPart1 += $range->getSumOfInvalidIdsForPart1();
}

echo "The sum of the invalid IDs for part 1 is $sumForPart1\n";

