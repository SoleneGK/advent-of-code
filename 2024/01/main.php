<?php

declare(strict_types=1);

$leftList = [];
$rightList = [];

$file = fopen('input.txt', 'rb');

while (false !== $line = fgets($file)) {
    [$leftValue, $rightValue] = explode('   ', $line);
    $leftList[] = (int) $leftValue;
    $rightList[] = (int) trim($rightValue);
}

sort($leftList);
sort($rightList);

$distanceSum = 0;

$length = count($leftList);

for ($i = 0; $i < $length; $i++) {
    $distanceSum += abs($leftList[$i] - $rightList[$i]);
}

echo 'The total distance between the lists is ' . $distanceSum . "\n";


// Part 2

$similarityData = [];

foreach ($leftList as $value) {
    if (!isset($similarityData[$value])) {
        $similarityData[$value] = [
            'leftOccurences' => 0,
            'rightOccurences' => 0,
        ];
    }

    $similarityData[$value]['leftOccurences']++;
}

foreach ($rightList as $value) {
    if (!isset($similarityData[$value])) {
        $similarityData[$value] = [
            'leftOccurences' => 0,
            'rightOccurences' => 0,
        ];
    }

    $similarityData[$value]['rightOccurences']++;
}

$similarityScore = 0;

foreach ($similarityData as $key => $data) {
    $similarityScore += $key * $data['leftOccurences'] * $data['rightOccurences'];
}

echo 'The similarity score is ' . $similarityScore . "\n";