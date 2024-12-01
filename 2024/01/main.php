<?php

declare(strict_types=1);

$leftList = [];
$rightList = [];

$file = fopen('input.txt', 'rb');

while (false !== $line = fgets($file)) {
    [$leftValue, $rightValue] = explode('   ', $line);
    $leftList[] = $leftValue;
    $rightList[] = $rightValue;
}

sort($leftList);
sort($rightList);

$distanceSum = 0;

$length = count($leftList);

for ($i = 0; $i < $length; $i++) {
    $distanceSum += abs($leftList[$i] - $rightList[$i]);
}

echo 'The total distance between the lists is ' . $distanceSum . "\n";