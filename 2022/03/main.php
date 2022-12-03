<?php

$file = fopen('input.txt', 'rb');

$prioritySum = 0;

while (false !== $rucksack = fgets($file)) {
    $rucksack = str_split($rucksack);

    [$firstHalf, $secondHalf] = array_chunk($rucksack, count($rucksack) / 2);

    $errorItemList = array_intersect($firstHalf, $secondHalf);
    $errorItem = array_shift($errorItemList);

    $isCapital = ctype_upper($errorItem);
    $asciiValue = ord($errorItem);

    $prioritySum += $isCapital ? $asciiValue - 64 + 26 : $asciiValue - 96;
}

echo "The answer for part 1 is $prioritySum\n";