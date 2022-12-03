<?php

$file = fopen('input.txt', 'rb');

$prioritySum = 0;

function getPriority(string $char): int
{
    $isCapital = ctype_upper($char);
    $asciiValue = ord($char);

    return $isCapital ? $asciiValue - 64 + 26 : $asciiValue - 96;
}

while (false !== $rucksack = fgets($file)) {
    $rucksack = str_split($rucksack);

    [$firstHalf, $secondHalf] = array_chunk($rucksack, count($rucksack) / 2);

    $errorItemList = array_intersect($firstHalf, $secondHalf);
    $errorItem = array_shift($errorItemList);

    $prioritySum += getPriority($errorItem);
}

echo "The answer for part 1 is $prioritySum\n";

rewind($file);

$prioritySum = 0;

while (
    (false !== $rucksack[0] = fgets($file))
    && (false !== $rucksack[1] = fgets($file))
    && (false !== $rucksack[2] = fgets($file))
) {
    array_walk($rucksack, static function (&$value) {
        $value = str_split(trim($value));
    });

    /** @var array $commonItemList */
    $commonItemList = array_intersect(...$rucksack);
    $commonItem = array_shift($commonItemList);

    $prioritySum += getPriority($commonItem);
}

echo "The answer for part 2 is $prioritySum\n";