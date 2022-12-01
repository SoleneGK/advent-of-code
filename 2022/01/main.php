<?php

$file = fopen('input.txt', 'rb');

$elfCalories = [];

$i = 1;
$elfCalories[1] = 0;

while (false !== $line = fgets($file)) {
    $line = trim($line);

    if (empty($line)) {
        $i++;
        $elfCalories[$i] = 0;

        continue;
    }

    $elfCalories[$i] += (int) $line;
}

$maxCalories = max($elfCalories);

echo "The answer for part 1 is $maxCalories\n";

sort($elfCalories);
$firstThreeElves = array_slice($elfCalories, 0, 3);

$firstThreeElvesCalories = array_sum($firstThreeElves);

echo "The answer for part 2 is $firstThreeElvesCalories\n";
