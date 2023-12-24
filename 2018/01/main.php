<?php

declare(strict_types=1);

$frequency = 0;

$file = fopen('input.txt', 'rb');

while ($line = fgets($file)) {
    $frequency += (int) trim($line);
}

fclose($file);

echo "The resulting frequency is $frequency\n";


$frequencyChangeList = array_map('intval', explode(PHP_EOL, file_get_contents('input.txt')))  ;

$frequency = 0;
$reachedFrenquencyList = [0];

$i = 0;
$iMax = count($frequencyChangeList);
$answerFound = false;

while (!$answerFound) {
    $frequency += $frequencyChangeList[$i];

    if (\in_array($frequency, $reachedFrenquencyList, true)) {
        $answerFound = true;
    }

    $reachedFrenquencyList[] = $frequency;

    $i = ++$i % $iMax;
}

echo "The first frequency the device reach twice is $frequency\n";
