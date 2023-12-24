<?php

declare(strict_types=1);

$file = fopen('input.txt', 'rb');

$twoCounter = 0;
$threeCounter = 0;

while ($line = fgets($file)) {
    $line = trim($line);

    $lineCharacters = [];

    foreach (str_split($line) as $character) {
        if (!isset($lineCharacters[$character])) {
            $lineCharacters[$character] = 1;
            continue;
        }

        ++$lineCharacters[$character];
    }

    $hasTwo = false;
    $hasThree = false;

    foreach ($lineCharacters as $character => $number) {
        if (3 === $number && !$hasThree) {
            $hasThree = true;
            ++$threeCounter;
        }

        if (2 === $number && !$hasTwo) {
            $hasTwo = true;
            ++$twoCounter;
        }
    }
}

fclose($file);

echo 'The checksum is ' . $twoCounter * $threeCounter . PHP_EOL;


$idList = array_map('str_split', array_map('trim', explode(PHP_EOL, file_get_contents('input.txt'))));
$numberOfIds = count($idList);

$i = 0;
$j = 0;
$max = count($idList);
$lineLength = count($idList[0]);

while ($i < $max) {
    $j = $i + 1;

    while ($j < $max) {
        $diffNumber = 0;

        for ($k = 0; $k < $lineLength; $k++) {
            if ($idList[$i][$k] !== $idList[$j][$k]) {
                ++$diffNumber;
            }
        }

        if (1 === $diffNumber) {
            break 2;
        }

        ++$j;
    }

    ++$i;
}

echo 'The answer is ';

for ($k = 0; $k < $lineLength; $k++) {
    if ($idList[$i][$k] === $idList[$j][$k]) {
        echo $idList[$i][$k];
    }
}

echo PHP_EOL;


