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
