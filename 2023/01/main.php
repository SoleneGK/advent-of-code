<?php

declare(strict_types=1);

$file = fopen('example.txt', 'rb');

$sumOfCalibrationValues = 0;

while (false !== $line = fgets($file)) {
    $firstDigit = null;
    $lastDigit = null;

    foreach (str_split($line) as $character) {
        if (!is_numeric($character)) {
            continue;
        }

        if (null === $firstDigit) {
            $firstDigit = (int) $character;
        }

        $lastDigit = (int) $character;
    }

    $sumOfCalibrationValues += 10 * $firstDigit + $lastDigit;
}

echo "The answer for part 1 id $sumOfCalibrationValues\n";