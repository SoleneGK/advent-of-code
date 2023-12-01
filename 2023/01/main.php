<?php

declare(strict_types=1);

$file = fopen('input.txt', 'rb');

const NUMBERS_IN_LETTERS = [
    'one',
    'two',
    'three',
    'four',
    'five',
    'six',
    'seven',
    'eight',
    'nine',
];

const VALUE_OF_NUMBERS = [
    'one' => 1,
    'two' => 2,
    'three' => 3,
    'four' => 4,
    'five' => 5,
    'six' => 6,
    'seven' => 7,
    'eight' => 8,
    'nine' => 9,
];

$sumOfCalibrationValues = 0;

while (false !== $line = fgets($file)) {
    $firstDigit = search_first_digit($line);
    $lastDigit = search_last_digit($line);

    $sumOfCalibrationValues += 10 * $firstDigit + $lastDigit;
}

fclose($file);

function search_first_digit(string $line): int
{
    $lineAsCharacters = str_split($line);

    foreach ($lineAsCharacters as $index => $character) {
        if (is_numeric($character)) {
            return (int) $character;
        }

        $valueFound = search_for_number(substr($line, $index));

        if (null === $valueFound) {
            continue;
        }

        return VALUE_OF_NUMBERS[$valueFound];
    }

    return 0;
}

function search_last_digit(string $line): int
{
    $lineAsCharacters = str_split($line);

    for ($i = strlen($line) - 1 ; $i >= 0 ; $i--) {
        if (is_numeric($lineAsCharacters[$i])) {
            return (int) $lineAsCharacters[$i];
        }

        $valueFound = search_for_number(substr($line, $i));

        if (null === $valueFound) {
            continue;
        }

        return VALUE_OF_NUMBERS[$valueFound];
    }

    return 0;
}


/**
 * Cherche un nombre en toutes lettres
 */
function search_for_number(string $line): ?string
{
    foreach(NUMBERS_IN_LETTERS as $numberInLetters) {
        if (str_starts_with($line, $numberInLetters)) {
            return $numberInLetters;
        }
    }

    return null;
}

echo "The answer for part 2 id $sumOfCalibrationValues\n";