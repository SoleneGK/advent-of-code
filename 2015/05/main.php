<?php

function isNicePart1(string $string): bool
{
    // Does the string contains a forbidden string?
    if (
        str_contains($string, 'ab')
        || str_contains($string, 'cd')
        || str_contains($string, 'pq')
        || str_contains($string, 'xy')
    ) {
        return false;
    }

    // Does the string contains at least three vowels?
    $numberOfVowels = substr_count($string, 'a')
         + substr_count($string, 'e')
         + substr_count($string, 'i')
         + substr_count($string, 'o')
         + substr_count($string, 'u')
    ;

    if ($numberOfVowels < 3) {
        return false;
    }

    // Does the string contains a double letter?
    $iMax = strlen($string) - 1;

    for ($i = 0; $i < $iMax; ++$i) {
        if ($string[$i] === $string[$i+1]) {
            return true;
        }
    }

    return false;
}

function isNicePart2(string $string): bool
{
    return hasOverlappingPair($string) && hasOneLetterRepeatingWithOneLetterBetween($string);
}

function hasOverlappingPair(string $string): bool
{
    // I could go with minus 1, but when there are 3 or less character left, it's not possible
    // to have 2 pairs not overlapping
    $iMax = strlen($string) - 3;

    for ($i = 0; $i < $iMax; ++$i) {
        $pair = substr($string, $i, 2);

        // Can we find the pair in the rest of the string?
        if (str_contains(substr($string, $i+2), $pair)) {
            return true;
        }
    }

    return false;
}

function hasOneLetterRepeatingWithOneLetterBetween(string $string): bool
{
    $iMax = strlen($string) - 2;

    for ($i = 0; $i < $iMax; ++$i) {
        if ($string[$i] === $string[$i+2]) {
            return true;
        }
    }

    return false;
}

$stringList = fopen('input.txt', 'rb');

$numberOfNiceStringsPart1 = 0;
$numberOfNiceStringsPart2 = 0;

while (false !== $string = fgets($stringList)) {
    if (isNicePart1($string)) {
        ++$numberOfNiceStringsPart1;
    }

    if (isNicePart2($string)) {
        ++$numberOfNiceStringsPart2;
    }
}

echo "There are $numberOfNiceStringsPart1 nice strings with part 1 rules\n";
echo "There are $numberOfNiceStringsPart2 nice strings with part 2 rules\n";