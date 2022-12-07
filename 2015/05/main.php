<?php

function isNice(string $string): bool
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

$stringList = fopen('input.txt', 'rb');

$numberOfNiceStrings = 0;

while (false !== $string = fgets($stringList)) {
    if (isNice($string)) {
        ++$numberOfNiceStrings;
    }
}

echo "There are $numberOfNiceStrings nice strings\n";