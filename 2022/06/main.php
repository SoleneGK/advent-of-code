<?php

$input = file_get_contents('input.txt');

function getFirstPositionWithoutDuplicate(string $input, int $length): int
{
    $maxOffset = strlen($input) - $length;

// For each position of first character
    for ($i = 0; $i < $maxOffset; ++$i) {
        // Get the 4 char string
        $potentialMarker = substr($input, $i, $length);
        // Remove duplicates
        $potentialMarkerWithoutDuplicate = array_unique(str_split($potentialMarker));

        // If no duplicate, the size are the same
        if (\strlen($potentialMarker) === \count($potentialMarkerWithoutDuplicate)) {
            return $i + $length;
        }
    }

    return -1;
}

echo 'The answer for part 1 is '.getFirstPositionWithoutDuplicate($input, 4)."\n";
echo 'The answer for part 1 is '.getFirstPositionWithoutDuplicate($input, 14)."\n";
