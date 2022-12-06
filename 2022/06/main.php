<?php

$input = file_get_contents('input.txt');

$maxOffset = strlen($input) - 4;

// For each position of first character
for ($i = 0; $i < $maxOffset; ++$i) {
    // Get the 4 char string
    $potentialMarker = substr($input, $i, 4);
    // Remove duplicates
    $potentialMarkerWithoutDuplicate = array_unique(str_split($potentialMarker));

    // If no duplicate, the size are the same
    if (\strlen($potentialMarker) === \count($potentialMarkerWithoutDuplicate)) {
        break;
    }
}

$answerPart1 = $i + 4;

echo "The answer for part 1 is $answerPart1\n";