<?php

declare(strict_types=1);

$dial = 50;
echo "Dial: 50\n";
$numberOfStopsAtZeroForPart1 = 0;
$numberOfStopsAtZeroForPart2 = 0;

$file = fopen('input.txt', 'rb');

while (false !== $line = fgets($file)) {
    $sense = $line[0];
    $distance = (int) substr($line, 1);

    echo "\nLine: $sense$distance\n";

    /** Part 2 */
    if ($distance >= 100) {
        $numberOfRotations = floor($distance / 100);
        $smallDistance = $distance % 100;
    } else {
        $numberOfRotations = 0;
        $smallDistance = $distance;
    }

    echo "Number of rotations: $numberOfRotations, small distance: $smallDistance\n";

    if ($numberOfRotations > 0) {
        $numberOfStopsAtZeroForPart2 += $numberOfRotations;
        echo "+$numberOfRotations\n";

        if ($smallDistance === 0 && $dial === 0) {
            $numberOfStopsAtZeroForPart2--;
            echo "-1\n";
        }
    }

    if ($smallDistance > 0 && $dial !== 0) {
        if ('R' === $sense && $smallDistance > 100 - $dial) {
            echo "+1\n";
            $numberOfStopsAtZeroForPart2++;
        } elseif ('L' === $sense && $smallDistance > $dial) {
            echo "+1\n";
            $numberOfStopsAtZeroForPart2++;
        }
    }
    /** End of part 2 */

    if ('R' === $sense) {
        $dial = ($dial + $smallDistance) % 100;
    } else {
        $dial = ($dial - $smallDistance + 100) % 100;
    }

    if (0 === $dial) {
        $numberOfStopsAtZeroForPart1++;
        $numberOfStopsAtZeroForPart2++;
    }

    echo "Dial: $dial\n";
}

echo "The dial stops $numberOfStopsAtZeroForPart1 times at zero.\n";
echo "The dial points $numberOfStopsAtZeroForPart2 times at zero.\n";