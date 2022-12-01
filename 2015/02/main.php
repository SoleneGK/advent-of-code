<?php

$presentList = fopen('input.txt', 'rb');

$paperNeeded = 0;

while (false !== $present = fgets($presentList)) {
    $presentDimensions = explode('x', $present);
    // 0 => l, 1 => w, 2 => h

    // Surfaces
    $presentPaperAreas = [
        $presentDimensions[0] * $presentDimensions[1],
        $presentDimensions[1] * $presentDimensions[2],
        $presentDimensions[0] * $presentDimensions[2]
    ];

    // Supplément = plus petite surface
    $paperSlack = min($presentPaperAreas);

    // Total du papier nécessaire
    $paperSize =
        2 * $presentPaperAreas[0]
        + 2 * $presentPaperAreas[1]
        + 2 * $presentPaperAreas[2]
        + $paperSlack
    ;

    $paperNeeded += $paperSize;
}

echo "They should order $paperNeeded square feet of wrapping paper\n";