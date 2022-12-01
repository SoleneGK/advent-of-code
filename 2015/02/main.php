<?php

$presentList = fopen('input.txt', 'rb');

$paperNeeded = 0;
$ribbonNeeded = 0;

while (false !== $present = fgets($presentList)) {
    $presentDimensions = explode('x', $present);
    // 0 => l, 1 => w, 2 => h

    // Partie 1

    // Surfaces
    $presentAreas = [
        $presentDimensions[0] * $presentDimensions[1],
        $presentDimensions[1] * $presentDimensions[2],
        $presentDimensions[0] * $presentDimensions[2]
    ];

    // Supplément = plus petite surface
    $paperSlack = min($presentAreas);

    // Total du papier nécessaire
    $paperNeeded +=
        2 * $presentAreas[0]
        + 2 * $presentAreas[1]
        + 2 * $presentAreas[2]
        + $paperSlack
    ;

    // Partie 2

    // Périmètres
    $presentPerimeters = [
        2 * $presentDimensions[0] + 2 * $presentDimensions[1],
        2 * $presentDimensions[1] + 2 * $presentDimensions[2],
        2 * $presentDimensions[0] + 2 * $presentDimensions[2]
    ];

    $presentVolume = $presentDimensions[0] * $presentDimensions[1] * $presentDimensions[2];

    $ribbonNeeded += min($presentPerimeters) + $presentVolume;

}

echo "They should order $paperNeeded square feet of wrapping paper and $ribbonNeeded feet of ribbon\n";
