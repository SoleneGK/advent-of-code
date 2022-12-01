<?php

$directionInput = file_get_contents('input.txt', 'rb');
$directionList = str_split($directionInput);

$houses = [];

// Départ
$x = 0;
$y = 0;
$houses[$x][$y] = 1;

// Noter le nombre de visites dans chaque maison
foreach ($directionList as $direction) {
    switch ($direction) {
        case '<':
            --$y;
            break;
        case '>':
            ++$y;
            break;
        case '^':
            ++$x;
            break;
        case 'v':
            --$x;
    }

    if (!isset($houses[$x][$y])) {
        $houses[$x][$y] = 1;

        continue;
    }

    $houses[$x][$y]++;
}

// Compter le nombre de maisons visitées
$numberOfHousesVisited = 0;

foreach ($houses as $houseRow) {
    foreach ($houseRow as $house) {
        ++$numberOfHousesVisited;
    }
}

echo "Santa has visited $numberOfHousesVisited houses\n";
