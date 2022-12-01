<?php

$directionInput = file_get_contents('input.txt', 'rb');
$directionList = str_split($directionInput);

$houses = [];

// Départ
$xSanta = 0;
$ySanta = 0;
$xRoboSanta = 0;
$yRoboSanta = 0;
$houses[0][0] = 2;

// Noter le nombre de visites dans chaque maison
foreach ($directionList as $key => $direction) {
    if (0 === $key % 2) {
        switch ($direction) {
            case '<':
                --$ySanta;
                break;
            case '>':
                ++$ySanta;
                break;
            case '^':
                ++$xSanta;
                break;
            case 'v':
                --$xSanta;
        }

        if (!isset($houses[$xSanta][$ySanta])) {
            $houses[$xSanta][$ySanta] = 1;

            continue;
        }
    } else {
        switch ($direction) {
            case '<':
                --$yRoboSanta;
                break;
            case '>':
                ++$yRoboSanta;
                break;
            case '^':
                ++$xRoboSanta;
                break;
            case 'v':
                --$xRoboSanta;
        }

        if (!isset($houses[$xRoboSanta][$yRoboSanta])) {
            $houses[$xRoboSanta][$yRoboSanta] = 1;

            continue;
        }
    }
}

// Compter le nombre de maisons visitées
$numberOfHousesVisited = 0;

foreach ($houses as $houseRow) {
    foreach ($houseRow as $house) {
        ++$numberOfHousesVisited;
    }
}

echo "Santa and Robo-Santa have visited $numberOfHousesVisited houses\n";
