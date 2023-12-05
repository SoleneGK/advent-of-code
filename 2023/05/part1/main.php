<?php

declare(strict_types=1);

require_once 'Map.php';
require_once 'Range.php';

$file = fopen('example.txt', 'rb');

$mapList = [];
$seedList = [];
$currentMap = null;

$lineNumber = 0;

while (false !== $line = fgets($file)) {
    $lineNumber++;
    $line = trim($line);

    // line 1: seed list
    if (1 === $lineNumber) {
        $seedList = get_seed_list($line);

        continue;
    }

    // if empty line, next one is a new map
    if ('' === $line) {
        $currentMap = new Map();
        $mapList[] = $currentMap;

        continue;
    }

    // if first character is a letter, it the name of the new map
    if (ctype_alpha($line[0])) {
        $currentMap->setName($line);

        continue;
    }

    // else, it's a range data
    $currentMap->addRange($line);
}

fclose($file);

function get_seed_list(string $line): array
{
    $rawList = substr($line, 7);

    return array_map('intval', explode(' ', $rawList));
}

$numbers = [];

foreach ($seedList as $seed) {
    $numbers[$seed] = ['seed' => $seed];

    foreach ($mapList as $map) {
        $numbers[$seed][$map->destinationName] = $map->convert($numbers[$seed][$map->sourceName]);
    }
}

$locationValues = array_map(static fn(array $array) => $array['location'], $numbers);

echo 'The lowest location number is ' . min($locationValues) . "\n";