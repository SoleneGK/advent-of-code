<?php

declare(strict_types=1);

require_once 'Map.php';
require_once 'Range.php';

$file = fopen('input.txt', 'rb');

$mapList = [];
$seedRangeList = [];
$currentMap = null;

$lineNumber = 0;

while (false !== $line = fgets($file)) {
    $lineNumber++;
    $line = trim($line);

    // line 1: seed list
    if (1 === $lineNumber) {
        $seedRangeList = get_seed_range_list($line);

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
    $currentMap->addLine($line);
}

fclose($file);

function get_seed_range_list(string $line): array
{
    $seedRangeList = [];

    $line = substr($line, 7);
    $seedData = array_map('intval', explode(' ', $line));

    for ($i = 0, $iMax = count($seedData); $i < $iMax; $i += 2) {
        $start = $seedData[$i];
        $end = $start + $seedData[$i + 1] - 1;

        $seedRangeList[] = new Range($start, $end);
    }

    return $seedRangeList;
}

$numbers = ['seed' => $seedRangeList];

foreach ($mapList as $map) {
    $numbers[$map->destinationName] = [];

    foreach ($numbers[$map->sourceName] as $rangeList) {
        $numbers[$map->destinationName][] = $map->convert($rangeList);
    }

    $numbers[$map->destinationName] = array_merge(...$numbers[$map->destinationName]);
}

$locationData = array_map(static fn (Range $range) => $range->start, $numbers['location']);

echo 'The lowest location number is ' . min($locationData). PHP_EOL;




