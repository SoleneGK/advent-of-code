<?php

declare(strict_types=1);

$file = fopen('input.txt', 'rb');

$data = [];

while (false !== $line = fgets($file)) {
    $data[] = trim($line);
}


$total = 0;

foreach ($data as $line) {
    $matchList = [];

    preg_match_all('/mul\(\d{1,3},\d{1,3}\)/', $line, $matchList);

    foreach ($matchList[0] as $match) {
        $total += getMultResult($match);
    }
}

function getMultResult(string $mult) {
    $a = substr($mult, 4);
    $a = substr($a, 0, strlen($a) - 1);

    $a = explode(',', $a);
    return (int) $a[0] * (int) $a[1];
}

echo "Total: $total\n";


// Part 2

$total = 0;
$activated = true;

foreach ($data as $line) {
    $matchList = [];

    preg_match_all("/mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)/", $line, $matchList);    
    
    foreach ($matchList[0] as $match) {
        if ($match === 'do()') {
            $activated = true;
            
            continue;
        }
    
        if ($match === "don't()") {
            $activated = false;
    
            continue;
        }
    
        if (!$activated) {
            continue;
        }
    
        $total += getMultResult($match);
    }
}

echo "Total part 2: $total\n";
