<?php

declare(strict_types=1);

$file = fopen('input.txt', 'rb');

$total = 0;

while (false !== $line = fgets($file)) {
    $line = trim($line);
    $matchList = [];

    preg_match_all('/mul\(\d{1,3},\d{1,3}\)/', $line, $matchList);

    foreach ($matchList[0] as $match) {
        $a = substr($match, 4);
        $a = substr($a, 0, strlen($a) - 1);

        $a = explode(',', $a);
        $total += (int) $a[0] * (int) $a[1];
    }
}

echo "Total: $total\n";
