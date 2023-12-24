<?php

declare(strict_types=1);

$frequency = 0;

$file = fopen('input.txt', 'rb');

while ($line = fgets($file)) {
    $frequency += (int) trim($line);
}

fclose($file);

echo "The resulting frequency is $frequency\n";