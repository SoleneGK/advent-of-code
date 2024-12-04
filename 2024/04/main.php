<?php

declare(strict_types=1);

require_once 'Grid.php';

$file = fopen('example.txt', 'rb');

$grid = new Grid();

while (false !== $line = fgets($file)) {
    $grid->addLine(str_split(trim($line)));
}

echo "XMAS appears {$grid->getNumberOfXmas()} times\n";
