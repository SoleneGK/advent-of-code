<?php

declare(strict_types=1);

require_once 'Grid.php';

$file = fopen('input.txt', 'rb');

$grid = new Grid();

while (false !== $line = fgets($file)) {
    $grid->addLine(str_split(trim($line)));
}

echo "XMAS appears {$grid->getNumberOfXmas()} times\n";

echo "X-MAS appears {$grid->getNumberOfXOfMas()} times\n";
