<?php

declare(strict_types=1);

require_once 'Bag.php';
require_once 'Game.php';

$file = fopen('input.txt', 'rb');

$gameList = [];

while (false !== $line = fgets($file)) {
    try {
        $gameList[] = new Game($line);
    } catch (Exception $e) {
    }
}

/** Part 1 */
$bag = new Bag(12, 13, 14);

$idSum = 0;

foreach ($gameList as $game) {
    if ($bag->isValidGame($game)) {
        $idSum += $game->id;
    }
}

echo "The sum of the IDs of all valid games is $idSum\n";

/** Part 2 */
$sumOfMinimalSetPower = 0;

foreach($gameList as $game) {
    $sumOfMinimalSetPower += $game->getMinimalSetPower();
}

echo "The sum of the minimal set powers is $sumOfMinimalSetPower\n";

fclose($file);