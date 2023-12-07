<?php

declare(strict_types=1);

require_once 'Card.php';
require_once 'Hand.php';
require_once 'HandType.php';

$file = fopen('input.txt', 'rb');

$handList = [];

while (false !== $line = fgets($file)) {
    $handList[] = get_hand(trim($line));
}

fclose($file);

function get_hand(string $hand): Hand
{
    $data = explode(' ', $hand);
    return new Hand($data[0], (int) $data[1]);
}

usort($handList, static fn(Hand $hand1, Hand $hand2): int => HandType::getHigherHand($hand1, $hand2));

$totalWinnings = 0;

for ($i = count($handList) - 1; $i >= 0; --$i) {
    $totalWinnings += ($i + 1) * $handList[$i]->bidAmount;
}

echo "The total winnings are: $totalWinnings\n";

