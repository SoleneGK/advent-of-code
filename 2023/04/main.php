<?php

declare(strict_types=1);

require_once 'Card.php';

$file = fopen('input.txt', 'rb');

$cardList = [];

while (false !== $line = fgets($file)) {
    $cardList[] = new Card(trim($line));
}

fclose($file);

$score = 0;

foreach ($cardList as $card) {
    echo "The score of the card is $card->score\n";
    $score += $card->score;
}

echo "The score is $score\n";