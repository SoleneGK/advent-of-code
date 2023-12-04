<?php

declare(strict_types=1);

require_once 'Card.php';

$file = fopen('input.txt', 'rb');

$cardList = [];

while (false !== $line = fgets($file)) {
    $card = new Card(trim($line));
    $cardList[$card->number] = $card;
}

fclose($file);

$score = 0;

foreach ($cardList as $card) {
    $score += $card->score;
}

echo "The score is $score\n";



$cardNumber = [];

foreach ($cardList as $number =>$card) {
    $cardNumber[$number] = 1;
}

foreach ($cardList as $number => $card) {
    if ($card->numberOfMatches > 0) {
        for ($i = 1; $i <= $card->numberOfMatches; $i++) {
            $cardNumber[$number + $i] += $cardNumber[$number];
        }
    }
}

$numberOfCards = array_sum($cardNumber);

echo "The number of cards is $numberOfCards\n";