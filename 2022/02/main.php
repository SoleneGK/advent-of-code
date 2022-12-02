<?php

$file = fopen('input.txt', 'rb');

$conversion = [
    'A' => 'Rock',
    'B' => 'Paper',
    'C' => 'Scissors',
    'X' => 'Rock',
    'Y' => 'Paper',
    'Z' => 'Scissors',
];

$choiceScore = [
    'Rock' => 1,
    'Paper' => 2,
    'Scissors' => 3,
];

function getOutcomeScore(string $playerChoice, string $opponentChoice): int
{
    // Paper / Paper - Rock / Rock - Scissors / Scissors
    if ($playerChoice === $opponentChoice) {
        return 3;
    }

    if ('Rock' === $playerChoice) {
        // Rock / Paper
        if ('Paper' === $opponentChoice) {
            return 0;
        }

        // Rock / Scissors
        return 6;
    }

    if ('Paper' === $playerChoice) {
        // Paper / Rock
        if ('Rock' === $opponentChoice) {
            return 6;
        }

        // Paper / Scissors
        return 0;
    }

    // Scissors / Rock
    if ('Rock' === $opponentChoice) {
        return 0;
    }

    // Scissors / Paper
    return 6;
}

$score = 0;

while (false !== $line = fgets($file)) {
    [$opponentChoice, $playerChoice] = explode(' ', $line);
    $playerChoice = trim($playerChoice);

    $opponentChoice = $conversion[$opponentChoice];
    $playerChoice = $conversion[$playerChoice];

    $score += $choiceScore[$playerChoice] + getOutcomeScore($playerChoice, $opponentChoice);
}

echo "The total score for part 1 would be $score\n";