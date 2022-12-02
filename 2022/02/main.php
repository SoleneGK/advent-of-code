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

/**
 * Pour $wantedOutcome :
 * X = défaite
 * Y = égalité
 * Z = victoire
 */
function computeChoice(string $opponentChoice, string $wantedOutcome): string
{
    if ('Y' === $wantedOutcome) {
        return $opponentChoice;
    }

    if ('X' === $wantedOutcome) {
        return match ($opponentChoice) {
            'Rock' => 'Scissors',
            'Paper' => 'Rock',
            default => 'Paper',
        };
    }

    return match ($opponentChoice) {
        'Rock' => 'Paper',
        'Paper' => 'Scissors',
        default => 'Rock',
    };
}

$scorePart1 = 0;
$scorePart2 = 0;

while (false !== $line = fgets($file)) {
    [$opponentData, $playerData] = explode(' ', $line);
    $playerData = trim($playerData);

    $opponentChoice = $conversion[$opponentData];

    // Part 1
    $playerChoicePart1 = $conversion[$playerData];
    $scorePart1 += $choiceScore[$playerChoicePart1]
        + getOutcomeScore($playerChoicePart1, $opponentChoice);

    // Part 2
    $playerChoicePart2 = computeChoice($opponentChoice, $playerData);
    $scorePart2 += $choiceScore[$playerChoicePart2]
        + getOutcomeScore($playerChoicePart2, $opponentChoice);
}

echo "The total score for part 1 would be $scorePart1\n";
echo "The total score for part 2 would be $scorePart2\n";