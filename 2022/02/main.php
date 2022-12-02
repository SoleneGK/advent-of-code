<?php

$file = fopen('input.txt', 'rb');

enum Choice
{
    case Rock;
    case Paper;
    case Scissors;
}

function getChoice(string $rawChoice): Choice
{
    return match ($rawChoice) {
        'A', 'X' => Choice::Rock,
        'B', 'Y' => Choice::Paper,
        'C', 'Z' => Choice::Scissors,
    };
}

function getChoiceScore(Choice $choice): int
{
    return match ($choice) {
        Choice::Rock => 1,
        Choice::Paper => 2,
        Choice::Scissors => 3,
    };
}

function getOutcomeScore(Choice $playerChoice, Choice $opponentChoice): int
{
    return match ([$playerChoice, $opponentChoice]) {
        [Choice::Rock, Choice::Rock],
            [Choice::Paper, Choice::Paper],
            [Choice::Scissors, Choice::Scissors]
            => 3,
        [Choice::Rock, Choice::Paper],
            [Choice::Scissors, Choice::Rock],
            [Choice::Paper, Choice::Scissors]
            => 0,
        [Choice::Rock, Choice::Scissors],
            [Choice::Paper, Choice::Rock],
            [Choice::Scissors, Choice::Paper]
            => 6,
    };
}

enum Outcome
{
    case Loss;
    case Draw;
    case Win;
}

function getOutcome(string $rawOutcome): Outcome
{
    return match ($rawOutcome) {
        'X' => Outcome::Loss,
        'Y' => Outcome::Draw,
        'Z' => Outcome::Win,
    };
}

function computeChoice(Choice $opponentChoice, Outcome $wantedOutcome): Choice
{
    return match ([$opponentChoice, $wantedOutcome]) {
        [Choice::Rock, Outcome::Loss],
            [Choice::Scissors, Outcome::Draw],
            [Choice::Paper, Outcome::Win]
            => Choice::Scissors,
        [Choice::Paper, Outcome::Loss],
            [Choice::Rock, Outcome::Draw],
            [Choice::Scissors, Outcome::Win]
            => Choice::Rock,
        [Choice::Scissors, Outcome::Loss],
            [Choice::Paper, Outcome::Draw],
            [Choice::Rock, Outcome::Win]
            => Choice::Paper,
    };
}

$scorePart1 = 0;
$scorePart2 = 0;

while (false !== $line = fgets($file)) {
    [$opponentData, $playerData] = explode(' ', $line);
    $playerData = trim($playerData);

    $opponentChoice = getChoice($opponentData);

    // Part 1
    $playerChoicePart1 = getChoice($playerData);
    $scorePart1 += getChoiceScore($playerChoicePart1)
        + getOutcomeScore($playerChoicePart1, $opponentChoice);

    // Part 2
    $wantedOutcome = getOutcome($playerData);
    $playerChoicePart2 = computeChoice($opponentChoice, $wantedOutcome);
    $scorePart2 += getChoiceScore($playerChoicePart2)
        + getOutcomeScore($playerChoicePart2, $opponentChoice);
}

echo "The total score for part 1 would be $scorePart1\n";
echo "The total score for part 2 would be $scorePart2\n";