<?php

declare(strict_types=1);

$file = fopen('input.txt', 'rb');

$valueHistoryList = [];

while ($line = fgets($file)) {
    $valueHistoryList[] = array_map('intval', explode(' ', trim($line)));
}

fclose($file);

$valueList = extrapolate_value_list($valueHistoryList);

$sumExtrapolatedValues = array_reduce(
    $valueList,
    static fn(int $carry, array $data) => $carry + $data['next_value'],
    0)
;
$sumExtrapolatedBackwardsValues = array_reduce(
    $valueList,
    static fn(int $carry, array $data) => $carry + $data['previous_value'],
    0)
;

echo "The sum of extrapolated values is $sumExtrapolatedValues\n";
echo "The sum of extrapolated backwards values is $sumExtrapolatedBackwardsValues\n";

function extrapolate_value_list(array $valueHistoryList): array
{
    $extrapolatedValueList = [];

    foreach ($valueHistoryList as $valueHistory) {
        $extrapolatedValueList[] = extrapolate_values($valueHistory);
    }

    return $extrapolatedValueList;
}

function extrapolate_values(array $valueHistory): array
{
    $sequenceList = get_sequence($valueHistory);
    extrapolate($sequenceList);
    extrapolate_backwards($sequenceList);

    return [
        'next_value'     => $sequenceList[0][count($sequenceList[0]) - 2],
        'previous_value' => $sequenceList[0][-1],
    ];
}

function get_sequence(array $valueHistory): array
{
    $sequenceList[] = $valueHistory;
    $currentLine = $valueHistory;

    // calculate next line while next current line is not full 0
    do {
        $nextLine = [];

        for ($i = 0, $iMax = count($currentLine) - 2; $i <= $iMax; $i++) {
            $nextLine[] = $currentLine[$i + 1] - $currentLine[$i];
        }

        $sequenceList[] = $nextLine;
        $currentLine = $nextLine;
    } while (array_filter($currentLine, static fn(int $value) => $value !== 0) !== []);

    return $sequenceList;
}

function extrapolate(array &$sequenceList): void
{
    $lineNumber = count($sequenceList) - 1;
    $sequenceList[$lineNumber][] = 0;

    for ($lineNumber = count($sequenceList) - 2; $lineNumber >= 0; $lineNumber--) {
        $sequenceList[$lineNumber][] = $sequenceList[$lineNumber][count($sequenceList[$lineNumber]) - 1]
            + $sequenceList[$lineNumber + 1][count($sequenceList[$lineNumber + 1]) - 1];
    }
}

function extrapolate_backwards(array &$sequenceList): void
{
    $lineNumber = count($sequenceList) - 1;
    $sequenceList[$lineNumber][-1] = 0;

    for ($lineNumber = count($sequenceList) - 2; $lineNumber >= 0; $lineNumber--) {
        $sequenceList[$lineNumber][-1] = $sequenceList[$lineNumber][0] - $sequenceList[$lineNumber + 1][-1];
    }
}
