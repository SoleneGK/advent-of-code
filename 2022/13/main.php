<?php

function isInRightOrder(int|array $left, int|array $right): ?bool
{
    if (is_int($left) && is_int($right)) {
        if ($left === $right) {
            return null;
        }

        return $left < $right;
    }

    if (is_array($left) && is_int($right)) {
        return isInRightOrder($left, [$right]);
    }

    if (is_int($left) && is_array($right)) {
        return isInRightOrder([$left], $right);
    }

    if ([] === $left && [] === $right) {
        return null;
    }

    if ([] === $left) {
        return true;
    }

    if ([] === $right) {
        return false;
    }

    $leftFistItem = array_shift($left);
    $rightFirstItem = array_shift($right);

    return isInRightOrder($leftFistItem, $rightFirstItem) ?? isInRightOrder($left, $right);
}

function stringify(array $array): string
{
    $string = '[';

    foreach ($array as $item) {
        if ($string !== '[') {
            $string .= ',';
        }

        if (is_numeric($item)) {
            $string .= $item;
        } else {
            $string .= stringify($item);
        }
    }

    return $string.']';
}

$input = explode("\n", file_get_contents('input.txt'));

$rawPairList = array_chunk($input, 3);
$pairList = [];

foreach ($rawPairList as $pair) {
    eval("
        \$pairList[] = [
            'left' => $pair[0],
            'right' => $pair[1],
        ];
    ");

}

$pairsInRightOrder = [];

foreach ($pairList as $key => $pair) {
    if (isInRightOrder($pair['left'], $pair['right'])) {
        $pairsInRightOrder[] = $key + 1;
    }
}

$answerPartOne = array_sum($pairsInRightOrder);

echo "The answer for part 1 is $answerPartOne\n";

