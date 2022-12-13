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


$rawPacketList = array_filter($input, static fn($value) => '' !== $value);

$firstDividerPacket = [[2]];
$lastDividerPacket = [[6]];

$packetList = [
    0 => $firstDividerPacket,
    1 => $lastDividerPacket,
];

foreach ($rawPacketList as $packet) {
    eval("\$packetList[] = $packet;");
}

usort($packetList, static fn(array $a, array $b) => isInRightOrder($a, $b) ? -1 : 1);

$firstDividerIndex = 0;
$lastDividerIndex = 0;

foreach ($packetList as $key => $packet) {
    if ($packet === $firstDividerPacket) {
        $firstDividerIndex = $key + 1;

        continue;
    }

    if ($packet === $lastDividerPacket) {
        $lastDividerIndex = $key + 1;

        break;
    }
}

$decoderKey = $firstDividerIndex * $lastDividerIndex;

echo "The answer for part 2 is $decoderKey\n";