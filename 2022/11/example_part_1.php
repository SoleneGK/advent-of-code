<?php

$monkeyList = [
    0 => new Monkey(
        itemList: [79, 98],
        operation: fn(int $value): int => $value * 19,
        testValue: 23,
        number: 0
    ),
    1 => new Monkey(
        itemList: [54, 65, 75, 74],
        operation: fn(int $value): int => $value + 6,
        testValue: 19,
        number: 1
    ),
    2 => new Monkey(
        itemList: [79, 60, 97],
        operation: fn(int $value): int => $value * $value,
        testValue: 13,
        number: 2
    ),
    3 => new Monkey(
        itemList: [74],
        operation: fn(int $value): int => $value + 3,
        testValue: 17,
        number: 3
    ),
];

$monkeyList[0]->setTargetMonkeyIfTestPass($monkeyList[2]);
$monkeyList[0]->setTargetMonkeyIfTestFail($monkeyList[3]);

$monkeyList[1]->setTargetMonkeyIfTestPass($monkeyList[2]);
$monkeyList[1]->setTargetMonkeyIfTestFail($monkeyList[0]);

$monkeyList[2]->setTargetMonkeyIfTestPass($monkeyList[1]);
$monkeyList[2]->setTargetMonkeyIfTestFail($monkeyList[3]);

$monkeyList[3]->setTargetMonkeyIfTestPass($monkeyList[0]);
$monkeyList[3]->setTargetMonkeyIfTestFail($monkeyList[1]);