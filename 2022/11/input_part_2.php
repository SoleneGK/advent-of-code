<?php

$monkeyList = [
    0 => new Monkey(
        itemList: [54, 53],
        operation: fn(Item $value): Item => $value->multiply(3),
        testValue: 2,
        number: 0),
    1 => new Monkey(
        itemList: [95, 88, 75, 81, 91, 67, 65, 84],
        operation: fn(Item $value): Item => $value->multiply(11),
        testValue: 7,
        number: 1),
    2 => new Monkey(
        itemList: [76, 81, 50, 93, 96, 81, 83],
        operation: fn(Item $value): Item => $value->add(6),
        testValue: 3,
        number: 2),
    3 => new Monkey(
        itemList: [83, 85, 85, 63],
        operation: fn(Item $value): Item => $value->add(4),
        testValue: 11,
        number: 3),
    4 => new Monkey(
        itemList: [85, 52, 64],
        operation: fn(Item $value): Item => $value->add(8),
        testValue: 17,
        number: 4),
    5 => new Monkey(
        itemList: [57],
        operation: fn(Item $value): Item => $value->add(2),
        testValue: 5,
        number: 5),
    6 => new Monkey(
        itemList: [60, 95, 76, 66, 91],
        operation: fn(Item $value): Item => $value->square(),
        testValue: 13,
        number: 6),
    7 => new Monkey(
        itemList: [65, 84, 76, 72, 79, 65],
        operation: fn(Item $value): Item => $value->add(5),
        testValue: 19,
        number: 7),
];

$monkeyList[0]->setTargetMonkeyIfTestPass($monkeyList[2]);
$monkeyList[0]->setTargetMonkeyIfTestFail($monkeyList[6]);

$monkeyList[1]->setTargetMonkeyIfTestPass($monkeyList[3]);
$monkeyList[1]->setTargetMonkeyIfTestFail($monkeyList[4]);

$monkeyList[2]->setTargetMonkeyIfTestPass($monkeyList[5]);
$monkeyList[2]->setTargetMonkeyIfTestFail($monkeyList[1]);

$monkeyList[3]->setTargetMonkeyIfTestPass($monkeyList[7]);
$monkeyList[3]->setTargetMonkeyIfTestFail($monkeyList[4]);

$monkeyList[4]->setTargetMonkeyIfTestPass($monkeyList[0]);
$monkeyList[4]->setTargetMonkeyIfTestFail($monkeyList[7]);

$monkeyList[5]->setTargetMonkeyIfTestPass($monkeyList[1]);
$monkeyList[5]->setTargetMonkeyIfTestFail($monkeyList[3]);

$monkeyList[6]->setTargetMonkeyIfTestPass($monkeyList[2]);
$monkeyList[6]->setTargetMonkeyIfTestFail($monkeyList[5]);

$monkeyList[7]->setTargetMonkeyIfTestPass($monkeyList[6]);
$monkeyList[7]->setTargetMonkeyIfTestFail($monkeyList[0]);