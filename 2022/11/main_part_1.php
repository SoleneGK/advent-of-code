<?php

class Monkey
{
    private readonly int $number;
    private array $itemList = [];
    private readonly Closure $operation;
    private readonly int $testValue;
    private readonly Monkey $targetMonkeyIfTestPass;
    private readonly Monkey $targetMonkeyIfTestFail;
    private int $numberOfItemsInspected = 0;

    public function __construct(array $itemList, Closure $operation, int $testValue, int $number)
    {
        $this->itemList = $itemList;
        $this->operation = $operation;
        $this->testValue = $testValue;
        $this->number = $number;
    }

    public function getNumber(): int
    {
        return $this->number;
    }

    public function getNumberOfItemsInspected(): int
    {
        return $this->numberOfItemsInspected;
    }

    public function setTargetMonkeyIfTestPass(Monkey $monkey): void
    {
        $this->targetMonkeyIfTestPass = $monkey;
    }

    public function displayItemList(): void
    {
        echo 'Monkey '.$this->number.': ';

        foreach ($this->itemList as $key => $item) {
            if (0 !== $key) {
                echo ', ';
            }

            echo $item;
        }

        echo "\n";
    }

    public function setTargetMonkeyIfTestFail(Monkey $monkey): void
    {
        $this->targetMonkeyIfTestFail = $monkey;
    }

    public function playTurn(): void
    {
        $numberOfItems = count($this->itemList);

        for ($i = 0; $i < $numberOfItems; ++$i) {
            $item = array_shift($this->itemList);
            $item = $this->inspectItem($item);
            $item = $this->getBoredWithItem($item);

            $target = $this->testItem($item) ? $this->targetMonkeyIfTestPass : $this->targetMonkeyIfTestFail;
            $this->giveItem($item, $target);
        }
    }

    public function receiveItem(int $worryLevel): void
    {
        $this->itemList[] = $worryLevel;
    }

    private function giveItem(int $worryLevel, Monkey $target): void
    {
        $target->receiveItem($worryLevel);
    }

    private function inspectItem(int $worryLevel): int
    {
        ++$this->numberOfItemsInspected;

        return ($this->operation)($worryLevel);
    }

    public function getBoredWithItem(int $worryLevel): int
    {
        return floor($worryLevel / 3);
    }

    private function testItem(int $worryLevel): bool
    {
        return 0 === $worryLevel % $this->testValue;
    }
}

function playRound(array $monkeyList): void
{
    foreach ($monkeyList as $monkey) {
        $monkey->playTurn();
    }
}

include 'input_part_1.php';

$numberOfRounds = 20;

for ($i = 0; $i < $numberOfRounds; ++$i) {
    playRound($monkeyList);
}

$numberOfInspections = [];

foreach ($monkeyList as $monkey) {
    $numberOfInspections[] = $monkey->getNumberOfItemsInspected();
    echo 'Monkey '.$monkey->getNumber().' inspected items '.$monkey->getNumberOfItemsInspected()." times.\n";
}

echo "\n";

rsort($numberOfInspections);
$levelOfMonkeyBusiness = $numberOfInspections[0] * $numberOfInspections[1];

echo "The level of monkey business after 20 rounds of stuff-slinging simian shenanigans is $levelOfMonkeyBusiness\n";