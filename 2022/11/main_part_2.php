<?php

/**
 * For part 2, storing the entire number isn't possible: values go too high
 * However, it is possible to store only the modulos!
 * (a + b) % c = ((a % c) + b) % c
 * (a * b) % c = ((a % c) * b) % c
 * (a * a) % c = ((a % c) * (a % c)) % c
 */
class Item
{
    // I'm storing numbers for the example and the input
    private const NUMBERS = [2, 3, 5, 7, 11, 13, 17, 19, 23];
    private array $moduloList = [];

    public function __construct(int $initialValue)
    {
        foreach (self::NUMBERS as $number) {
            $this->moduloList[$number] = $initialValue % $number;
        }
    }

    public function add(int $value): self
    {
        foreach (self::NUMBERS as $number) {
            $this->moduloList[$number] = ($this->moduloList[$number] + $value) % $number;
        }

        return $this;
    }

    public function multiply(int $value): self
    {
        foreach (self::NUMBERS as $number) {
            $this->moduloList[$number] = ($this->moduloList[$number] * $value) % $number;
        }

        return $this;
    }

    public function square(): self
    {
        foreach (self::NUMBERS as $number) {
            $this->moduloList[$number] = ($this->moduloList[$number] * $this->moduloList[$number]) % $number;
        }

        return $this;
    }

    /**
     * @throws Exception
     */
    public function isDivisibleBy(int $value): bool
    {
        if (!in_array($value, self::NUMBERS)) {
            throw new Exception('Invalid value '.$value);
        }

        return 0 === $this->moduloList[$value];
    }
}

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
        foreach($itemList as $item) {
            $this->itemList[] = new Item($item);
        }

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

            $target = $this->testItem($item) ? $this->targetMonkeyIfTestPass : $this->targetMonkeyIfTestFail;
            $this->giveItem($item, $target);
        }
    }

    public function receiveItem(Item $worryLevel): void
    {
        $this->itemList[] = $worryLevel;
    }

    private function giveItem(Item $worryLevel, Monkey $target): void
    {
        $target->receiveItem($worryLevel);
    }

    private function inspectItem(Item $worryLevel): Item
    {
        ++$this->numberOfItemsInspected;

        return ($this->operation)($worryLevel);
    }

    /**
     * @throws Exception
     */
    private function testItem(Item $worryLevel): bool
    {
        return $worryLevel->isDivisibleBy($this->testValue);
    }
}

function playRound(array $monkeyList): void
{
    foreach ($monkeyList as $monkey) {
        $monkey->playTurn();
    }
}

include 'input_part_2.php';

$numberOfRounds = 10000;

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

echo "The level of monkey business after $numberOfRounds rounds of stuff-slinging simian shenanigans is $levelOfMonkeyBusiness\n";


