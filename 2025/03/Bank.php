<?php

declare(strict_types=1);

class Bank
{
    private array $batteryList;

    public function __construct(string $line)
    {
        $this->batteryList = str_split($line);
    }

    public function getMaxJoltage(): int
    {
        $maxJoltage = 0;
        $numberOfBatteries = count($this->batteryList);

        for ($i = 0; $i < $numberOfBatteries; ++$i) {
            for ($j = $i + 1; $j < $numberOfBatteries; ++$j) {
                $joltage = $this->batteryList[$i] * 10 + $this->batteryList[$j];

                if ($joltage > $maxJoltage) {
                    $maxJoltage = $joltage;
                }
            }
        }

        return $maxJoltage;
    }
}
