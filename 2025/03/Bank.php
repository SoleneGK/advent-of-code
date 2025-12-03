<?php

declare(strict_types=1);

class Bank
{
    private array $batteryList;

    public function __construct(string $line)
    {
        $this->batteryList = str_split($line);
    }

    public function getMaxJoltageForPart1(): int
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

    public function getMaxJoltageForPart2(): int
    {
        $solution = $this->findBatteriesToActivate(12, -1);
        $maxJoltage = 0;

        foreach ($solution as $digit) {
            $maxJoltage *= 10;
            $maxJoltage += $digit;
        }

        return $maxJoltage;
    }

    private function findBatteriesToActivate(int $numberOfBatteriesToActivate, int $firstIndex): array
    {
        if ($numberOfBatteriesToActivate <= 0) {
            return [];
        }

        // On ne teste pas les éléments avant le 1er index (qui correspond à la position de la batterie activée précédente)
        // On ne teste pas les $numberOfBatteriesToActivate - 1 derniers éléments, ils ne sont pas des solutions valables
        $length = count($this->batteryList) - $numberOfBatteriesToActivate - $firstIndex;
        $batteriesToTest = array_slice($this->batteryList, $firstIndex + 1, $length);

        $newBatteryValue = max($batteriesToTest);

        if ($numberOfBatteriesToActivate === 1) {
            return [$newBatteryValue];
        }

        // Renvoie toutes les batteries avec la valeur max
        $newBatteryIndexList = array_keys($batteriesToTest, $newBatteryValue);

        // Tester pour toutes les solutions
        $potentialSolutionList = [];

        foreach ($newBatteryIndexList as $newBatteryIndex) {
            $index = $firstIndex + $newBatteryIndex + 1;

            $potentialSolutionList[] = array_merge(
                [$newBatteryValue],
                $this->findBatteriesToActivate($numberOfBatteriesToActivate - 1, $index)
            );
        }

        // Trouver la meilleure solution et la renvoyer
        return $this->getBestSolution($potentialSolutionList);
    }

    private function getBestSolution(array $solutionList): array
    {
        $bestSolution = [];

        foreach ($solutionList as $solution) {
            if ([] === $bestSolution) {
                $bestSolution = $solution;
            }

            $length = \count($solution);

            for ($i = 0; $i < $length; ++$i) {
                if ($bestSolution[$i] < $solution[$i]) {
                    $bestSolution = $solution;

                    break;
                }
            }
        }

        return $bestSolution;
    }
}
