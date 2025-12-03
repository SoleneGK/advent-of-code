<?php

declare(strict_types=1);

class Range
{
    private int $firstId;
    private int $lastId;

    public function __construct(string $range)
    {
        $rangeData = explode('-', $range);
        $this->firstId = (int) $rangeData[0];
        $this->lastId = (int) $rangeData[1];
    }

    public function getSumOfInvalidIdsForPart1(): int
    {
        $sum = 0;

        for ($id = $this->firstId; $id <= $this->lastId; ++$id) {
            if (!self::isValidForPart1($id)) {
                $sum += $id;
            }
        }

        return $sum;
    }

    private static function isValidForPart1(int $id): bool
    {
        $stringId = (string) $id;
        $length = \strlen($stringId);

        if (1 === $length % 2) {
            return true;
        }

        $firstPart = substr($stringId, 0, $length / 2);
        $secondPart = substr($stringId, $length / 2);

        return $firstPart !== $secondPart;
    }

    public function getSumOfInvalidIdsForPart2(): int
    {
        $sum = 0;

        for ($id = $this->firstId; $id <= $this->lastId; ++$id) {
            if (!self::isValidForPart2($id)) {
                $sum += $id;
            }
        }

        return $sum;
    }

    private static function isValidForPart2(int $id): bool
    {
        $stringId = (string) $id;
        $length = \strlen($stringId);

        for ($i = 1; $i <= floor($length / 2); ++$i) {
            // si on ne peut pas écrire i fois un pattern
            if ($length % $i !== 0) {
                continue;
            }

            $numberOfRepetitions = $length / $i;
            $pattern = substr($stringId, 0, $i);
            $potentialPattern = '';

            for ($j = 1; $j <= $numberOfRepetitions; ++$j) {
                $potentialPattern .= $pattern;
            }

            if ($potentialPattern === $stringId) {
                return false;
            }
        }

        return true;
    }
}
