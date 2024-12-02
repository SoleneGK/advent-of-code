<?php

declare(strict_types=1);

readonly class Report
{
    public function __construct(protected array $levels)
    {
    }

    public function isSafe(): bool
    {
        return self::areSafeLevels($this->levels);
    }

    public static function areSafeLevels(array $levels): bool
    {
        $intervals = self::computeIntervals($levels);

        if (!self::isAllIncreasingOrDecreasing($intervals)) {
            return false;
        }

        return self::areIntervalsSafe($intervals);
    }

    protected static function computeIntervals(array $levels): array
    {
        $intervals = [];
        $length = count($levels);

        for ($i = 0; $i < $length - 1; $i++) {
            $intervals[] = $levels[$i] - $levels[$i + 1];
        }

        return $intervals;
    }

    protected static function isAllIncreasingOrDecreasing(array $intervals): bool
    {
        $variationType = null;

        foreach ($intervals as $interval) {
            if (0 === $interval) {
                return false;
            }

            if (null === $variationType) {
                $variationType = $interval > 0 ? 1 : -1;

                continue;
            }

            if ($interval * $variationType < 0) {
                return false;
            }
        }

        return true;
    }

    protected static function areIntervalsSafe(array $intervals): bool
    {
        foreach ($intervals as $interval) {
            if (abs($interval) < 1 || abs($interval) > 3) {
                return false;
            }
        }

        return true;
    }

    public function isSafeWithProblemDampener(): bool
    {
        if ($this->isSafe()) {
            return true;
        }

        foreach ($this->levels as $key => $level) {
            // calcule la liste sans la valeur
            $reducedLevels = $this->levels;
            unset($reducedLevels[$key]);
            $reducedLevels = array_values($reducedLevels);

            // calcule si safe
            if (self::areSafeLevels($reducedLevels))
            {
                return true;
            }
        }

        return false;
    }
}
