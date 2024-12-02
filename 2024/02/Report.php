<?php

declare(strict_types=1);

class Report
{
    protected array $intervals;

    public function __construct(protected readonly array $levels)
    {
        $length = count($levels);

        for ($i = 0; $i < $length - 1; $i++) {
            $this->intervals[] = $levels[$i] - $levels[$i + 1];
        }
    }

    public function isSafe(): bool
    {
        if (!$this->isAllIncreasingOrDecreasing()) {
            return false;
        }

        return $this->areIntervalsSafe();
    }

    protected function isAllIncreasingOrDecreasing(): bool
    {
        $variationType = null;

        foreach ($this->intervals as $interval) {
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

    protected function areIntervalsSafe(): bool
    {
        foreach ($this->intervals as $interval) {
            if (abs($interval) < 1 || abs($interval) > 3) {
                return false;
            }
        }

        return true;
    }
}
