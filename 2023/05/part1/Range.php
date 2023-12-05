<?php

declare(strict_types=1);

readonly class Range
{
    public function __construct(
        public int $destinationStartRange,
        public int $sourceStartRange,
        public int $rangeLength
    ) {
    }

    public function handle(int $value): bool
    {
        return $value >= $this->sourceStartRange && $value < $this->sourceStartRange + $this->rangeLength;
    }

    public function convert(int $value): int
    {
        return $value - $this->sourceStartRange + $this->destinationStartRange;
    }
}