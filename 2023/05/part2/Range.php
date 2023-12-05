<?php

declare(strict_types=1);

readonly class Range
{
    public function __construct(
        public int $start,
        public int $end
    ) {
    }
}