<?php

declare(strict_types=1);

class Map
{
    public readonly string $sourceName;
    public readonly string $destinationName;
    public array $rangeList;

    public function setName(string $name): void
    {
        if (isset($this->name)) {
            return;
        }

        $fullName = explode(' ', $name)[0];
        [$this->sourceName, $this->destinationName] = explode('-to-', $fullName);
    }

    public function addRange(string $rangeData): void
    {
        [$destinationStartRange, $sourceStartRange, $rangeLength] = explode(' ', $rangeData);
        $this->rangeList[] = new Range(
            (int) $destinationStartRange,
            (int) $sourceStartRange,
            (int) $rangeLength
        );
    }

    public function convert(int $value): int
    {
        foreach ($this->rangeList as $range) {
            if ($range->handle($value)) {
                return $range->convert($value);
            }
        }

        return $value;
    }
}