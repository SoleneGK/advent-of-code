<?php

declare(strict_types=1);

class Map
{
    public readonly string $sourceName;
    public readonly string $destinationName;
    public array $limits = [0];
    public array $conversionRules = [0 => 0];

    public function setName(string $name): void
    {
        if (isset($this->name)) {
            return;
        }

        $fullName = explode(' ', $name)[0];
        [$this->sourceName, $this->destinationName] = explode('-to-', $fullName);
    }

    public function addLine(string $line): void
    {
        [$destinationStartRange, $sourceStartRange, $rangeLength] = array_map('intval', explode(' ', $line));
        $nextRangeStart = $sourceStartRange + $rangeLength;
        $operation = $destinationStartRange - $sourceStartRange;

        // on conserve seulement la valeur à ajouter ou enlever
        if (!isset($this->conversionRules[$sourceStartRange])) {
            $this->limits[] = $sourceStartRange;
        }

        $this->conversionRules[$sourceStartRange] = $operation;

        if (!isset($this->conversionRules[$nextRangeStart])) {
            $this->limits[] = $nextRangeStart;
            $this->conversionRules[$nextRangeStart] = 0;
        }

        sort($this->limits);
    }

    public function convert(Range $range): array
    {
        $convertedRangeList = [];
        $currentFirstValueToProcess = $range->start;
        $currentLastValueToProcess = $range->end;

        $iMax = count($this->limits) - 1;

        foreach ($this->limits as $i => $limit) {
            // after this number, every value returns itself
            if ($i === $iMax) {
                $convertedRangeList[] = new Range($currentFirstValueToProcess, $currentLastValueToProcess);

                break;
            }

            // check of values to convert are in current range of conversion
            if ($currentFirstValueToProcess >= $this->limits[$i + 1]) {
                continue;
            }

            $endOfRange = min($currentLastValueToProcess, $this->limits[$i + 1] - 1);
            $operation = $this->conversionRules[$limit];
            $convertedRangeList[] = new Range($currentFirstValueToProcess + $operation, $endOfRange + $operation);

            if ($currentLastValueToProcess < $this->limits[$i + 1]) {
                break;
            }

            $currentFirstValueToProcess = $this->limits[$i + 1];
        }

        return $convertedRangeList;
    }
}