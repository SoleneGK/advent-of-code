<?php

declare(strict_types=1);

class Game
{
    public readonly int $id;
    private array $rounds = [];

    /**
     * @throws Exception
     */
    public function __construct(string $rawData)
    {
        $rawData = substr($rawData, 5);
        [$id, $rounds] = explode(':', $rawData);

        $this->id = (int) $id;

        $this->initRoundList(trim($rounds));
    }

    /**
     * @throws Exception
     */
    private function initRoundList(string $rawRound): void
    {
        $rawRoundList = explode(';', $rawRound);

        foreach ($rawRoundList as $round) {
            $roundData = [
                'red' => 0,
                'green' => 0,
                'blue' => 0,
            ];

            $colorData = explode(',', $round);

            foreach($colorData as $colorUnit) {
                $colorUnit = trim($colorUnit);
                $color = explode(' ', $colorUnit);
                $roundData[$color[1]] = (int) $color[0];
            }

            $this->rounds[] = $roundData;
        }
    }

    public function getRounds(): array
    {
        return $this->rounds;
    }

    public function getMinimalSetPower(): int
    {
        $minimalSet = $this->getMinimalSet();

        return $minimalSet['red'] * $minimalSet['green'] * $minimalSet['blue'];
    }

    public function getMinimalSet(): array
    {
        $minimalSet = [
            'red' => 0,
            'green' => 0,
            'blue' => 0,
        ];

        foreach ($this->rounds as $round) {
            $minimalSet['red'] = max($minimalSet['red'], $round['red']);
            $minimalSet['green'] = max($minimalSet['green'], $round['green']);
            $minimalSet['blue'] = max($minimalSet['blue'], $round['blue']);
        }

        return $minimalSet;
    }
}