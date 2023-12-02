<?php

declare(strict_types=1);

class Bag
{
    private array $cubes;

    public function __construct(
        int $redCubes,
        int $greenCubes,
        int $blueCubes,
    ) {
        $this->cubes['red'] = $redCubes;
        $this->cubes['green'] = $greenCubes;
        $this->cubes['blue'] = $blueCubes;
    }

    public function isValidGame(Game $game): bool
    {
        foreach ($game->getRounds() as $round) {
            if (!$this->isValidRound($round)) {
                return false;
            }
        }

        return true;
    }

    private function isValidRound(array $round): bool
    {
        foreach ($round as $color => $count) {
            if ($this->cubes[$color] < $count) {
                return false;
            }
        }

        return true;
    }
}