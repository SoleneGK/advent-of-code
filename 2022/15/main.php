<?php

class Sensor
{
    private int $row;
    private int $col;
    private int $beaconDistance;

    public function __construct(int $sensorRow, int $sensorCol, int $beaconRow, int $beaconCol)
    {
        $this->row = $sensorRow;
        $this->col = $sensorCol;
        $this->beaconDistance = abs($sensorRow - $beaconRow) + abs($sensorCol - $beaconCol);
    }

    public function getCellsInAreaOnRow(int $row): array
    {
        $cellList = [];

        if (!$this->isRowInExclusionArea($row)) {
            return $cellList;
        }

        $maxColOffset = $this->beaconDistance - abs($this->row - $row);
        $colMin = $this->col - $maxColOffset;
        $colMax = $this->col + $maxColOffset;

        foreach (range($colMin, $colMax) as $col) {
            $cellList[] = $col;
        }

        return $cellList;
    }

    private function isRowInExclusionArea(int $row): bool
    {
        return $row >= $this->row - $this->beaconDistance && $row <= $this->row + $this->beaconDistance;
    }

    public function getInfo(): void
    {
        echo 'x = '.$this->col.', y = '.$this->row.', distance = '.$this->beaconDistance."\n";
    }
}

class Beacon
{
    public int $row;
    public int $col;

    public function __construct(int $row, int $col)
    {
        $this->row = $row;
        $this->col = $col;
    }
}

function getSensorList(array $input): array
{
    $sensorList = [];
    $beaconList = [];

    foreach ($input as $line) {
        [$sensorRawData, $beaconRawData] = explode(':', $line);

        [$rawSensorCol, $rawSensorRow] = explode(',', $sensorRawData);
        $sensorCol = (int) substr($rawSensorCol, strlen('Sensor at x='));
        $sensorRow = (int) substr($rawSensorRow, strlen(' y='));

        [$rawBeaconCol, $rawBeaconRow] = explode(',', $beaconRawData);
        $beaconCol = (int) substr($rawBeaconCol, strlen(' closest beacon is at x='));
        $beaconRow = (int) substr($rawBeaconRow, strlen(' y='));

        $beaconList[] = new Beacon($beaconRow, $beaconCol);
        $sensorList[] = new Sensor($sensorRow, $sensorCol, $beaconRow, $beaconCol);
    }

    return [$sensorList, $beaconList];
}

function getAnswerPart1(array $sensorList, array $beaconList, int $rowToCheck): int
{
    $exclusionArea = [];

    /** @var Sensor $sensor */
    foreach ($sensorList as $sensor) {
        foreach ($sensor->getCellsInAreaOnRow($rowToCheck) as $col) {
            $exclusionArea[$col] = true;
        }
    }

    // Check if a beacon is on an exclusion area cell
    foreach ($beaconList as $beacon) {
        if ($rowToCheck === $beacon->row && isset($exclusionArea[$beacon->col])) {
            unset($exclusionArea[$beacon->col]);
        }
    }

    return count($exclusionArea);
}

$input = explode("\n", file_get_contents('input.txt'));
$rowToCheck = 2000000;

[$sensorList, $beaconList] = getSensorList($input);

$answerPart1 = getAnswerPart1($sensorList, $beaconList, $rowToCheck);
echo "The answer for part 1 is $answerPart1\n";