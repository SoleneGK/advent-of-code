<?php

declare(strict_types=1);

require_once 'Report.php';

$file = fopen('input.txt', 'rb');

$reportList = [];

while (false !== $line = fgets($file)) {
    $levels = array_map('intval', explode(' ', trim($line)));
    $reportList[] = new Report($levels);
}

$numberOfSafeReports = 0;

/** @var Report $report */
foreach ($reportList as $report) {
    if ($report->isSafe()) {
        $numberOfSafeReports++;
    }
}

echo 'There are ' . $numberOfSafeReports . ' safe reports';