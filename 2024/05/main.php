<?php

declare(strict_types=1);

require_once 'Rule.php';
require_once 'Update.php';

$file = fopen('input.txt', 'rb');

$ruleList = [];
$sortRules = [];
$updateList = [];

while (false !== $line = fgets($file)) {
    if ('' === trim($line)) {
        break;
    }

    $ruleList[] = new Rule($line);

    $pageList = explode('|', $line);
    $firstPage = (int) trim($pageList[0]);
    $secondPage = (int) trim($pageList[1]);

    $sortRules[$firstPage][$secondPage] = 1;
    $sortRules[$secondPage][$firstPage] = -1;
}

while (false !== $line = fgets($file)) {
    $updateList[] = new Update(trim($line));
}

$totalPart1 = 0;
$totalPart2 = 0;

/** @var Update $update */
foreach ($updateList as $update) {
    if ($update->isInRightOrder($ruleList)) {
        $totalPart1 += $update->getMiddlePageNumber();

        continue;
    }

    $update->sort($sortRules);

    $totalPart2 += $update->getMiddlePageNumber();
}

echo "The total for part 1 is $totalPart1\n";
echo "The total for part 2 is $totalPart2\n";

