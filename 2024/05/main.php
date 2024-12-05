<?php

declare(strict_types=1);

require_once 'Rule.php';
require_once 'Update.php';

$file = fopen('input.txt', 'rb');

$ruleList = [];
$updateList = [];

while (false !== $line = fgets($file)) {
    if ('' === trim($line)) {
        break;
    }

    $ruleList[] = new Rule($line);
}

while (false !== $line = fgets($file)) {
    $updateList[] = new Update(trim($line));
}

$total = 0;

/** @var Update $update */
foreach ($updateList as $update) {
    if ($update->isInRightOrder($ruleList)) {
        $total += $update->getMiddlePageNumber();
    }
}

echo "The total is $total\n";