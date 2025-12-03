<?php

declare(strict_types=1);

require_once 'Bank.php';

$file = fopen('input.txt', 'rb');

$bankList = [];
$totalOutputJoltageForPart1 = 0;
$totalOutputJoltageForPart2 = 0;

while (false !== $line = fgets($file)) {
    $bank = new Bank(trim($line));
    $bankList[] = $bank;
    $totalOutputJoltageForPart1 += $bank->getMaxJoltageForPart1();
    $totalOutputJoltageForPart2 += $bank->getMaxJoltageForPart2();
}

echo "The total output joltage for part 1 is $totalOutputJoltageForPart1\n";
echo "The total output joltage for part 2 is $totalOutputJoltageForPart2\n";

