<?php

declare(strict_types=1);

require_once 'Bank.php';

$file = fopen('input.txt', 'rb');

$bankList = [];
$totalOutputJoltage = 0;

while (false !== $line = fgets($file)) {
    $bank = new Bank(trim($line));
    $bankList[] = $bank;
    $totalOutputJoltage += $bank->getMaxJoltage();
}

echo "The total output joltage is $totalOutputJoltage\n";

