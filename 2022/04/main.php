<?php

$file = fopen('input.txt', 'rb');

$containNumber = 0;

while (false !== $line = fgets($file)) {
    [$elf1Assignment, $elf2Assignment] = explode(',', trim($line));

    [$elf1AssignmentStart, $elf1AssignmentEnd] = explode('-', $elf1Assignment);
    [$elf2AssignmentStart, $elf2AssignmentEnd] = explode('-', $elf2Assignment);

    if (
        (
            $elf1AssignmentStart <= $elf2AssignmentStart
            && $elf1AssignmentEnd >= $elf2AssignmentEnd
        ) || (
            $elf1AssignmentStart >= $elf2AssignmentStart
            && $elf1AssignmentEnd <= $elf2AssignmentEnd
        )
    ) {
        ++$containNumber;
    }
}

echo "The answer for part 1 is $containNumber\n";