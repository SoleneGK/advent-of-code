<?php

$file = fopen('input.txt', 'rb');

$containNumber = 0;
$overlapNumber = 0;

while (false !== $line = fgets($file)) {
    [$elf1Assignment, $elf2Assignment] = explode(',', trim($line));

    [$elf1AssignmentStart, $elf1AssignmentEnd] = explode('-', $elf1Assignment);
    [$elf2AssignmentStart, $elf2AssignmentEnd] = explode('-', $elf2Assignment);

    if (
        (
            // 1 contains 2
            $elf1AssignmentStart <= $elf2AssignmentStart
            && $elf1AssignmentEnd >= $elf2AssignmentEnd
        ) || (
            // 2 contains 1
            $elf1AssignmentStart >= $elf2AssignmentStart
            && $elf1AssignmentEnd <= $elf2AssignmentEnd
        )
    ) {
        ++$containNumber;
        ++$overlapNumber;

        continue;
    }

    if (
        (
            // Start for 1 is included in assignment for 2
            $elf1AssignmentStart >= $elf2AssignmentStart
            && $elf1AssignmentStart <= $elf2AssignmentEnd
        ) || (
            // End for 1 is included in assignment for 2
            $elf1AssignmentEnd >= $elf2AssignmentStart
            && $elf1AssignmentEnd <= $elf2AssignmentEnd
        )
        // No need to check the same with 1 and 2 inverted
        // If 1 overlaps 2, then 2 overlaps 1
        // Only case not handled is when 2 is contained in 1
        // This case is cheched before
    ) {
        ++$overlapNumber;
    }
}

echo "The answer for part 1 is $containNumber\n";
echo "The answer for part 2 if $overlapNumber\n";