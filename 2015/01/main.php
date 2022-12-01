<?php

$input = file_get_contents('input.txt');

$floor = 0;
$instructionList = str_split($input);

foreach ($instructionList as $instruction) {
    switch ($instruction) {
        case '(':
            ++$floor;
            break;
        case ')':
            --$floor;
            break;
    }
}

echo "The final floor is $floor\n";
