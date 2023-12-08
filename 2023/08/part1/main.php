<?php

declare(strict_types=1);

require_once 'Node.php';

$file = fopen('input.txt', 'rb');

$instructionList = str_split(trim(fgets($file)));
// empty line
fgets($file);

$nodeList = [];

while ($line = fgets($file)) {
    $node = new Node(trim($line));
    $nodeList[$node->name] = $node;
}

fclose($file);

function calculate_number_of_steps(array $instructionList, array $nodeList): int
{
    $numberOfSteps = 0;
    $currentNodeName = 'AAA';
    $instructionIndex = 0;
    $numberOfInstructions = count($instructionList);

    while ('ZZZ' !== $currentNodeName) {
        ++$numberOfSteps;

        $currentNodeName = get_next_node_name(
            $nodeList,
            $nodeList[$currentNodeName],
            $instructionList[$instructionIndex]
        );

        $instructionIndex = ($instructionIndex + 1) % $numberOfInstructions;
    }


    return $numberOfSteps;
}

function get_next_node_name(array $nodeList, Node $currentNode, string $instruction): string
{
    if ('L' === $instruction) {
        return $currentNode->left;
    }

    return $currentNode->right;
}

$answerPart1 = calculate_number_of_steps($instructionList, $nodeList);

echo "The number of steps is $answerPart1\n";
