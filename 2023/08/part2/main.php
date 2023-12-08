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

$numberOfSteps = get_number_of_steps($nodeList, $instructionList);

echo "The number of steps is $numberOfSteps\n";

function get_number_of_steps(array $nodeList, array $instructionList): int
{
    // What makes it a particular case: Z nodes are only found at end of a cycle
    $startNodeList = get_start_node_list($nodeList);
    $pathData = get_destination_node_list($nodeList, $instructionList);
    $nodesInALoop = get_nodes_in_a_loop($pathData);

    $loopData = get_loop_data($startNodeList, $pathData, $nodesInALoop);
    // New particularity: for all starting nodes, only 1 step is needed to get into a loop
    // New particularity : Z node is always at position loopLength - 1, so first passage at a Z point is same as loop length
    // New particularity: loop length is always a primary number

    $numberOfSteps = 1;

    foreach ($loopData as $loop) {
        $numberOfSteps *= $loop['loopLength'];
    }

    return $numberOfSteps * count($instructionList);
}

function get_start_node_list(array $nodeList): array
{
    return array_filter($nodeList, static fn(Node $node) => str_ends_with($node->name, 'A'));
}

function get_destination_node_list(array $nodeList, array $instructionList): array
{
    $destinationNodeList = [];

    foreach ($nodeList as $nodeName => $node) {
        $currentNode = $node;

        foreach ($instructionList as $instruction) {
            $currentNode = get_next_node($nodeList, $currentNode, $instruction);
        }

        $destinationNodeList[$nodeName] = $currentNode->name;
    }

    return $destinationNodeList;
}

function get_next_node(array $nodeList, Node $currentNode, string $instruction): Node
{
    if ('L' === $instruction) {
        return $nodeList[$currentNode->left];
    }

    return $nodeList[$currentNode->right];
}

function get_nodes_in_a_loop(array $pathData): array
{
    $nodesInALoop = [];

    foreach ($pathData as $destinationNodeName) {
        if (!isset($nodesInALoop[$destinationNodeName])) {
            $nodesInALoop[$destinationNodeName] = 1;
            continue;
        }

        ++$nodesInALoop[$destinationNodeName];
    }

    return array_keys($nodesInALoop);
}

function get_loop_data(array $startNodeList, array $pathData, array $nodesInALoop): array
{
    $loopList = [];

    foreach ($startNodeList as $startNode) {
        $loopData = [
            'startLength' => 0,
            'loopLength'  => 0,
        ];

        $currentNodeName = $startNode->name;

        // find how many cycles before going into a loop
        while (!in_array($currentNodeName, $nodesInALoop, true)) {
            ++$loopData['startLength'];
            $currentNodeName = $pathData[$currentNodeName];
        }

        // find how many cycles is the loop long
        $startLoopNodeName = $currentNodeName;

        do {
            if (str_ends_with($currentNodeName, 'Z')) {
                $loopData['zPositionInLoop'] = $loopData['loopLength'];
            }

            ++$loopData['loopLength'];
            $currentNodeName = $pathData[$currentNodeName];
        } while ($currentNodeName !== $startLoopNodeName);

        $loopList[$startNode->name] = $loopData;
    }

    return $loopList;
}
