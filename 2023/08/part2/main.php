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
    $startNodeList = get_start_node_list($nodeList);
    $pathData = get_path_data_list($nodeList, $instructionList);

    $endFound = false;
    $numberOfSteps = 0;
    $numberOfInstructions = count($instructionList);

    while (!$endFound) {
        // est-ce qu'on a une position commune à tous les paths ?
        $zPosition = search_common_z_position($pathData, $startNodeList);

        // non : ajouter le nombre d'instructions au nombre de steps et prendre les nodes d'arrivée en nouveaux nodes de départ
        if (false === $zPosition) {
            $numberOfSteps += $numberOfInstructions;
            $startNodeList = get_end_node_list($pathData, $startNodeList);

            echo "$numberOfSteps\n";
            continue;
        }

        // oui : ajouter la position au nombre de steps et mettre endFound à true
        $numberOfSteps += $zPosition;
        $endFound = true;

        echo "$numberOfSteps\n";
    }


    return $numberOfSteps;
}

function get_start_node_list(array $nodeList): array
{
    return array_filter($nodeList, static fn(Node $node) => str_ends_with($node->name, 'A'));
}

function get_path_data_list(array $nodeList, array $instructionList): array
{
    $pathDataList = [];

    foreach ($nodeList as $nodeName => $node) {
        $pathData = [
            'zPositionList' => [],
        ];

        $currentNode = $node;

        foreach ($instructionList as $index => $instruction) {
            $currentNode = get_next_node($nodeList, $currentNode, $instruction);

            if ($currentNode->endWithZ) {
                $pathData['zPositionList'][] = $index + 1;
            }
        }

        $pathData['endNode'] = $currentNode;
        $pathDataList[$nodeName] = $pathData;
    }

    return $pathDataList;
}

function get_next_node(array $nodeList, Node $currentNode, string $instruction): Node
{
    if ('L' === $instruction) {
        return $nodeList[$currentNode->left];
    }

    return $nodeList[$currentNode->right];
}

function search_common_z_position(array $pathData, array $startNodeList): int|false
{
    $zPositionList = [];

    foreach ($startNodeList as $node) {
        $zPositionList[] = $pathData[$node->name]['zPositionList'];
    }

    $commonZPosition = array_intersect(...$zPositionList);

    if ([] === $commonZPosition) {
        return false;
    }

    return current($commonZPosition);
}

function get_end_node_list(array $pathData, array $startNodeList): array
{
    $endNodeList = [];

    foreach ($startNodeList as $node) {
        $endNodeList[] = $pathData[$node->name]['endNode'];
    }

    return $endNodeList;
}
