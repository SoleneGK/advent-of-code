<?php

class Node
{
    private readonly int $row;
    private readonly int $col;
    /** @var array<Node> */
    private array $accessibleNeighbours = [];
    private readonly int $altitude;
    private ?int $distance = null;
    private bool $visited = false;

    public function __construct(int $row, int $col, string $altitude)
    {
        $this->row = $row;
        $this->col = $col;
        $this->setAltitude($altitude);
    }

    public function getCoordinates(): string
    {
        return 'row: '.$this->row.' col:'.$this->col;
    }

    private function setAltitude(string $altitude): void
    {
        $this->altitude = match ($altitude) {
            'S' => 0,
            'E' => 25,
            default => ord($altitude) - ord('a'),
        };
    }

    public function getAltitude(): int
    {
        return $this->altitude;
    }

    public function getDistance(): ?int
    {
        return $this->distance;
    }

    public function isVisited(): bool
    {
        return $this->visited;
    }

    public function setDistance(?int $distance): void
    {
        $this->distance = $distance;
    }

    public function addNeighbour(Node $neighbour): void
    {
        if ($neighbour->getAltitude() <= $this->altitude + 1) {
            $this->accessibleNeighbours[] = $neighbour;
        }
    }

    public function getUnvisitedNeighbours(): array
    {
        $nodeList = [];

        foreach ($this->accessibleNeighbours as $neighbour) {
            if (!$neighbour->isVisited()) {
                $nodeList[] = $neighbour;
            }
        }

        return $nodeList;
    }

    public function visit(): void
    {
        $this->visited = true;

        foreach ($this->accessibleNeighbours as $neighbour) {
            if ($neighbour->isVisited()) {
                continue;
            }

            if (
                null === $neighbour->getDistance()
                || $neighbour->getDistance() > $this->distance + 1
            ) {
                $neighbour->setDistance($this->distance + 1);
            }
        }
    }

    public function reset(): void
    {
        $this->visited = false;
        $this->distance = null;
    }
}

class NodeList
{
    private array $nodeList = [];
    private ?Node $startNode = null;
    private ?Node $targetNode = null;
    private array $nodesToVisit = [];

    public function __construct(array $map)
    {
        foreach ($map as $row => $line) {
            $squareList = str_split($line);

            foreach ($squareList as $col => $square) {
                $this->nodeList[$row][$col] = new Node($row, $col, $square);

                if ('S' === $square) {
                    $this->startNode = $this->nodeList[$row][$col];
                    continue;
                }

                if ('E' === $square) {
                    $this->targetNode = $this->nodeList[$row][$col];
                }
            }
        }

        $this->setNeighbours();
    }

    private function setNeighbours(): void
    {
        $rowNumber = count($this->nodeList);
        $colNumber = count($this->nodeList[0]);

        foreach ($this->nodeList as $row => $line) {
            foreach($line as $col => &$node) {
                if ($row > 0) {
                    $node->addNeighbour($this->nodeList[$row-1][$col]);
                }

                if ($row < $rowNumber - 1) {
                    $node->addNeighbour($this->nodeList[$row+1][$col]);
                }

                if ($col > 0) {
                    $node->addNeighbour($this->nodeList[$row][$col-1]);
                }

                if ($col < $colNumber - 1) {
                    $node->addNeighbour($this->nodeList[$row][$col+1]);
                }
            }
        }
    }

    public function display(): void
    {
        echo 'Node list:';

        /** @var Node $node */
        foreach ($this->nodeList as $node) {
            echo "\n".$node->getCoordinates();
        }

        echo "\n";
    }

    public function findShortestPath(): ?int
    {
        $this->resetNodes();
        $this->nodesToVisit = [$this->startNode];

        while (!empty($this->nodesToVisit)) {
            $currentNode = $this->getClosestNode();

            if ($currentNode === $this->targetNode) {
                break;
            }

            $currentNode->visit();
            $this->removeNode($currentNode);

            foreach ($currentNode->getUnvisitedNeighbours() as $node) {
                $this->addNode($node);
            }
        }

        return $this->targetNode->getDistance();
    }

    private function getClosestNode(): Node
    {
        $closestNode = null;

        foreach ($this->nodesToVisit as $node) {
            if (
                null === $closestNode
                || $node->getDistance() < $closestNode->getDistance()
            ) {
                $closestNode = $node;
            }
        }

        return $closestNode;
    }

    private function removeNode(Node $node): void
    {
        foreach ($this->nodesToVisit as $key => $item) {
            if ($item === $node) {
                unset($this->nodesToVisit[$key]);

                break;
            }
        }
    }

    private function addNode(Node $node): void
    {
        if (!in_array($node, $this->nodesToVisit, true)) {
            $this->nodesToVisit[] = $node;
        }
    }

    public function getOptimalPath(): ?int
    {
        $shortestPath = null;

        /** @var Node $node */
        foreach ($this->nodeList as $line) {
            foreach ($line as $node) {
                if (0 !== $node->getAltitude()) {
                    continue;
                }

                $this->startNode = $node;
                $pathLength = $this->findShortestPath();

                if (
                    null === $shortestPath
                    || (
                        null !== $pathLength
                        && $pathLength < $shortestPath
                    )
                ) {
                    $shortestPath = $pathLength;
                }
            }
        }

        return $shortestPath;
    }

    private function resetNodes(): void
    {
        foreach ($this->nodeList as $line) {
            foreach ($line as $node) {
                $node->reset();
            }
        }
    }
}


$map = explode("\n",  file_get_contents('input.txt'));
$nodeList = new NodeList($map);

$answerPart1 = $nodeList->findShortestPath();
echo "The answer for part 1 is $answerPart1\n";

/**
 * Note : it worked because my computer is powerful enough
 * but using Dijkstra for each combination is not efficient
 * it would be better to compute distances for all nodes by
 * taking E as start node, then parse the nodes to search for
 * the shortest path in 0-altitude nodes
 */
$answerPart2 = $nodeList->getOptimalPath();
echo "The answer for part 2 is $answerPart2\n";
