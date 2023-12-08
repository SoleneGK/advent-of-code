<?php

declare(strict_types=1);

readonly class Node
{
    public string $name;
    public string $left;
    public string $right;
    public bool $endWithZ;

    public function __construct(string $line)
    {
        $this->name = substr($line, 0, 3);
        $this->left = substr($line, 7, 3);
        $this->right = substr($line, 12, 3);

        $this->endWithZ = str_ends_with($this->name, 'Z');
    }
}