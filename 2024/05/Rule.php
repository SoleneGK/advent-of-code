<?php

declare(strict_types=1);

readonly class Rule
{
    public int $firstPage;
    public int $secondPage;

    public function __construct(string $rule)
    {
        $pageList = explode('|', $rule);
        $this->firstPage = (int) trim($pageList[0]);
        $this->secondPage = (int) trim($pageList[1]);
    }
}
