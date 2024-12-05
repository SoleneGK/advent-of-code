<?php

declare(strict_types=1);

class Update
{
    protected array $pageList;

    public function __construct(string $rawPageList)
    {
        $pageList = explode(',', $rawPageList);
        $this->pageList = array_map('intval', $pageList);
    }

    public function isInRightOrder(array $ruleList): bool
    {
        foreach ($ruleList as $rule) {
            if (!$this->respectsRule($rule)) {
                return false;
            }
        }

        return true;
    }

    public function respectsRule(Rule $rule): bool
    {
        $firstPageIndex = array_search($rule->firstPage, $this->pageList, true);
        $secondPageIndex = array_search($rule->secondPage, $this->pageList, true);

        // If the page numbers are not present, it doesn't invalidate the update
        if (false === $firstPageIndex || false === $secondPageIndex) {
            return true;
        }

        return $firstPageIndex < $secondPageIndex;
    }

    public function getMiddlePageNumber(): int
    {
        $index = (count($this->pageList) - 1) / 2;

        return $this->pageList[$index];
    }

    public function sort(array $sortRules): void
    {
        usort($this->pageList, static fn ($a, $b) => $sortRules[$a][$b]);
    }
}
