<?php

class File
{
    private string $name;
    private string $size;

    /**
     * @param string $name
     * @param string $size
     */
    public function __construct(string $name, string $size)
    {
        $this->name = $name;
        $this->size = $size;
    }

    /**
     * @return string
     */
    public function getName(): string
    {
        return $this->name;
    }

    public function getSize(): string
    {
        return $this->size;
    }

    public function display(int $indent = 0): void
    {
        for ($i = 0; $i < $indent; ++$i) {
            echo ' ';
        }

        echo '- '.$this->name.' (file, size = '.$this->size.")\n";
    }
}

// Directory is already taken by PHP
class Dir
{
    private string $name;
    /** @var array<Dir|File> */
    private array $content = [];
    private ?Dir $parentDir;

    public function __construct(string $name, ?Dir $parentDir = null)
    {
        $this->name = $name;
        $this->parentDir = $parentDir;
    }

    public function getName(): string
    {
        return $this->name;
    }

    public function getContent(): array
    {
        return $this->content;
    }

    public function getParentDir(): ?Dir
    {
        return $this->parentDir;
    }

    public function addDir(string $dirName): void
    {
        $this->content[] = new Dir($dirName, $this);
    }

    public function addFile(string $fileName, int $size): void
    {
        $this->content[] = new File($fileName, $size);
    }

    // Return child dir with given name
    public function getChildDir(string $dirName): ?Dir
    {
        foreach ($this->content as $childNode) {
            if ($childNode instanceof self && $childNode->getName() === $dirName) {
                return $childNode;
            }
        }

        return null;
    }

    // Return child dir list
    public function getChildDirList(): array
    {
        return array_filter($this->content, static function($childNode) {
            return $childNode instanceof self;
        });
    }

    public function display(int $indent = 0): void
    {
        for ($i = 0; $i < $indent; ++$i) {
            echo ' ';
        }

        echo '- '.$this->name." (dir, size = ".$this->getSize().")\n";

        foreach ($this->content as $childNode) {
            $childNode->display($indent + 2);
        }
    }

    // Get the size of this dir
    public function getSize(): int
    {
        $size = 0;

        foreach ($this->content as $childNode) {
            $size += $childNode->getSize();
        }

        return $size;
    }

    // Get the size of all dir
    // Name of dir is not stored because they can have the same name
    public function getSizeList(): array
    {
        $tempList = [];

        foreach ($this->getChildDirList() as $childDir) {
            $tempList[] = $childDir->getSizeList();
        }

        return array_merge(
            [$this->getSize()],
            ...$tempList
        );
    }
}

$instructionList = explode("\n", file_get_contents('input.txt'));

$i = 0;
$iMax = count($instructionList) - 1;

/** @var Dir $rootDir */
$rootDir = null;
/** @var Dir $currentDir */
$currentDir = null;

// Parse instructions
while ($i <= $iMax) {
    $instruction = explode(' ', $instructionList[$i]);
    ++$i;

    if ('$' === $instruction[0]) {
        // Nothing to do here
        if ('ls' === $instruction[1]) {
            continue;
        }

        // Let's change current dir
        switch ($instruction[2]) {
            case '..':
                $currentDir = $currentDir->getParentDir();
                break;
            case '/':
                $rootDir = new Dir('/');
                $currentDir = $rootDir;
                break;
            default:
                $currentDir = $currentDir->getChildDir($instruction[2]);

                // Shouldn't happen, put here just in case
                if (null === $currentDir) {
                    die();
                }
        }

        continue;
    }

    if ('dir' === $instruction[0]) {
        $currentDir->addDir($instruction[1]);

        continue;
    }

    $currentDir->addFile($instruction[1], (int) $instruction[0]);

}


// $rootDir->display();
$answerPart1 = 0;

$sizeList = $rootDir->getSizeList();

// Let's check those sizes
foreach ($sizeList as $name => $size) {
    if ($size <= 100000) {
        $answerPart1 += $size;
    }
}

echo "The answer for part 1 is $answerPart1\n";