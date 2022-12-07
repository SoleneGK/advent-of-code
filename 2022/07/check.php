<?php

$sizes = explode("\n", file_get_contents('sizes_ok.txt'));
$sizesToCheck = explode("\n", file_get_contents('sizes_to_check.txt'));

echo count($sizes)."\n";
echo count($sizesToCheck)."\n";


function parse(array $arrayToParse): array
{
    $return = [];

    $i = 1;

    foreach ($arrayToParse as $line) {
        echo $i."\n";
        $lineData = explode(' ', $line);
        $return[$lineData[0]] = (int) $lineData[1];
        ++$i;
    }

    return $return;
}

$sizes = parse($sizes);
$sizesToCheck = parse($sizesToCheck);
echo count($sizes)."\n";
echo count($sizesToCheck)."\n";


if (count($sizes) === count($sizesToCheck)) {
    echo 'Taille OK';
} else {
    echo "ERREUR TAILLE\n";
    echo 'ok : '.count($sizes)."\n";
    echo 'à check : '.count($sizesToCheck)."\n";
}

foreach ($sizes as $name => $value) {
    if (!isset($sizesToCheck[$name])) {
        echo "Valeur manquante pour $name\n";

        continue;
    }

    if ($value !== $sizesToCheck [$name]) {
        echo "Valeurs différentes pour $name\n";
    }
}


