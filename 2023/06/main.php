<?php

declare(strict_types=1);

$file = fopen('input.txt', 'rb');

$timeLine = fgets($file);
$distanceLine = fgets($file);

fclose($file);

$timeList = get_time_data($timeLine);
$distanceList = get_distance_data($distanceLine);

function get_time_data(string $line): array
{
    $times = trim(explode(':', $line)[1]);
    $times = preg_replace('!\s+!', ' ', $times);
    return array_map('intval', explode(' ', $times));
}

function get_distance_data(string $line): array
{
    $distances = trim(explode(':', $line)[1]);
    $distances = preg_replace('!\s+!', ' ', $distances);
    return array_map('intval', explode(' ', $distances));
}

function get_number_of_ways_to_win(int $time, int $recordDistance): int
{
    /**
     * soit s la vitesse (starting speed)
     *      t le temps de la course
     *      d la distance à battre
     *
     * la distance parcourue est de (t - s) * s, notée dp
     * dp est nulle pour s = 0 et s = t
     * on sait par l'énoncé que dp est positive pour s ∈ [0, t]
     *
     * on veut le nombre de valeurs entières de s pour lesquelles dp = d
     * soit (t - s) * s > d
     * les solutions pour (t - s) * s = d sont t/2 ± √(t²/4 - d)
     *
     * on prend l'arrondi à l'entier inférieur de la plus petite solution + 1
     * et l'arrondi à l'entier supérieur de la plus grande solution - 1
     * ce qui donne les bornes des solutions entières de l'inéquation
     */

    $smallerSolution = (int) floor($time / 2 - sqrt($time ** 2 / 4 - $recordDistance) + 1);
    $biggerSolution = (int) ceil($time / 2 + sqrt($time ** 2 / 4 - $recordDistance) - 1);

    return $biggerSolution - $smallerSolution + 1;
}

$part1Solution = 1;

foreach ($timeList as $key => $time) {
    $numberOfSolutions = get_number_of_ways_to_win($time, $distanceList[$key]);
    $part1Solution *= $numberOfSolutions;
}

echo "The solutions for part 1 is $part1Solution\n";


function get_time_part2(string $line): int
{
    $times = trim(explode(':', $line)[1]);
    return (int) preg_replace('!\s+!', '', $times);
}

function get_distance_part2(string $line): int
{
    $distances = trim(explode(':', $line)[1]);
    return (int) preg_replace('!\s+!', '', $distances);
}


$timePart2 = get_time_part2($timeLine);
$distancePart2 = get_distance_part2($distanceLine);

echo 'The solution for part 2 is ' . get_number_of_ways_to_win($timePart2, $distancePart2) . PHP_EOL;
