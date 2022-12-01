<?php

$secretKey = 'ckczppom';

$i = 1;

while (!str_starts_with(md5($secretKey.$i), '000000')) {
	++$i;
}

echo "The answer for part 1 is $i\n";