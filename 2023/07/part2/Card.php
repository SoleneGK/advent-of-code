<?php

declare(strict_types=1);

enum Card: int
{
    case A = 14;
    case K = 13;
    case Q = 12;
    case T = 10;
    case NINE = 9;
    case EIGHT = 8;
    case SEVEN = 7;
    case SIX = 6;
    case FIVE = 5;
    case FOUR = 4;
    case THREE = 3;
    case TWO = 2;
    case J = 1;


    public static function getCard(string $cardValue): Card
    {
        if (1 !== strlen($cardValue)) {
            throw new InvalidArgumentException();
        }

        return match ($cardValue) {
            'A' => self::A,
            'K' => self::K,
            'Q' => self::Q,
            'J' => self::J,
            'T' => self::T,
            '9' => self::NINE,
            '8' => self::EIGHT,
            '7' => self::SEVEN,
            '6' => self::SIX,
            '5' => self::FIVE,
            '4' => self::FOUR,
            '3' => self::THREE,
            '2' => self::TWO,
            default => throw new InvalidArgumentException(),
        };
    }
}
