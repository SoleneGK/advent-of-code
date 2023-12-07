<?php

declare(strict_types=1);

enum HandType: int
{
    case FiveOfAKind = 6;
    case FourOfAKind = 5;
    case FullHouse = 4;
    case ThreeOfAKind = 3;
    case TwoPair = 2;
    case OnePair = 1;
    case HighCard = 0;

    public static function getHandType(Hand $hand): HandType
    {
        $cards = [];

        for ($i = 1; $i <= 5; $i++) {
            $value = $hand->{'card'. $i}->value;

            if (!isset($cards[$value])) {
                $cards[$value] = 1;
                continue;
            }

            ++$cards[$value];
        }

        if (1 === count($cards)) {
            return self::FiveOfAKind;
        }

        if (5 === count($cards)) {
            return self::HighCard;
        }

        if (4 === count($cards)) {
            return self::OnePair;
        }

        if (2 === count($cards)) {
            $count = current($cards);

            if (1 === $count || 4 === $count) {
                return self::FourOfAKind;
            }

            return self::FullHouse;
        }

        if (\in_array(3, $cards, true)) {
            return self::ThreeOfAKind;
        }

        return self::TwoPair;
    }

    public static function getHigherHand(Hand $hand1, Hand $hand2): int
    {
        if ($hand1->handType->value > $hand2->handType->value) {
            return 1;
        }

        if ($hand1->handType->value < $hand2->handType->value) {
            return -1;
        }

        for ($i = 1; $i <= 5; $i++) {
            if ($hand1->{'card'. $i}->value > $hand2->{'card'. $i}->value) {
                return 1;
            }

            if ($hand1->{'card'. $i}->value < $hand2->{'card'. $i}->value) {
                return -1;
            }
        }

        // should not happen
        return 0;
    }
}
