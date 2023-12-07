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

        // let's take care of jokers
        if (!isset($cards[Card::J->value])) {
            return self::getHandTypeByNumberOfCards($cards);
        }

        $jNumber = $cards[Card::J->value];

        if (5 === $jNumber) {
            return self::getHandTypeByNumberOfCards($cards);
        }

        unset($cards[Card::J->value]);

        for ($i = 0; $i < $jNumber; $i++) {
            // get most numerous card
            $mostNumerousCard = self::getMostNumerousCard($cards);
            ++$cards[$mostNumerousCard];
        }

        return self::getHandTypeByNumberOfCards($cards);
    }

    private static function getMostNumerousCard(array $cards): int
    {
        $maxCount = 0;
        $mostNumerousCard = 0;

        foreach ($cards as $value => $count) {
            if ($count > $maxCount) {
                $maxCount = $count;
                $mostNumerousCard = $value;
            }
        }

        return $mostNumerousCard;
    }

    private static function getHandTypeByNumberOfCards(array $numberOfCards): HandType
    {
        if (1 === count($numberOfCards)) {
            return self::FiveOfAKind;
        }

        if (5 === count($numberOfCards)) {
            return self::HighCard;
        }

        if (4 === count($numberOfCards)) {
            return self::OnePair;
        }

        if (2 === count($numberOfCards)) {
            $count = current($numberOfCards);

            if (1 === $count || 4 === $count) {
                return self::FourOfAKind;
            }

            return self::FullHouse;
        }

        if (\in_array(3, $numberOfCards, true)) {
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
