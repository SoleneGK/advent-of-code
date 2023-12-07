<?php

declare(strict_types=1);

readonly class Hand
{
    public Card $card1;
    public Card $card2;
    public Card $card3;
    public Card $card4;
    public Card $card5;
    public HandType $handType;

    public int $bidAmount;

    public function __construct(string $cardData, int $bidAmount)
    {
        $this->bidAmount = $bidAmount;
        $this->getCards($cardData);
        $this->handType = HandType::getHandType($this);
    }

    private function getCards(string $cardData): void
    {
        for ($i = 0; $i < 5; $i++) {
            $cardName = 'card' . $i + 1;
            $this->$cardName = Card::getCard($cardData[$i]);
        }
    }
}