package main

func NewColor(name string) *Color {
	return &Color{
		Name:     name,
		Contains: map[*Color]int{},
	}
}

type Color struct {
	Name     string
	Contains map[*Color]int
}

func (c *Color) AddColorToContains(color *Color, quantity int) {
	c.Contains[color] = quantity
}

func (c *Color) GetTotalNumberOfBagsContained() (total int) {
	for color := range c.Contains {
		total += c.Contains[color] * (1 + color.GetTotalNumberOfBagsContained())
	}

	return
}
