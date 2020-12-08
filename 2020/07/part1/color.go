package main

type Color struct {
	Name             string
	Contains         []*Color
	CanBeContainedIn []*Color
}

func (c *Color) AddColorToContains(newColor *Color) {
	c.Contains = c.addColorUnique(c.Contains, newColor)
}

func (c *Color) AddColorToCanBeContainedIn(newColor *Color) {
	c.CanBeContainedIn = c.addColorUnique(c.CanBeContainedIn, newColor)
}

func (c *Color) addColorUnique(colorSlice []*Color, colorToAdd *Color) []*Color {
	if !c.contains(colorSlice, colorToAdd) {
		colorSlice = append(colorSlice, colorToAdd)
	}
	return colorSlice
}

func (c *Color) contains(colorSlice []*Color, color *Color) bool {
	for _, element := range colorSlice {
		if element.Name == color.Name {
			return true
		}
	}

	return false
}

func (c *Color) GetNumberOfPossibleContainers() int {
	possibleContainers := c.getAllPossibleContainers()
	return len(possibleContainers)
}

func (c *Color) getAllPossibleContainers() (possibleContainers []*Color) {
	if len(c.CanBeContainedIn) != 0 {
		possibleContainers = c.CanBeContainedIn

		for _, directContainer := range c.CanBeContainedIn {
			indirectContainers := directContainer.getAllPossibleContainers()

			for _, element := range indirectContainers {
				possibleContainers = c.addColorUnique(possibleContainers, element)
			}
		}
	}

	return
}
