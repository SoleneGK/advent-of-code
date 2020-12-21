package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"sort"
	"strings"
)

func main() {
	file, err := os.Open("data")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	answerPart1, answerPart2 := getAnswers(file)
	fmt.Printf("The answer for part 1 is %d\n", answerPart1)
	fmt.Printf("The answer for part 2 is %s\n", answerPart2)
}

var (
	ingredientList []Ingredient
	allergenList   []Allergen
)

func getAnswers(file io.Reader) (answerPart1 int, answerPart2 string) {
	foodList := getFoodList(file)
	ingredientList = getIngredientList(foodList)
	allergenList = getAllergenList(foodList)

	possibleAllergenIngredientCombinations := getPossibleAllergenIngredientCombination(foodList)
	ingredentsWithoutAllergen := getIngredientsWithoutAllergen(possibleAllergenIngredientCombinations)

	answerPart1 = getNumberOfOccurences(ingredentsWithoutAllergen, foodList)
	answerPart2 = getAnswerPart2(possibleAllergenIngredientCombinations)

	return
}

func getFoodList(file io.Reader) (foodList []Food) {
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		foodList = append(foodList, getNewFood(scanner.Text()))
	}

	return
}

func getNewFood(description string) Food {
	parts := strings.Split(description, " (contains ")

	return Food{
		Ingredients: getIngredients(parts[0]),
		Allergens:   getAllergens(parts[1]),
	}
}

func getIngredients(toProcessList string) (ingredientList []Ingredient) {
	rawList := strings.Split(toProcessList, " ")

	for _, element := range rawList {
		ingredientList = append(ingredientList, Ingredient(element))
	}

	return
}

func getAllergens(toProcessList string) (allergenList []Allergen) {
	toProcessList = strings.TrimSuffix(toProcessList, ")")
	rawList := strings.Split(toProcessList, ", ")

	for _, element := range rawList {
		allergenList = append(allergenList, Allergen(element))
	}

	return
}

func getIngredientList(foodList []Food) (ingredientList []Ingredient) {
	for _, food := range foodList {
		for _, ingredient := range food.Ingredients {
			ingredientList = addUniqueIngredient(ingredientList, ingredient)
		}
	}
	return
}

func getAllergenList(foodList []Food) (allergenList []Allergen) {
	for _, food := range foodList {
		for _, allergen := range food.Allergens {
			allergenList = addUniqueAllergen(allergenList, allergen)
		}
	}
	return
}

func getPossibleAllergenIngredientCombination(foodList []Food) map[Allergen][]Ingredient {
	possibleCombinations := map[Allergen][]Ingredient{}

	for _, allergen := range allergenList {
		foodsWithAllergen := getFoodsThatContainsAllergen(foodList, allergen)
		possibleCombinations[allergen] = getCommonIngredients(foodsWithAllergen)
	}

	return possibleCombinations
}

func getFoodsThatContainsAllergen(foodList []Food, allergen Allergen) (foodsWithAllergen []Food) {
	for _, food := range foodList {
		if containsAllergen(food.Allergens, allergen) {
			foodsWithAllergen = append(foodsWithAllergen, food)
		}
	}
	return
}

func getCommonIngredients(foodList []Food) (commonIngredients []Ingredient) {
	commonIngredients = foodList[0].Ingredients

	for _, food := range foodList[1:] {
		commonIngredients = getIngredientIntersection(commonIngredients, food.Ingredients)
	}

	return commonIngredients
}

func getIngredientIntersection(list1, list2 []Ingredient) (commonIngredients []Ingredient) {
	for _, ingredient := range list1 {
		if containsIngredient(list2, ingredient) {
			commonIngredients = append(commonIngredients, ingredient)
		}
	}
	return
}

func getIngredientsWithoutAllergen(possibleAllergenIngredientCombinations map[Allergen][]Ingredient) []Ingredient {
	ingredientsWithoutAllergen := make([]Ingredient, len(ingredientList))
	copy(ingredientsWithoutAllergen, ingredientList)

	for _, allergen := range allergenList {
		for _, ingredient := range possibleAllergenIngredientCombinations[allergen] {
			ingredientsWithoutAllergen = removeIngredient(ingredientsWithoutAllergen, ingredient)
		}
	}

	return ingredientsWithoutAllergen
}

func getNumberOfOccurences(ingredientList []Ingredient, foodList []Food) (number int) {
	for _, food := range foodList {
		number += len(getIngredientIntersection(ingredientList, food.Ingredients))
	}
	return
}

func getAnswerPart2(possibleAllergenIngredientCombinations map[Allergen][]Ingredient) string {
	combinations := findCombinations(possibleAllergenIngredientCombinations)
	return formatAnswer(combinations)
}

func findCombinations(possibleAllergenIngredientCombinations map[Allergen][]Ingredient) map[Allergen]Ingredient {
	combination := map[Allergen]Ingredient{}

	for len(possibleAllergenIngredientCombinations) > 0 {
		for allergen, ingredientList := range possibleAllergenIngredientCombinations {
			if len(ingredientList) == 1 {
				combination[allergen] = ingredientList[0]
				delete(possibleAllergenIngredientCombinations, allergen)
				removeAllIngredientOccurences(possibleAllergenIngredientCombinations, ingredientList[0])
			}
		}
	}

	return combination
}

func removeAllIngredientOccurences(table map[Allergen][]Ingredient, ingredient Ingredient) {
	for allergen, ingredientList := range table {
		table[allergen] = removeIngredient(ingredientList, ingredient)
	}
}

func formatAnswer(combinations map[Allergen]Ingredient) (answer string) {
	sortedAllergens := sortAllergens(allergenList)

	answer += string(combinations[sortedAllergens[0]])

	for i := 1; i < len(sortedAllergens); i++ {
		answer += "," + string(combinations[sortedAllergens[i]])
	}

	return
}

func sortAllergens(unsortedList []Allergen) (sortedList []Allergen) {
	stringList := []string{}

	for _, allergen := range unsortedList {
		stringList = append(stringList, string(allergen))
	}

	sort.Strings(stringList)

	for _, allergen := range stringList {
		sortedList = append(sortedList, Allergen(allergen))
	}

	return
}

type Food struct {
	Ingredients []Ingredient
	Allergens   []Allergen
}

type Allergen string

type Ingredient string

func addUniqueIngredient(ingredientList []Ingredient, ingredient Ingredient) []Ingredient {
	if !containsIngredient(ingredientList, ingredient) {
		ingredientList = append(ingredientList, ingredient)
	}
	return ingredientList
}

func containsIngredient(haystack []Ingredient, needle Ingredient) bool {
	for _, element := range haystack {
		if element == needle {
			return true
		}
	}
	return false
}

func addUniqueAllergen(allergenList []Allergen, allergen Allergen) []Allergen {
	if !containsAllergen(allergenList, allergen) {
		allergenList = append(allergenList, allergen)
	}
	return allergenList
}

func removeIngredient(ingredientList []Ingredient, ingredientToRemove Ingredient) (newList []Ingredient) {
	for _, ingredient := range ingredientList {
		if ingredient != ingredientToRemove {
			newList = append(newList, ingredient)
		}
	}
	return
}

func containsAllergen(haystack []Allergen, needle Allergen) bool {
	for _, element := range haystack {
		if element == needle {
			return true
		}
	}
	return false
}
