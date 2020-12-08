package main

import (
	"testing"
)

func TestAnalyzeRule(t *testing.T) {
	t.Run("Rule without content", func(t *testing.T) {
		rawRule := "light red bags contain no other bags."
		rule := NewRule(rawRule)

		want := Rule{"light red", map[string]int{}}

		assertRule(t, rule, want)
	})

	t.Run("Rule with one color in content", func(t *testing.T) {
		rawRule := "dotted teal bags contain 4 pale indigo bags."
		rule := NewRule(rawRule)

		want := Rule{
			"dotted teal",
			map[string]int{
				"pale indigo": 4,
			},
		}

		assertRule(t, rule, want)
	})

	t.Run("Rule with several colors in content", func(t *testing.T) {
		rawRule := "pale olive bags contain 3 striped blue bags, 5 faded magenta bags, 3 light white bags."
		rule := NewRule(rawRule)

		want := Rule{
			"pale olive",
			map[string]int{
				"striped blue":  3,
				"faded magenta": 5,
				"light white":   3,
			},
		}

		assertRule(t, rule, want)
	})
}
