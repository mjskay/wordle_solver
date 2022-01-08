Solving Wordles
================
Matthew Kay

## Introduction

In this document I play around with solving Wordles automatically.
Probably the strategy I came up with is silly, but it seems to do an
okay job, so I figured I’d write this up!

I rely heavily on [coolbutuseless](https://twitter.com/coolbutuseless)’s
[wordle](https://github.com/coolbutuseless/wordle) package.

Packages needed:

``` r
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# install via devtools::install_github("coolbutuseless/wordle")
library(wordle)

theme_set(theme_light())
```

## General strategy

I’m going to take a pretty simple approach:

-   We’ll only look one word ahead at a time.

-   We’ll score words based on two things (essentially based on the
    green and yellow squares Wordle gives as feedback):

    1.  The expected number of letters in the word in the correct
        position, given the remaining possible words.

    2.  The expected number of unique letters anywhere in the word,
        given the remaining possible words.

-   We’ll calculate these expectations assuming the words chosen by
    Wordle follow some distribution based on word frequencies in the
    English language (though actually we’ll check this too!).

## Getting a word frequency distribution

Given all that, our first step will be to get a frequency distribution
of words to weight our expectations by.

Helpfully, the `{wordle}` package already includes the full dictionary
of 5-letter strings that Wordle considers to be “words”:

``` r
helper = WordleHelper$new(nchar = 5)
str(helper$words)
```

    ##  chr [1:12972] "aahed" "aalii" "aargh" "aarti" "abaca" "abaci" "aback" ...

Using two different data sources, let’s hack together a rough table of
English word frequencies. Here I am using a [corpus of commonly-used
words from the Google Web Trillion Word Corpus]() and the [BNC word
frequency list](https://martinweisser.org/corpora_site/word_lists.html),
and just naively stuffing them together with equal weights:

``` r
freq = tibble(word = helper$words) %>%
  left_join(read.csv("unigram_freq.csv"), by = "word") %>%
  left_join(
    read.csv("bnc_freq.csv") %>%
      group_by(word) %>%
      summarise(count = sum(count)),
    by = "word"
  ) %>%
  mutate(
    count.x = ifelse(is.na(count.x), 0, count.x),
    count.y = ifelse(is.na(count.y), 0, count.y),
    count = count.x/sum(count.x) + count.y/sum(count.y),
    # need a nonzero count for all words, so just assume words that don't
    # appear at all are half as frequent as the least frequent appearing word
    count = ifelse(count == 0, min(count[count != 0])/2, count),
    # rough log of the count shifted above 0
    # (we'll want this later)
    log_count = log(count) - log(min(count)/2)
  )

freq %>%
  arrange(-count)
```

    ## # A tibble: 12,972 x 5
    ##    word     count.x count.y  count log_count
    ##    <chr>      <dbl>   <dbl>  <dbl>     <dbl>
    ##  1 which  810514085    3719 0.0554      13.9
    ##  2 there  701170205    3278 0.0486      13.8
    ##  3 their  782849411    2608 0.0423      13.6
    ##  4 about 1226734006    1971 0.0420      13.6
    ##  5 would  572644147    2551 0.0383      13.5
    ##  6 other  978481319    1421 0.0317      13.4
    ##  7 could  302311431    1683 0.0241      13.1
    ##  8 these  541003982    1254 0.0229      13.0
    ##  9 first  578161543    1193 0.0228      13.0
    ## 10 after  372948094    1160 0.0192      12.9
    ## # ... with 12,962 more rows

## How are Wordle words picked?

I don’t know how Wordle words are picked, but a reasonable
simplification might be to assume they follow some distribution that has
some relationship to their frequency in English. So let’s compare the
frequency of the past answers to their frequencies in English.

### Past answers (SPOILERS!)

As it happens, you can grab past answers from the Wordle source code
(and future answers! though we won’t do that). Answers so far are:

``` r
past_answers = c("cigar","rebut","sissy","humph","awake","blush","focal","evade","naval","serve","heath","dwarf","model","karma","stink","grade","quiet","bench","abate","feign","major","death","fresh","crust","stool","colon","abase","marry","react","batty","pride","floss","helix","croak","staff","paper","unfed","whelp","trawl","outdo","adobe","crazy","sower","repay","digit","crate","cluck","spike","mimic","pound","maxim","linen","unmet","flesh","booby","forth","first","stand","belly","ivory","seedy","print","yearn","drain","bribe","stout","panel","crass","flume","offal","agree","error","swirl","argue","bleed","delta","flick","totem","wooer","front","shrub","parry","biome","lapel","start","greet","goner","golem","lusty","loopy","round","audit","lying","gamma","labor","islet","civic","forge","corny","moult","basic","salad","agate","spicy","spray","essay","fjord","spend","kebab","guild","aback","motor","alone","hatch","hyper","thumb","dowry","ought","belch","dutch","pilot","tweed","comet","jaunt","enema","steed","abyss","growl","fling","dozen","boozy","erode","world","gouge","click","briar","great","altar","pulpy","blurt","coast","duchy","groin","fixer","group","rogue","badly","smart","pithy","gaudy","chill","heron","vodka","finer","surer","radio","rouge","perch","retch","wrote","clock","tilde","store","prove","bring","solve","cheat","grime","exult","usher","epoch","triad","break","rhino","viral","conic","masse","sonic","vital","trace","using","peach","champ","baton","brake","pluck","craze","gripe","weary","picky","acute","ferry","aside","tapir","troll","unify","rebus","boost","truss","siege","tiger","banal")
```

Let’s see if we can figure out how they relate to the frequency
distribution of words in English.

Here are the frequencies of the selected words compared to their
frequencies in English and the scaled log of their frequencies:

``` r
freq %>%
  mutate(count_rank = rank(-count)) %>%
  filter(word %in% past_answers) %>%
  ggplot(aes(x = count_rank)) +
  geom_histogram(aes(y = stat(ncount)), bins = 100) +
  geom_line(aes(y = count, color = method), data = . %>% 
    pivot_longer(c(count, log_count), names_to = "method", values_to = "count") %>%
    group_by(method) %>%
    mutate(count = count/max(count))
  ) +
  labs(
    x = "Past Wordle answers, ranked by English-language frequency",
    y = "Frequency (scaled)",
    color = "English-language\ncomparison"
  )
```

<img src="README_files/figure-gfm/word_freq-1.png" width="672" />

It would appear that the probability of a word being chosen is closer to
the log of its frequency than its actual frequency. This is probably a
sensible word selection strategy for making a good game, since it makes
the puzzle not just a bunch of very common words (but also not just a
bunch of rare words). So we may want to weight expectations by log
frequency, not frequency (we’ll actually try both and see).

In the meantime, we’ll keep both the frequency and log frequency as
lookup tables for later…

``` r
english_freq_table = freq$count
names(english_freq_table) = freq$word

english_log_freq_table = freq$log_count
names(english_log_freq_table) = freq$word
```

## Scoring guesses

Here’s a relatively naive scoring function for possible guesses. It
looks at every possible word and scores it according to a weighted sum
of two expectations: the expected number of letters in the correct
position and the expected number of unique letters in the word at all.
It calculates expectations over a provided frequency distribution of
possible words.

``` r
score_words = function(helper, weight = 0.5, freq_table = english_log_freq_table) {
  words = helper$words
  
  # get word frequencies
  word_freq = if (is.null(freq_table)) {
    rep(1, length(words))
  } else{
    freq_table[words]
  }
  word_freq = word_freq / sum(word_freq)

  # first part of score: expected number of letters in the correct position
  word_letters = strsplit(words, "")
  word_letters_matrix = simplify2array(word_letters)
  equal_score = vapply(word_letters, \(w) sum(colSums(w == word_letters_matrix) * word_freq), numeric(1))
  
  # second part of score: expected number of unique letters in the word at all
  word_letters_mask = lapply(word_letters, \(w) letters %in% w)
  word_letters_mask_matrix = simplify2array(word_letters_mask)
  in_score = colSums((word_freq * t(word_letters_mask_matrix)) %*% word_letters_mask_matrix)
  
  tibble(words, equal_score, in_score, score = weight*equal_score + (1 - weight)*in_score) %>%
    arrange(-score)
}
```

For example, we can ask it for an initial guess by scoring all possible
words given that we have made no guesses so far:

``` r
score_words(helper)
```

    ## # A tibble: 12,972 x 4
    ##    words equal_score in_score score
    ##    <chr>       <dbl>    <dbl> <dbl>
    ##  1 tares       0.791     1.89  1.34
    ##  2 lares       0.772     1.88  1.33
    ##  3 rales       0.748     1.88  1.31
    ##  4 rates       0.733     1.89  1.31
    ##  5 nares       0.747     1.86  1.30
    ##  6 cares       0.806     1.80  1.30
    ##  7 tales       0.767     1.82  1.29
    ##  8 dares       0.776     1.81  1.29
    ##  9 pares       0.792     1.77  1.28
    ## 10 tores       0.780     1.78  1.28
    ## # ... with 12,962 more rows

So it suggests “tares” as a first word to try. Let’s play against a
particular word and see how many steps it takes to get it:

``` r
game_try = function(game, guess, silent = FALSE) {
  # version of WordleGame$try() that can optionally run without output
  if (silent) {
    void = NULL
    void_con = sink(textConnection("void", "w", local = TRUE))
    on.exit(close(void_con))
    on.exit(sink())
  }
  game$try(guess)
}

play_against = function(word, first_guess = NULL, silent = FALSE, ...) {
  helper = WordleHelper$new(nchar = nchar(word))
  game = WordleGame$new(helper$words, target_word = word)
  
  if (!is.null(first_guess)) {
    helper$update(first_guess, game_try(game, first_guess, silent))
  }
  
  while (!game$is_solved()) {
    guess = score_words(helper, ...)$words[[1]]
    helper$update(guess, game_try(game, guess, silent))
  }
  
  game$attempts
}
```

Let’s try it out:

``` r
play_against("slump")
```

    ## [38;5;232m[48;5;249m t [48;5;249m a [48;5;249m r [48;5;249m e [48;5;226m s [39m[49m 
    ## [38;5;232m[48;5;46m s [48;5;249m o [48;5;249m i [48;5;226m l [48;5;249m y [39m[49m 
    ## [38;5;232m[48;5;46m s [48;5;46m l [48;5;46m u [48;5;46m m [48;5;46m p [39m[49m

    ## [1] "tares" "soily" "slump"

(This doesn’t seem to render properly in Github markdown, so here’s a
screenshot:)

![](solve_slump.png)

Not bad!

## Testing different strategies

There are two obvious parameters here: the `weight` between the two
expectations and the word frequency table used. Let’s try both in
various combinations:

``` r
strategy_steps = 
  map_dfr(list(freq = english_freq_table, log_freq = english_log_freq_table), .id = "table", \(freq_table) {
    map_dfr(seq(0, 1, length.out = 21), \(weight) {
      cat("Scoring weight = ", weight, "\n")
      # get the first guess here since it takes the longest
      first_guess = score_words(WordleHelper$new(nchar = nchar(past_answers[[1]])), weight = weight)$words[[1]]
      
      map_dfr(past_answers, \(word) data.frame(
        word, 
        weight,
        steps = tryCatch(
          length(play_against(
            word, first_guess = first_guess, 
            weight = weight, freq_table = freq_table,
            silent = TRUE
          )),
          error = function(e) NA
        )
      ))
    })
  })
```

Comparing the the various strategies…

``` r
strategy_steps %>%
  mutate(table = forcats::fct_recode(table,
    "English frequency" = "freq",
    "log(English frequency)" = "log_freq"
  )) %>%
  group_by(weight, table) %>%
  summarise(steps = mean(steps, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(weight, steps, color = table)) +
  geom_line() +
  labs(
    title = "Comparison of different strategies for picking the next word",
    x = "Weight between E(# letters in correct position) and\nE(# unique letters anywhere in word)",
    color = "Word weighting",
    y = "Average number of steps until correct answer"
  )
```

<img src="README_files/figure-gfm/strategy_steps-1.png" width="672" />

It seems like an equal weight between both expectations (using the log
frequency table) is not too shabby.

We could use `optim()` to find a more accurate value for `weight` but
honestly this seems fine. We could also try more complex strategies
(e.g. looking more than one choice ahead)—but hey, this simple approach
gives an expected number of steps of less than 4, which seems pretty
good to me!
