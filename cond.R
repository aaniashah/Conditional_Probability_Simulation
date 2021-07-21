#i)
set.seed(1234)
tier_A <- c("Rogue","Druid", "Shaman")
tier_B <- c("Warrior", "Warlock", "Hunter")
tier_C <- c("Mage", "Priest", "Paladin", "Demon Hunter")
arena <- c(tier_A, tier_B, tier_C)
N <- 10000
all_samples <- t(replicate(N, sample(arena, 3)))
head(all_samples)

#ii)
for (row in 1:6) {
  cat("Warlock" %in% all_samples[row,])
  print(all_samples[row,])
}

#iii)
in_tier_A <- function(game) {
  return (("Shaman" %in% game) | ("Rogue" %in% game) | ("Druid" %in% game))
}

in_tier_B <- function(game) {
  return (("Hunter" %in% game) | ("Warrior" %in% game) | ("Warlock" %in% game))
}

in_tier_C <- function(game) {
  return (("Mage" %in% game) | ("Priest" %in% game) | ("Paladin" %in% game) |
            ("Demon Hunter" %in% game))
}

all_tiers <- function(game) {
  return ((in_tier_A(game)) & (in_tier_B(game)) & (in_tier_C(game)))
}

#iv)
N <- 10000
count <- 0
for (row in 1:N) {
  if (all_tiers(all_samples[row,])) {
    count <- count + 1
  }
}
proportion <- count / N
proportion

# Q2di
N <- 10000
all_samples <- t(replicate(N, sample(arena, 3)))
total_number_in_A <- 0
count_rogue <- 0
for (row in 1:N) {
  if ((all_samples[row,1] %in% tier_A) || (all_samples[row,2] %in% tier_A) ||
      (all_samples[row,3] %in% tier_A)) {
    if ("Rogue" %in% all_samples[row,]) {
      count_rogue <- count_rogue + 1
    }
    total_number_in_A <- total_number_in_A + 1
  }
}
probability_rogue_given_A <- count_rogue / total_number_in_A
probability_rogue_given_A

# Q2dii
total_number_in_B <- 0
count_rogue <- 0
for (row in 1:N) {
  if (all_samples[row,1] %in% tier_B || all_samples[row,2] %in% tier_B ||
      all_samples[row,3] %in% tier_B) {
    if ("Rogue" %in% all_samples[row,]) {
      count_rogue <- count_rogue + 1
    }
    total_number_in_B <- total_number_in_B + 1
  }
}
probability_rogue_given_B <- count_rogue / total_number_in_B
probability_rogue_given_B

# Q2eii
set.seed(1234)
N <- 10000
samples_replace <- t(replicate(N, sample(arena, 3, replace = TRUE)))
head(samples_replace)

count_replace <- 0
for (row in 1:N) {
  if (all_tiers(samples_replace[row,])) {
    count_replace <- count_replace + 1
  }
}
proportion_with_replace <- count_replace / N
proportion_with_replace

# Q3b
set.seed(1234567)
N <- 10000
dice_results <- 1:6
die_roll <- t(replicate(N, sample(dice_results, 4, replace = TRUE)))
wins <- 0
for (row in 1:N) {
  if (1 %in% die_roll[row,]) {
    wins <- wins + 1
  }
}
proportion_of_wins = wins / N
proportion_of_wins

# Q3cii
double_six = function(n) {
  prob_win <- 1 - (35/36)^n
  prob_win
}

double_six(10)
double_six(20)
double_six(30)

# Q3ciii
x <- 1:50
y <- double_six(x)
plot(x, y, type='l', main="double_six(n)", col=2, lwd=3, xlab="n", 
     ylab = "double_six(n)")

# Q3civ
largest_int = function() {
  n <- 1
  while (double_six(n) < 0.5) {
    n <- n + 1
  }
  n <- n - 1
  n
}

largest_int()
double_six(24)
double_six(25)


  