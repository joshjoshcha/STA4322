group_A <- c(14, 18, 11, 13, 18, 17, 21, 9, 16, 17, 14, 15)
group_B <- c(12, 12, 14, 13, 6, 18, 14, 16, 10, 7, 15, 10)
data <- c(group_A, group_B)
n_A <- length(group_A)
n_B <- length(group_B)

obs_diff <- mean(group_A) - mean(group_B)

n_perm <- 3
perm_diffs <- numeric(n_perm)

set.seed(444444)

for (i in 1:n_perm){
  shuffled <- sample(data) # randomly reorders all the values
  perm_group_A <- shuffled[1:n_A] # take the first 12 values from shuffled
  perm_group_B <- shuffled[(n_A+1):(n_A+n_B)] # take the next 12 values from shuffled
  perm_diffs[i] <- mean(perm_group_A) - mean(perm_group_B)
  }

# You’ll have n_perm simulated differences in means stored in perm_diffs.
# These make up the null distribution
# Then you compare your observed difference to this distribution to see how extreme it is.

p_value <- mean(abs(perm_diffs) >= abs(obs_diff))

perm_diffs
obs_diff
p_value

# --- Large run (500,000 permutations) ---
n_perm <- 5000
perm_diffs <- numeric(n_perm)

set.seed(444444)  # reproducibility for this run too
for (i in 1:n_perm){
  shuffled <- sample(data)
  perm_group_A <- shuffled[1:n_A]
  perm_group_B <- shuffled[(n_A+1):(n_A+n_B)]
  perm_diffs[i] <- mean(perm_group_A) - mean(perm_group_B)
}

p_value <- mean(abs(perm_diffs) >= abs(obs_diff))
p_value

hist(perm_diffs, breaks = 50, col = "lightblue",
     main = "Permutation Test: Difference in Means",
     xlab = "Difference in Means")
abline(v = obs_diff, col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("Observed Diff =", round(obs_diff, 3)),
       col = "red", lty = 2, lwd = 2)



