#----------------------
# initialize session
source("src/load_paths.R")
source("src/load_packages.R")


# -------------------------------
# Wilcoxon signed rank test

mu01 <- 0.80
var01 <- rnorm(20, mu01, 0.01)
plot(var01)
abline(h=0.56, col = "green")

wilcox.test(var01, mu=0.56, correct = TRUE,  exact = FALSE)
wilcox.test(var01, mu=0.56, correct = FALSE, exact = FALSE)

t.test(var01, mu=0.56, alternative = "two.sided")



# -------------------------------
# Wilcoxon rank sum test

var01 <- rnorm(20, 0.7, 0.2)
var02 <- rnorm(30, 0.5, 0.1)

# Scatter plot
ggplot() +
  geom_point(aes(x = seq_along(var01), y = var01), color = "red") +
  geom_point(aes(x = seq_along(var02), y = var02), color = "green")

wilcox.test(var01, var02, correct = TRUE, exact = FALSE)

t.test(var01, var02, alternative = "two.sided", paired = FALSE, var.equal = FALSE)




# -------------------------------
# Wilcoxon paired data (Wilcoxon signed rank test)

# create correlated data 
var01 <- rnorm(50, 1.6, 0.5)
var02 <- var01 + rnorm(50, 0.1, 0.2)
var_diff <- var01 - var02

# Scatter plot
ggplot() +
  geom_point(aes(x = seq_along(var01), y = var01), color = "red") +
  geom_point(aes(x = seq_along(var02), y = var02), color = "green") +
  geom_point(aes(x = seq_along(var_diff), y = var_diff), color = "violet")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "violet")  

wilcox.test(var_diff, correct = TRUE, exact = FALSE)
wilcox.test(var01, var02, paired = TRUE, correct = TRUE, exact = FALSE)

t.test(var_diff, paired = FALSE, var.equal = FALSE)
t.test(var01, var02, paired = TRUE, var.equal = FALSE)
     
# failing to specify paired
wilcox.test(var01, var02, paired = FALSE, correct = TRUE, exact = FALSE)
















