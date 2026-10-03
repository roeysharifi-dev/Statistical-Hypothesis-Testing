investments_data <- data.frame(
  Amount = c(12000, 15000, 18000, 20000, 22000, 25000, 
             45000, 55000, 60000, 65000, 70000, 80000, 85000, 95000, 110000),
  
  Version = factor(c(rep("A", 6), rep("B", 9))),
  
  Risk_Level = factor(c(rep("Conservative", 3), 
                        rep("Balanced", 7), 
                        rep("Aggressive", 5)),
                      levels = c("Conservative", "Balanced", "Aggressive"))
)
print("==== Data Overview ====")
print(investments_data)
cat("\n\n")

cat("==== 1. Mann-Whitney U Test (A/B Testing) ====\n")
mann_whitney_result <- wilcox.test(Amount ~ Version, 
                                   data = investments_data, 
                                   alternative = "less", 
                                   exact = FALSE)
print(mann_whitney_result)
cat("\n\n")

week1 <- c(1, 0, 2, 1, 1, 0, 2)
week2 <- c(3, 2, 4, 3, 2, 1, 5)
# חישוב ההפרשים וכמות ההפרשים החיוביים
differences <- week2 - week1
positive_diffs <- sum(differences > 0)
total_valid_pairs <- length(differences[differences != 0]) # מתעלמים מתיקו אם יש

cat("==== 2. Sign Test (User Activity Growth) ====\n")
sign_test_result <- binom.test(x = positive_diffs, 
                               n = total_valid_pairs, 
                               p = 0.5, 
                               alternative = "greater")
print(sign_test_result)
cat("\n\n")

cat("==== 3. Kruskal-Wallis Test (Risk Profile Impact) ====\n")
kruskal_result <- kruskal.test(Amount ~ Risk_Level, data = investments_data)
print(kruskal_result)
cat("\n\n")

