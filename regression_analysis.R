# --- Regression Analysis Toolkit: mtcars & iris ---

# --- Regression with mtcars ---

# Load and explore the mtcars dataset
data(mtcars)
cat("Summary of mtcars:\n")
print(summary(mtcars))

# Visualize relationships
pairs(mtcars[, c("mpg", "wt", "hp")], main = "mtcars: mpg, wt, hp")

# Multiple linear regression: mpg ~ wt + hp
mtcars_model <- lm(mpg ~ wt + hp, data = mtcars)
cat("\nRegression summary for mtcars (mpg ~ wt + hp):\n")
print(summary(mtcars_model))

# Plot actual vs predicted
mtcars$predicted_mpg <- predict(mtcars_model)
plot(mtcars$mpg, mtcars$predicted_mpg,
     xlab = "Actual MPG", ylab = "Predicted MPG",
     main = "mtcars: Actual vs Predicted MPG")
abline(0, 1, col = "red")

# Diagnostic plots
par(mfrow = c(2, 2))
plot(mtcars_model)
par(mfrow = c(1, 1)) # Reset plot layout

# --- Polynomial Regression with mtcars ---

# Add a quadratic term for weight
mtcars$wt2 <- mtcars$wt^2

# Fit the polynomial regression model
mtcars_poly_model <- lm(mpg ~ wt + wt2 + hp, data = mtcars)
cat("\nPolynomial Regression summary for mtcars (mpg ~ wt + wt^2 + hp):\n")
print(summary(mtcars_poly_model))

# Compare actual vs predicted
mtcars$predicted_mpg_poly <- predict(mtcars_poly_model)
plot(mtcars$mpg, mtcars$predicted_mpg_poly,
     xlab = "Actual MPG", ylab = "Predicted MPG (Poly)",
     main = "mtcars: Actual vs Predicted MPG (Polynomial)")
abline(0, 1, col = "darkgreen")

# Diagnostic plots
par(mfrow = c(2, 2))
plot(mtcars_poly_model)
par(mfrow = c(1, 1))

# --- Regression with iris ---

# Load and explore the iris dataset
data(iris)
cat("\nSummary of iris:\n")
print(summary(iris))

# Visualize relationships
pairs(iris[, c("Sepal.Length", "Petal.Length", "Petal.Width")], main = "iris: Sepal.Length, Petal.Length, Petal.Width")

# Multiple linear regression: Sepal.Length ~ Petal.Length + Petal.Width
iris_model <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
cat("\nRegression summary for iris (Sepal.Length ~ Petal.Length + Petal.Width):\n")
print(summary(iris_model))

# Plot actual vs predicted
iris$predicted_sepal_length <- predict(iris_model)
plot(iris$Sepal.Length, iris$predicted_sepal_length,
     xlab = "Actual Sepal Length", ylab = "Predicted Sepal Length",
     main = "iris: Actual vs Predicted Sepal Length")
abline(0, 1, col = "blue")

# Diagnostic plots
par(mfrow = c(2, 2))
plot(iris_model)
par(mfrow = c(1, 1)) # Reset plot layout

# --- Regression with Interaction in iris ---

# Fit a model with interaction
iris_interaction_model <- lm(Sepal.Length ~ Petal.Length * Petal.Width, data = iris)
cat("\nRegression summary for iris (Sepal.Length ~ Petal.Length * Petal.Width):\n")
print(summary(iris_interaction_model))

# Compare actual vs predicted
iris$predicted_sepal_length_inter <- predict(iris_interaction_model)
plot(iris$Sepal.Length, iris$predicted_sepal_length_inter,
     xlab = "Actual Sepal Length", ylab = "Predicted Sepal Length (Interaction)",
     main = "iris: Actual vs Predicted Sepal Length (Interaction)")
abline(0, 1, col = "purple")

# Diagnostic plots
par(mfrow = c(2, 2))
plot(iris_interaction_model)
par(mfrow = c(1, 1))

# --- Interpretation Tips ---
cat("\n--- Interpretation Tips ---\n")
cat("Coefficients: Significant p-values (< 0.05) indicate important predictors.\n")
cat("R-squared: Closer to 1 means better model fit.\n")
cat("Residual plots: Should look random; patterns may indicate model issues.\n")
cat("Interaction term: If significant, the effect of one variable depends on the other.\n") 