library(ggplot2)
data(iris)
iris$Species <- factor(ifelse(iris$Species == "setosa", "Setosa", "Non-Setosa"), levels = c("Non-Setosa", "Setosa"))
log_model <- glm(Species ~ Petal.Length + Petal.Width, data = iris, family = binomial)

iris$Predicted <- factor(ifelse(predict(log_model, type = "response") > 0.5, "Setosa", "Non-Setosa"), levels = c("Non-Setosa", "Setosa"))

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Predicted, shape = Species)) +
  geom_point(size = 3) +
  labs(title = "Logistic Regression Predictions") +
  theme_minimal()

################################################################################
#using test and control
# 1. Preparar los datos base
data(iris)
iris$Species <- factor(ifelse(iris$Species == "setosa", "Setosa", "Non-Setosa"), levels = c("Non-Setosa", "Setosa"))

# 2. Dividir en entrenamiento (70%) y prueba (30%)
set.seed(42) # Para asegurar la reproducibilidad
train_index <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_index, ]
test_data  <- iris[-train_index, ]

# 3. Entrenar el modelo SOLO con los datos de entrenamiento
log_model <- glm(Species ~ Petal.Length + Petal.Width, data = train_data, family = binomial)

# 4. Hacer predicciones y evaluar en el conjunto de prueba (Test)
test_probs <- predict(log_model, newdata = test_data, type = "response")
test_data$Predicted <- factor(ifelse(test_probs > 0.5, "Setosa", "Non-Setosa"), levels = c("Non-Setosa", "Setosa"))

# Calcular la precisión (Accuracy)
accuracy <- mean(test_data$Predicted == test_data$Species)
cat("Precisión en el conjunto de Test:", round(accuracy * 100, 2), "%\n")

# Ver la matriz de confusión
print(table(Real = test_data$Species, Predicho = test_data$Predicted))

# 5. Visualizar las predicciones en el conjunto de prueba
ggplot(test_data, aes(x = Petal.Length, y = Petal.Width, color = Predicted, shape = Species)) +
  geom_point(size = 3) +
  labs(
    title = "Logistic Regression: Predicciones en Test",
    subtitle = paste("Precisión del modelo:", round(accuracy * 100, 1), "%")
  ) +
  theme_minimal()
