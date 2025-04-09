data(iris)

iris_bin <- subset(iris, Species %in% c("setosa", "versicolor"))
iris_bin$Species <- factor(iris_bin$Species)

modelo_log <- glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                  data = iris_bin, family = binomial)

summary(modelo_log)

probabilidades <- predict(modelo_log, type = "response")
predicoes <- ifelse(probabilidades > 0.5, "versicolor", "setosa")
predicoes <- factor(predicoes, levels = levels(iris_bin$Species))

tabela_confusao <- table(Predito = predicoes, Real = iris_bin$Species)
acuracia <- mean(predicoes == iris_bin$Species)

saveRDS(modelo_log, "modelo_logistico_iris.rds")

print(tabela_confusao)
print(paste("Acurácia do modelo:", round(acuracia * 100, 2), "%"))