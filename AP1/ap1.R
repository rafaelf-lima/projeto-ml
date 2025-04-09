# Dataset: Red Wine Quality - https://www.kaggle.com/datasets/uciml/red-wine-quality-cortez-et-al-2009
# Registros: 1599
# Variáveis: Doze variáveis + uma variável binária criada para permitir a regressão logistíca (quality_bin)  
# Justificativa: Permite aplicar regressão linear (quality contínua) e regressão logística (qualidade binária).

library(tidyverse)


# 1. Carregamento e criação da variável binária -----------------------

df <- read.csv("winequality-red.csv", sep = ",")
df$quality_bin <- ifelse(df$quality >= 6, "Qualidade superior", "Qualidade inferior")
View(df)

# 2. Pré-processamento e análise exploratória -------------------------

str(df)
sum(duplicated(df))
df[duplicated(df), ]

print(colSums(is.na(df))) # Não há valores nulos
sum(is.na(df))

dim(df) # L: 1599, C: 13

summary(df)

maximo_quality <- max(df$quality, na.rm = TRUE)
minimo_quality <- min(df$quality, na.rm = TRUE)
print(paste("Máximo de Quality:", maximo_quality, " | Mínimo de Quality:", minimo_quality)) # Máx. e min. de Quality

maximo_alcohol <- max(df$alcohol, na.rm = TRUE)
minimo_alcohol <- min(df$alcohol, na.rm = TRUE)
print(paste("Máximo de álcool:", maximo_alcohol, " | Mínimo de álcool:", minimo_alcohol)) # Máx. e min. de Alcohol

df_numerico <- df %>% select(where(is.numeric))

df_numerico %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free", ncol = 4) +
  geom_histogram(fill = "darkblue", color = "white", bins = 30) +
  theme_minimal() +
  labs(title = "Distribuição das variáveis numéricas")

df_numerico %>% 
  gather() %>%
  ggplot(aes(x = key, y = value)) +
  geom_boxplot(fill = "tomato", alpha = 0.7, outlier.colour = "black", outlier.size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Boxplots",
       x = "Variável", y = "Valor")

cor_matrix <- cor(df_numerico)
corrplot::corrplot(cor_matrix, method = "color", tl.col = "black", type = "full", 
                   tl.cex = 0.8, addCoef.col = "black", number.cex = 0.5) # Matriz de correlação indica poucas variáveis com alta correlação com Quality

ggplot(df, aes(x = alcohol, y = quality)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Relação entre álcool e qualidade") #  Tendência geral é de aumento entre quantidade de álcool e qualidade

ggplot(df, aes(x = density, y = quality)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  theme_minimal() +
  labs(title = "Relação entre densidade e qualidade") # Tendência geral é de que quanto maior a densidade do vinho, menor tende a ser sua qualidade

ggplot(df, aes(x = density, y = alcohol)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  theme_minimal() +
  labs(title = "Relação entre densidade e álcool") # Tendência geral é de que quanto maior a densidade do vinho, menor tende a ser o seu teor alcoólico

ggplot(df, aes(x = fixed.acidity, y = quality)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  theme_minimal() +
  labs(title = "Relação entre acidez corrigida e qualidade") # Tendência leve de que vinhos com maior acidez corrigida serem avaliados como de qualidade um pouco melhor

ggplot(df, aes(x = fixed.acidity, y = density)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  theme_minimal() +
  labs(title = "Relação entre acidez corrigida e densidade") # Tendência geral é de que quanto maior a acidez corrigida, maior a densidade


boxplot(df$alcohol, main = "Boxplot da variável Alcohol", 
        ylab = "Teor Alcoólico", 
        col = "lightblue", border = "darkblue") # Alguns candidatos a outliers


barplot(table(df$quality), 
        main = "Frequência de valores de Quality", 
        xlab = "Qualidade", 
        ylab = "Frequência", 
        col = "skyblue", 
        border = "blue") # Grande parte dos registros com quality entre 5 e 6

barplot(table(df$alcohol), 
        main = "Frequência de valores de Alcohol", 
        xlab = "Teor Alcoólico", 
        ylab = "Frequência", 
        col = "skyblue", 
        border = "blue") # Grande parte dos registros com teor alcóolico entre 9 e 10

ggplot(df, aes(x = alcohol, fill = factor(quality_bin))) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green"), labels = c("Q. inferior (0)", "Q. superior (1)")) +
  labs(title = "Distribuição do álcool por classe de qualidade",
       fill = "Qualidade") # Quanto maior o teor alcoólico, maior a chance do vinho ser considerado de qualidade superior

ggplot(df, aes(x = quality_bin, y = alcohol, fill = quality_bin)) +
  stat_summary(fun = "mean", geom = "bar", width = 0.6) +
  labs(
    title = "Teor Alcoólico Médio por Classificação de Qualidade",
    x = "Classificação da Qualidade",
    y = "Álcool (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Novamente, quanto maior o teor alcoólico, maior a chance do vinho ser considerado de qualidade superior

ggplot(df, aes(x = quality_bin, fill = quality_bin)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribuição de Qualidade do Vinho",
       x = "Qualidade (binária)",
       y = "Contagem") +
  theme_minimal() +
  theme(legend.position = "none") # Classe alvo balanceada, um pouco mais da metade dos registros com vinhos classificados como qualidade superior (igual ou acima de 6) 

table(df$quality_bin) # 855 classificados como "Qualidade superior" e 744 classificados como "Qualidade inferior"

df %>%
  group_by(quality_bin) %>%
  summarise(media_density = mean(density, na.rm = TRUE)) # Média de densidade é bem próxima entre os dois tipos de qualidade (um pouco acima para qualidade inferior)

df %>%
  group_by(quality_bin) %>%
  summarise(media_alcool = mean(alcohol, na.rm = TRUE)) # Como visto, quanto maior o teor alcoólico, maior a chance do vinho ser considerado de qualidade superior

df %>%
  group_by(quality_bin) %>%
  summarise(media_pH = mean(pH, na.rm = TRUE)) # Média de pH não muda entre os dois tipos de qualidade

df %>%
  group_by(quality_bin) %>%
  summarise(media_fixed_acidity = mean(fixed.acidity, na.rm = TRUE)) # Vinhos de qualidade superior indicam ter uma acidez corrigida média maior - parte da acidez que não se evapora facilmente com o calor

# 3. Teste de normalidade ---------------------------------------------
# Escolha por Shapiro-Wilk: Número de observações relativamente baixo (menor que 2000)
# Resultados indicam que as variáveis analisadas não seguem distribuição normal (p < 0.05)
shapiro_alcohol <- shapiro.test(df$alcohol)
print(shapiro_alcohol) # Não segue uma distribuição normal

shapiro_quality <- shapiro.test(df$quality)
print(shapiro_quality) # Não segue uma distribuição normal

shapiro_density <- shapiro.test(df$density)
print(shapiro_density) # Não segue uma distribuição normal

shapiro_fixed_acidity <- shapiro.test(df$fixed.acidity)
print(shapiro_fixed_acidity) # Não segue uma distribuição normal

resultado_shapiro <- lapply(df_numerico, shapiro.test)
sapply(resultado_shapiro, function(x) x$p.value) # Depois de ver individualmente para as principais, agora usando para todos. Nenhuma segue uma distribuição normal


# 4. Correlação -------------------------------------------------------
# Correlação positiva moderada entre álcool e qualidade (Spearman p = 0.48), 
# indicando que, em geral, vinhos com maior teor alcoólico tendem a ter qualidade superior

cor_result_alcohol_quality_p <- cor.test(df$alcohol, df$quality, method = "pearson")
print(cor_result_alcohol_quality_p) # Teste de correlação de Pearson entre Alcohol e Quality (indica correlação moderada e positiva com coeficiente de 0.47)

cor_result_alcohol_density_p <- cor.test(df$alcohol, df$density, method = "pearson")
print(cor_result_alcohol_density_p) # Teste de correlação de Pearson entre Alcohol e Density (indica correlação moderada e negativa com coeficiente de -0.49)

cor_result_alcohol_quality_s <- cor.test(df$alcohol, df$quality, method = "spearman")
print(cor_result_alcohol_quality_s) # Valor semelhante com o uso do método de Spearman comparado a Pearson 

cor_result_alcohol_density_s <- cor.test(df$alcohol, df$density, method = "spearman")
print(cor_result_alcohol_density_s) # Valor semelhante com o uso do método de Spearman comparado a Pearson

# 5. Regressão Linear Simples -----------------------------------------

modelo_quality_alcohol <- lm(quality ~ alcohol, data = df) # Modelo de regressão linear: prevendo quality a partir de alcohol
summary(modelo_quality_alcohol) # R2: 22%; como o modelo de regressão linear foi construído com apenas uma variável preditora (alcohol), é esperado que o valor de R² não seja elevado. O teor alcoólico contribui significativamente para a qualidade do vinho, mas não é o único fator determinante 

qualidade_prevista  <- predict(modelo_quality_alcohol)
qualidade_real <- df$quality

r2 <- summary(modelo_quality_alcohol)$r.squared
valor_mae <- mae(qualidade_real, qualidade_prevista)
valor_rmse <- rmse(qualidade_real, qualidade_prevista)

print(paste("R²:", round(r2, 3))) # O modelo explica 22% da variação da qualidade
print(paste("MAE:", round(valor_mae, 3))) # Em média, o modelo erra cerca de 0.56 pontos na previsão da qualidade do vinho
print(paste("RMSE:", round(valor_rmse, 3))) # O modelo tem um erro médio de aproximadamente 0.71 pontos, sendo ligeiramente mais sensível a grandes desvios

ggplot(df, aes(x = alcohol, y = quality)) +
  geom_jitter(alpha = 0.4, width = 0.1, height = 0.1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
  labs(
    title = "Regressão Linear: qualidade ~ teor alcoólico",
    x = "Álcool (%)",
    y = "Qualidade do Vinho"
  ) +
  theme_minimal() # Tendência geral de que o aumento do álcool leve ao aumento da qualidade do vinho

# 6. Regressão Logística ----------------------------------------------

df$quality_bin <- factor(df$quality_bin, levels = c("Qualidade inferior", "Qualidade superior")) 

modelo_log <- glm(quality_bin ~ alcohol, data = df, family = "binomial") # Modelo de regressão logística para prever a qualidade binária com base no teor alcoólico

summary(modelo_log)

probabilidades <- predict(modelo_log, type = "response")
predicoes <- ifelse(probabilidades > 0.5, "Qualidade superior", "Qualidade inferior")
predicoes <- factor(predicoes, levels = levels(df$quality_bin))

tabela_confusao <- table(Predito = predicoes, Real = df$quality_bin)
acuracia <- mean(predicoes == df$quality_bin)

print(tabela_confusao) # O modelo foi capaz de classificar os vinhos com desempenho consistente e equilibrado entre classes
print(paste("Acurácia do modelo:", round(acuracia * 100, 2), "%")) # O modelo acertou cerca de 70,36% das previsões com base somente no teor alcoólico

saveRDS(modelo_quality_alcohol, file = "modelo_linear.rds")
saveRDS(modelo_log, file = "modelo_logistico.rds")
