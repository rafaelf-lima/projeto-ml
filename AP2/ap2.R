install.packages("tidyverse")
install.packages("moments")
install.packages('nortest')
install.packages('ROSE')
library(nortest)
library(pROC)
library(tidyverse)
library(corrplot)
library(ROSE)
library(moments)

# ----------------------------------------------------------------------------------------------------
# PERGUNTAS
# 1. Quais fatores demográficos e comportamentais estão mais associados ao risco de inadimplência?
# 2. É possível construir um modelo preditivo eficiente para identificar clientes que provavelmente vão dar default no próximo mês?
# 3. Como as características do histórico de pagamento (atrasos e valores pagos) se relacionam com o comportamento futuro de pagamento?
# ----------------------------------------------------------------------------------------------------

# Importação do dataset
df <- read_csv("UCI_Credit_Card.csv")
sum(is.na(df)) # 0
# Transformação das variáveis em factor
df <- df %>%
  mutate(
    SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    EDUCATION = factor(EDUCATION, levels = c(0,1, 2, 3, 4, 5, 6),
                       labels = c("Unknown", "Graduate School", "University", "High School", "Others", "Unknown", "Unknown")),
    MARRIAGE = factor(MARRIAGE, levels = c(0, 1, 2, 3),
                      labels = c("Unknown", "Married", "Single", "Others")),
    default.payment.next.month = factor(default.payment.next.month, levels = c(0, 1),
                                        labels = c("No", "Yes"))
  )

# Não há valores nulos
sum(is.na(df))

# ----------------------------------------------------------------------------------------------------
# Visão geral do dataset
head(df)
# Tipos dos dados
str(df)
# Estatística descritiva das variáveis
summary(df)
# Visualização do dataset
glimpse(df)

# 35 valores duplicados
sum(duplicated(df))
df[duplicated(df), ]

# 30000 linhas | 25 colunas 
dim(df)

# ----------------------------------------------------------------------------------------------------
# Criação da variável age_range (faixa de idade)
breaks <- seq(20, 80, by = 10)

df$age_range <- cut(df$AGE,
                    breaks = breaks,
                    right = FALSE,
                    include.lowest = TRUE,
                    labels = c("21-29", "30-39", "40-49", "50-59", "60-69", "70-79"))

# Exclusão da variável ID
df <- df[, !(names(df) %in% "ID")]
names(df)

# ----------------------------------------------------------------------------------------------------
# Criação de dataframe com variáveis numéricas
df_numerico <- df %>% select(where(is.numeric))

# Gráficos (EDA)
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

# Valores únicos das variáveis abaixo
unique(df$SEX)
unique(df$AGE)
unique(df$EDUCATION)
unique(df$MARRIAGE)
unique(df$PAY_0)
# Idade mínima
min(df$AGE)
# Idade máxima
max(df$AGE)

# ----------------------------------------------------------------------------------------------------
# Visualização de assimetria e curtose
assimetria <- sapply(df_numerico, skewness)
curtose <- sapply(df_numerico, kurtosis)
assimetria
curtose

# ----------------------------------------------------------------------------------------------------
# Comparação entre os rótulos da variável alvo - alto desbalanceamento
table(df$default.payment.next.month)

# Gráficos (EDA)
barplot(table(df$SEX), 
        main = "Frequência de valores de gênero", 
        xlab = "Gênero", 
        ylab = "Frequência", 
        col = "skyblue", 
        border = "blue")

barplot(table(df$EDUCATION), 
        main = "Frequência de valores de nível escolar", 
        xlab = "Nível escolar", 
        ylab = "Frequência", 
        col = "skyblue", 
        border = "blue")

barplot(table(df$MARRIAGE), 
        main = "Frequência de valores de estado civil", 
        xlab = "Estado civil", 
        ylab = "Frequência", 
        col = "skyblue", 
        border = "blue")

barplot(table(df$PAY_0), 
        main = "Frequência de valores de PAY_0", 
        xlab = "PAY_0", 
        ylab = "Frequência", 
        col = "skyblue", 
        border = "blue")

# ----------------------------------------------------------------------------------------------------
# Matriz de correlação - todas as variáveis numéricas
cor_matrix <- cor(df_numerico)
corrplot(cor_matrix, method = "color", tl.col = "black", type = "full", 
                   tl.cex = 0.4, addCoef.col = "black", number.cex = 0.4)

# Matriz de correlação - PAY_[0-9]
pay_cols <- grep("^PAY_[0-9]", names(df), value = TRUE)
cor_pay <- cor(df[, pay_cols], use = "complete.obs")
corrplot(cor_pay, method = "color", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)

# Matriz de correlação - PAY_AMT
pay_amt_cols <- grep("^PAY_AMT", names(df), value = TRUE)
cor_pay_amt <- cor(df[, pay_amt_cols], use = "complete.obs")
corrplot(cor_pay_amt, method = "color", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)

# Matriz de correlação - BILL_AMT
bill_cols <- grep("^BILL_AMT", names(df), value = TRUE)
cor_bill <- cor(df[, bill_cols], use = "complete.obs")
corrplot(cor_bill, method = "color", tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7)

# ----------------------------------------------------------------------------------------------------
# Gráficos (EDA)
ggplot(df, aes(x = default.payment.next.month)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição da variável Default",
       x = "Default (0=Não, 1=Sim)",
       y = "Contagem") +
  theme_minimal()

ggplot(df, aes(x = SEX, fill = default.payment.next.month)) +
  geom_bar(position = "dodge") +
  labs(title = "Default por Sexo",
       x = "Sexo (1=Masculino, 2=Feminino)",
       y = "Contagem",
       fill = "Default") +
  theme_minimal()

ggplot(df, aes(x = EDUCATION, fill = default.payment.next.month)) +
  geom_bar(position = "dodge") +
  labs(title = "Default por Nível de Escolaridade",
       x = "Educação (1=Graduação, 2=Universidade, 3=Ensino Médio, 4-6=Outros)",
       y = "Contagem",
       fill = "Default") +
  theme_minimal()

ggplot(df, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  labs(title = "Distribuição da Idade",
       x = "Idade",
       y = "Contagem") +
  theme_minimal()

ggplot(df, aes(x = age_range, fill = default.payment.next.month)) +
  geom_bar(position = "dodge") +
  labs(title = "Default por Faixa Etária",
       x = "Faixa Etária (Age Range)",
       y = "Contagem",
       fill = "Default\n(0 = Não, 1 = Sim)") +
  theme_minimal()

# ----------------------------------------------------------------------------------------------------
# seguindo o teste de Anderson-Darling, é possível observar que nenhuma variável segue uma distribuição normal
ad.test(df$LIMIT_BAL)
ad.test(df$BILL_AMT1)
ad.test(df$BILL_AMT2)
ad.test(df$BILL_AMT3)
ad.test(df$BILL_AMT4)
ad.test(df$BILL_AMT5)
ad.test(df$BILL_AMT6)
ad.test(df$PAY_AMT1)
ad.test(df$PAY_AMT2)
ad.test(df$PAY_AMT3)
ad.test(df$PAY_AMT4)
ad.test(df$PAY_AMT5)
ad.test(df$PAY_AMT6)

# Testes de LIMIT_BAL: relevantes para entender o perfil de crédito dos clientes (crédito concedido)
# LIMIT_BAL ~ EDUCATION: diferença estatisticamente significativa entre níveis de escolaridade
summary(aov(LIMIT_BAL ~ EDUCATION, data = df))
# LIMIT_BAL ~ MARRIAGE: diferença estatisticamente significativa entre estados civis
summary(aov(LIMIT_BAL ~ MARRIAGE, data = df))
# LIMIT_BAL ~ SEX: diferença estatisticamente significativa entre homens e mulheres
t.test(LIMIT_BAL ~ SEX, data = df)
# LIMIT_BAL ~ default: limite médio maior para quem não deu default (178099.7 vs 130109.7)
t.test(LIMIT_BAL ~ default.payment.next.month, data = df)

# ----------------------------------------------------------------------------------------------------
# Separação em treino e teste (70% | 30%)
set.seed(123)
train_indices <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Criação de oversampling - alto desbalancemamento na variável alvo
df_balanced <- ovun.sample(default.payment.next.month ~ ., 
                           data = train_data, 
                           method = "over", 
                           N = 2 * table(train_data$default.payment.next.month)[1])$data

# Criação de um modelo de regressão logística, a partir de múltiplas variáveis
model <- glm(default.payment.next.month ~ 
      LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE +
      PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
      BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
      PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
      data = df_balanced, 
      family = binomial)

# Modelo salvo (RDS)
saveRDS(model, "credit_default_model.rds")

# Calcula probabilidades previstas
test_data$predicted_prob <- predict(model, newdata = test_data, type = "response")
# Classifica probabilidades (>0.5 = "Yes")
test_data$predicted_class <- ifelse(test_data$predicted_prob >= 0.5, "Yes", "No") %>% 
  factor(levels = c("No", "Yes"))

# Gera matriz de confusão
table(Predicted = test_data$predicted_class, Actual = test_data$default.payment.next.month)
# Calcula acurácia (67,63%)
acuracia <- mean(test_data$predicted_class == test_data$default.payment.next.month, na.rm = TRUE)
cat("Acurácia:", acuracia, "\n")

# AUC - área sob curva ROC (71,27%)
roc_obj <- roc(test_data$default.payment.next.month, test_data$predicted_prob)
auc_valor <- auc(roc_obj)
cat("AUC:", auc_valor, "\n")