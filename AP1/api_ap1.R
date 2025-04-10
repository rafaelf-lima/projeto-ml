#* @apiTitle AP1 - Red Wine Quality
#* @apiDescription Esta API permite prever a qualidade do vinho com base no teor alcoólico, usando modelos de regressão linear e logística treinados previamente.
# Treinamentos feitos no outro arquivo (ap1.R)
modelo_quality_alcohol <- readRDS("modelo_linear.rds")
modelo_log <- readRDS("modelo_logistico.rds")

#* Predição de qualidade (regressão linear com base no teor alcoólico)
#* @param alcohol O teor alcoólico do vinho (ex: 11.5)
#* @get /predicao
function(alcohol) {
  alcohol <- as.numeric(alcohol)
  pred <- predict(modelo_quality_alcohol, newdata = data.frame(alcohol = alcohol))
  list(qualidade_prevista = round(pred, 2))
}

#* Classificação da qualidade do vinho com base no teor alcoólico (regressão logística)
#* @param alcohol O teor alcoólico do vinho (ex: 11.5)
#* @get /classificacao
function(alcohol) {
  alcohol <- as.numeric(alcohol)
  prob <- predict(modelo_log, newdata = data.frame(alcohol = alcohol), type = "response")
  classe <- ifelse(prob > 0.5, "Qualidade superior", "Qualidade inferior")
  list(
    classe_prevista = classe
  )
}