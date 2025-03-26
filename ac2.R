#* @apiTitle API de Classificação de Espécies Iris
#* @param Sepal.Length
#* @param Sepal.Width
#* @param Petal.Length
#* @param Petal.Width
#* @get /prever
function(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) {
  
  Sepal.Length <- as.numeric(Sepal.Length)
  Sepal.Width <- as.numeric(Sepal.Width)
  Petal.Length <- as.numeric(Petal.Length)
  Petal.Width <- as.numeric(Petal.Width)
  
  if(any(is.na(c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)))) {
    return(list(error = "Parâmetro inválido"))
  }
  
  novos_dados <- data.frame(
    Sepal.Length = Sepal.Length,
    Sepal.Width = Sepal.Width,
    Petal.Length = Petal.Length,
    Petal.Width = Petal.Width
  )
  
  probabilidade <- predict(modelo_log, novos_dados, type = "response")
  classe <- ifelse(probabilidade > 0.5, "versicolor", "setosa")
  
  return(list(
    sepal_length = Sepal.Length,
    sepal_width = Sepal.Width,
    petal_length = Petal.Length,
    petal_width = Petal.Width,
    classe_prevista = classe
  ))
  
  # r <- plumb("ac2.R")
  # r$run(port = 8000)
}