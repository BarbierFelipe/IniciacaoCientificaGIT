rm(list=ls())
getwd()
# CARREGANDO PACOTES
library(dplyr)
library(MLmetrics)
library(forecast)
library(readxl)
library(urca)
library(nnfor)
library(keras)
library(writexl)
# IMPORTANDO OS DADOS
setwd("C:/Users/Felipe Barbier/OneDrive/Documentos/UFRRJ/IniciacaoCientifica") 
dados = read_excel('Dados_consumo_industria_corrigida.xlsx')

url = "Dados_consumo_industria_corrigida.xlsx"
dados2 = read_excel(url)
print(dados)


# TRANSFORMANDO EM SERIE SEMPORAL
brasil_ts = ts(dados$BRASIL/1000, start = c(2004,1), end = c(2023,12), frequency = 12) # /1000 MW TO GW.

# SEPARANDO EM TREINO (2004 - 2022) E TESTE (2023)
brasil_treino = window(brasil_ts, start = c(2004,1), end = c(2022,12))
brasil_teste = window(brasil_ts, start = c(2023,1), end = c(2023,12))

# APLICANDO OS MODELOS
modelo_ingenuo = snaive(brasil_treino, h = 12)
modelo_arima = Arima(brasil_treino, order = c(2,1,2), seasonal = list(order = c(0,1,0), period = 12))

modelos = list()
modelos = list(
  INGENUO = modelo_ingenuo,
  ARIMA = modelo_arima
)

ajustados = list()
for (nome in names(modelos)) {
  ajustados[[nome]] = modelos[[nome]]$fitted
}

previsoes = list()
for (nome in names(modelos)) {
  previsoes[[nome]] = forecast(modelos[[nome]], h = length(brasil_teste))
  previsoes[[nome]] = previsoes[[nome]]$mean
}

metricas_ajuste = list()
for (nome in names(ajustados)) {
  metricas_ajuste[[nome]] = accuracy(brasil_treino, ajustados[[nome]])
}

metricas_previsao = list()
for (nome in names(previsoes)) {
  metricas_previsao[[nome]] = accuracy(brasil_teste, previsoes[[nome]])
}

# CONVERTER AS METRICAS EM UM DATA.FRAME PARA FACILITAR A ANALISE
## AJUSTE
df_metricas_ajuste = data.frame()
df_metricas_ajuste = data.frame(Modelo = character(),
                                RMSE = numeric(),
                                MAE = numeric(),
                                MAPE = numeric(), stringsAsFactors = FALSE)
for (nome in names(metricas_ajuste)){
  df_temp = data.frame(
    Modelo = nome, 
    RMSE = metricas_ajuste[[nome]]["Test set", "RMSE"] %>% round(3), 
    MAE = metricas_ajuste[[nome]]["Test set", "MAE"] %>% round(3), 
    MAPE = metricas_ajuste[[nome]]["Test set", "MAPE"] %>% round(3)
  )
  df_metricas_ajuste = rbind(df_metricas_ajuste, df_temp)
}
print(df_metricas_ajuste)

## PREVISAO
df_metricas_previsao = data.frame()
df_metricas_previsao = data.frame(Modelo = character(),
                                  RMSE = numeric(),
                                  MAE = numeric(),
                                  MAPE = numeric(), stringsAsFactors = FALSE)
for (nome in names(metricas_previsao)){
  df_temp = data.frame(
    Modelo = nome, 
    RMSE = metricas_previsao[[nome]]["Test set", "RMSE"] %>% round(3), 
    MAE = metricas_previsao[[nome]]["Test set", "MAE"] %>% round(3), 
    MAPE = metricas_previsao[[nome]]["Test set", "MAPE"] %>% round(3)
  )
  df_metricas_previsao = rbind(df_metricas_previsao, df_temp)
}
print(df_metricas_previsao)

plot(brasil_treino, 
     main = "(a)", 
     xlab = "Tempo", 
     ylab = "Consumo de energia elétrica (GWh)", 
     col = "black", 
     lwd = 2,
     type = "o", 
     pch = 1,
)
grid(col = "gray", lty = "dotted")
# Linha do modelo Ingênuo
lines(ajustados$INGENUO, 
      col = "red", 
      lty = 2, 
      lwd = 2)

# Linha do modelo ARIMA
lines(ajustados$ARIMA, 
      col = "blue", 
      lty = 2, 
      lwd = 2)

# Legenda
legend("topleft", 
       legend = c("Série temporal", "Ingênuo sazonal", "Modelo ARIMA"),
       col = c("black", "red", "blue"),
       lty = c(1, 2, 2),
       lwd = 2,
       pch = c(1, NA, NA),
       bty = "n")

