#-------------------------------------------------------#
# /Project Felipe Barbier and Felipe Silva  ====
# /Time series ====
# Produto: Iniciação Científica
#-------------------------------------------------------#

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

# TRANSFORMANDO EM SERIE SEMPORAL
brasil_ts = ts(dados$BRASIL/1000, start = c(2004,1), end = c(2023,12), frequency = 12) # /1000 MW TO GW
plot(brasil_ts)

### ANALISANDO A NORMALIDADE DA TS 
qqnorm(brasil_ts)
qqline(brasil_ts)
# Ho = distribuicao normal : p > 0.05
# ha = distribuicao != normal : p <= 0.05 
shapiro.test(brasil_ts)

### TESTANDO A ESTACIONARIDADE DA TS KPSS (kwiatkowski - phillips - schimidt- shin)
#Ha = nao eh estacionaria: teste estatistico > valor critico
#ho = eh estacionaria: teste estatistico <= valor critico
kpss = ur.kpss(brasil_ts)
summary(kpss)

### TESTANDO A AUTOCORRELACAO GRAFICAMENTE
# SE ESTIVER FORA DO INTERVALO DE CONFIANCA, POSSUI GRANDE CHANCE DE TER AUCORORRELACAO
acf(brasil_ts) 
pacf(brasil_ts)

### TESTANDO A AUTOCORRELACAO (Ljung - Box)
# Ho = nao eh autocorrelacionado: p > 0.05
# Ha = eh autocorrelacionado: p <= 0.05
Box.test(brasil_ts, type = 'Ljung')

# SEPARANDO EM TREINO (2004 - 2022) E TESTE (2023)
brasil_treino = window(brasil_ts, start = c(2004,1), end = c(2022,12))
brasil_teste = window(brasil_ts, start = c(2023,1), end = c(2023,12))

# APLICANDO OS MODELOS
modelo_ingenuo = snaive(brasil_treino, h = 12)
modelo_arima = Arima(brasil_treino, order = c(0,0,0), seasonal = list(order = c(0,1,0), period = 12))
modelo_nnar = nnetar(brasil_treino, p = 18, size = 24)
modelo_mlp = mlp(brasil_treino, reps = 30, hd = c(12,12), lags = 12)
modelo_elm = elm(brasil_treino, hd = 60, reps = 40)
modelo_ets = ets(brasil_treino)

modelos = list()
modelos = list(
  INGENUO = modelo_ingenuo,
  ARIMA = modelo_arima,
  NNAR  = modelo_nnar,
  MLP   = modelo_mlp,
  ELM   = modelo_elm,
  ETS   = modelo_ets
)

# DADOS AJUSTADOS
ajustados = list()
for (nome in names(modelos)) {
  ajustados[[nome]] = modelos[[nome]]$fitted
}
print(ajustados)

# PREVISAO PARA 12 MESES
previsoes = list()
for (nome in names(modelos)) {
  previsoes[[nome]] = forecast(modelos[[nome]], h = length(brasil_teste))
  previsoes[[nome]] = previsoes[[nome]]$mean
}
print(previsoes)

# METRICAS AJUSTES
metricas_ajuste = list()
for (nome in names(ajustados)) {
  metricas_ajuste[[nome]] = accuracy(brasil_treino, ajustados[[nome]])
}
print(metricas_ajuste)

# METRICAS PREVISAO
metricas_previsao = list()
for (nome in names(previsoes)) {
  metricas_previsao[[nome]] = accuracy(brasil_teste, previsoes[[nome]])
}
print(metricas_previsao)

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

# SALVANDO A TABELA DE METRICAS DENTRO DE UMA TABELA NO EXCEL
arquivo_ajuste = "C:/Users/Felipe Barbier/OneDrive/Documentos/UFRRJ/IniciacaoCientifica/tabela_metricas_ajuste.xlsx"
write_xlsx(df_metricas_ajuste, arquivo_ajuste)
arquivo_previsao = "C:/Users/Felipe Barbier/OneDrive/Documentos/UFRRJ/IniciacaoCientifica/tabela_metricas_previsao.xlsx"
write_xlsx(df_metricas_previsao, arquivo_previsao)






# Pegando os tempos da série brasil_teste
#tempo <=time(brasil_teste)
# Criando rótulos no formato "2023.1", "2023.2", ..., "2023.12"
#meses_formatados = seq(from = 2023.1, by = 0.1, length.out = length(tempo))
# Plotando sem eixo X automático
#plot(brasil_teste, xaxt = "n", xlab = "Ano.Mês", ylab = "Valores")
# Adicionando os rótulos corretamente
#axis(1, at = tempo, labels = meses_formatados)
