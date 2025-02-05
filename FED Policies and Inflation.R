# LIBRERÍAS

# Análisis financiero
library(quantmod)

# Series temporales
library(tseries)

# Transformación de datos
library(dplyr)
library(reshape2)

# Modelado de datos
library(lmtest)
library(randomForest)
library(caret)

# Gráficos
library(ggplot2)


# 1 CARGA DE DATOS

# Obtener los datos de la tasa de interés de la FED y de la inflación (IPC)
getSymbols("FEDFUNDS", src = "FRED", from = "1990-01-01", to = Sys.Date())
getSymbols("CPIAUCNS", src = "FRED", from = "1990-01-01", to = Sys.Date())

# Convertir a Data Frame y renombrar la columna
fed_df <- data.frame(
  Date = index(FEDFUNDS),
  FED = as.numeric(FEDFUNDS[, 1])
)

ipc_df <- data.frame(
  Date = index(CPIAUCNS),
  IPC = as.numeric(CPIAUCNS[, 1])
)

# Calcular el incremento mensual del IPC
ipc_df$IPC_Monthly <- c(NA, diff(ipc_df$IPC))

# Graficar tasa de interés de la FED
ggplot(fed_df, aes(x = Date, y = FED)) +
  geom_line() +
  ggtitle("Tasa de Interés de la FED") +
  xlab("Fecha") +
  ylab("Tasa (%)") +
  theme_minimal()

# Graficar inflación (IPC)
ggplot(ipc_df, aes(x = Date, y = IPC_Monthly)) +
  geom_line() +
  ggtitle("Índice de Precios al Consumidor (Inflación)") +
  xlab("Fecha") +
  ylab("Índice") +
  theme_minimal()


# 2 RELACIÓN ENTRE FED & IPC

# 2.1 CORRELACIÓN

# Correlación entre la tasa de la FED y la inflación mes a mes
merged_df <- merge(fed_df, ipc_df, by = "Date")
correlacion <- cor(merged_df$FED, merged_df$IPC_Monthly, use = "complete.obs")
print(correlacion)

# Correlación entre la tasa de la FED y la inflación año a año

# Promedio anual de la FED
fed_df$Year <- format(fed_df$Date, "%Y")
fed_annual <- aggregate(FED ~ Year, data = fed_df, FUN = mean)

# Promedio anual del IPC
ipc_df$Year <- format(ipc_df$Date, "%Y")
ipc_annual <- aggregate(IPC ~ Year, data = ipc_df, FUN = mean)
ipc_annual$IPC_Annualy <- c(NA, diff(ipc_annual$IPC))

merged_annual <- merge(fed_annual, ipc_annual, by = "Year")
merged_annual <- na.omit(merged_annual)
correlacio_annual <- cor(merged_annual$FED, merged_annual$IPC_Annualy, use = "complete.obs")
print(correlacio_annual)

# 2.2 TEST DE CAUSALIDAD DE GRANGER

# Series temporaleS
merged_df <- na.omit(merged_df)
fed_ts <- ts(merged_df$FED, start = c(2000, 1), frequency = 12)
ipc_ts <- ts(merged_df$IPC_Monthly, start = c(2000, 1), frequency = 12)

# Lags a utilizar: 12 meses
granger_test <- grangertest(ipc_ts ~ fed_ts, order = 12) # FED -> IPC
print(granger_test)

# 2.3 REZAGOS

# FED
for (i in 1:12) {
  fed_df <- fed_df %>%
    mutate(!!paste("FED_lag", i, sep = "_") := lag(FED, i))
}

# IPC
for (i in 1:12) {
  ipc_df <- ipc_df %>%
    mutate(!!paste("IPC_lag", i, sep = "_") := lag(IPC_Monthly, i))
}

# Unir las series de tiempo
merged_df <- merge(fed_df, ipc_df, by = "Date")

# Regresión de la inflación actual sobre los rezagos de la tasa de la FED
model <- lm(IPC_Monthly ~ FED_lag_1 + FED_lag_2 + FED_lag_3 + FED_lag_4 + FED_lag_5 +FED_lag_6 + FED_lag_7 + FED_lag_8 + FED_lag_9 + FED_lag_10 + FED_lag_11 + FED_lag_12, data = merged_df)
summary(model)

# IPC -> FED
model_fed <- lm(FED ~ IPC_lag_1 + IPC_lag_2 + IPC_lag_3 + 
                  IPC_lag_4 + IPC_lag_5 + IPC_lag_6 + IPC_lag_7 + 
                  IPC_lag_8 + IPC_lag_9 + IPC_lag_10 + IPC_lag_11 + IPC_lag_12, 
                data = merged_df)
summary(model_fed)


# 3 OTRAS COMPONENTES

# IPC Núcleo (excluye alimentos y energía)
getSymbols("CPILFESL", src = "FRED", from = "1990-01-01", to = Sys.Date())
core_ipc_df <- data.frame(
  Date = index(CPILFESL),
  IPC_core = as.numeric(CPILFESL[, 1])
)

#  Expectativas de inflación de la Universidad de Michigan
getSymbols("MICH", src = "FRED", from = "1990-01-01", to = Sys.Date())
expectations_df <- data.frame(
  Date = index(MICH),
  IPC_exp = as.numeric(MICH[, 1])
)

# Ratio Dólar vs Euro (U.S. Dollars to Euro Spot Exchange Rate) [https://fred.stlouisfed.org/series/DEXUSEU]
getSymbols("DEXUSEU", src = "FRED", from = "1990-01-01", to = Sys.Date())
dolar_df <- data.frame(
  Date = index(DEXUSEU),
  USD_EU = as.numeric(DEXUSEU[, 1])
)

# Incorporación de variables
merged_df <- merge(merged_df, core_ipc_df, by = "Date", all.x = TRUE)
merged_df <- merge(merged_df, expectations_df, by = "Date", all.x = TRUE)
merged_df <- merge(merged_df, dolar_df, by = "Date", all.x = TRUE)
merged_df <- na.omit(merged_df)

# Modelo extendido
model_fed_extended <- lm(FED ~ IPC_core + IPC_exp + USD_EU +
                           IPC_lag_1 + IPC_lag_2 + IPC_lag_3 + 
                           IPC_lag_4 + IPC_lag_5 + IPC_lag_6 + 
                           IPC_lag_7 + IPC_lag_8 + IPC_lag_9 + 
                           IPC_lag_10 + IPC_lag_11 + IPC_lag_12, 
                         data = merged_df)

# Matriz de correlación
cor_matrix <- cor(merged_df[, c("FED", "IPC_core", "IPC_exp", "USD_EU", 
                                "IPC_lag_1", "IPC_lag_2", "IPC_lag_3", 
                                "IPC_lag_4", "IPC_lag_5", "IPC_lag_6", 
                                "IPC_lag_7", "IPC_lag_8", "IPC_lag_9", 
                                "IPC_lag_10", "IPC_lag_11", "IPC_lag_12")], 
                  use = "complete.obs")

cor_matrix_melted <- melt(cor_matrix)

ggplot(cor_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#D64550", high = "#1AAB40", mid = "#FFFFFF", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Variables") +
  ylab("Variables") +
  ggtitle("Mapa de calor de correlaciones")


# 4 ÚLTIMOS AÑOS

start_date <- as.Date("2020-01-01")
filtered_df <- merged_df[merged_df$Date >= start_date, ]

model_last_years <- lm(FED ~ IPC_core + IPC_exp + USD_EU +
                           IPC_lag_1 + IPC_lag_2 + IPC_lag_3 + 
                           IPC_lag_4 + IPC_lag_5 + IPC_lag_6 + 
                           IPC_lag_7 + IPC_lag_8 + IPC_lag_9 + 
                           IPC_lag_10 + IPC_lag_11 + IPC_lag_12, 
                         data = filtered_df)

# Matriz de correlación
cor_matrix_last_years <- cor(filtered_df[, c("FED", "IPC_core", "IPC_exp", "USD_EU", 
                                "IPC_lag_1", "IPC_lag_2", "IPC_lag_3", 
                                "IPC_lag_4", "IPC_lag_5", "IPC_lag_6", 
                                "IPC_lag_7", "IPC_lag_8", "IPC_lag_9", 
                                "IPC_lag_10", "IPC_lag_11", "IPC_lag_12")], 
                  use = "complete.obs")

cor_matrix_melted_last_years <- melt(cor_matrix_last_years)

ggplot(cor_matrix_melted_last_years, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#D64550", high = "#1AAB40", mid = "#FFFFFF", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Variables") +
  ylab("Variables") +
  ggtitle("Mapa de calor de correlaciones - 2020 en adelante")


# 5 MODELO

# Convertir columnas a numéricas
filtered_df[] <- lapply(filtered_df, function(x) {
  if (is.factor(x) || is.character(x)) {
    as.numeric(as.factor(x))
  } else {
    x 
  }
})

# Normalizar los datos (excepto 'Date')
filtered_df_norm <- filtered_df %>%
  mutate(across(-Date, scale))

# Dividir 'train' y 'test'
set.seed(54)  # Reproducibilidad
trainIndex <- createDataPartition(filtered_df_norm$FED, p = 0.8, list = FALSE)
trainData <- filtered_df_norm[trainIndex, ]
testData <- filtered_df_norm[-trainIndex, ]

# Entrenar Random Forest
model_rf <- randomForest(FED ~ IPC_core + IPC_exp + USD_EU + IPC_lag_1 + 
                           IPC_lag_2 + IPC_lag_3 + IPC_lag_4 + IPC_lag_5 + 
                           IPC_lag_6 + IPC_lag_7 + IPC_lag_8 + IPC_lag_9 + 
                           IPC_lag_10 + IPC_lag_11 + IPC_lag_12, 
                         data = trainData, 
                         ntree = 500)

predictions <- predict(model_rf, newdata = testData)

# Métricas: Error cuadrático medio & R^2
mse <- mean((predictions - testData$FED)^2)
rsq <- 1 - sum((predictions - testData$FED)^2) / sum((testData$FED - mean(testData$FED))^2)
print(paste("MSE: ", mse))
print(paste("R-squared: ", rsq))

importance(model_rf)

# 6 HIPERPARÁMETROS

# mtry: Número de variables consideradas en cada división del árbol.
# ntree: Número total de árboles en el bosque.
# nodesize: Número mánimo de observaciones en cada nodo terminal.
# maxnodes: Número máximo de nodos en un árbol.

set.seed(54)

# Definir hiperparámetros a probar
tuneGrid <- expand.grid(mtry = seq(2, ncol(trainData) - 1, by = 2))

# Guardar errores OOB para distintos valores de mtry
mtry_values <- 1:(ncol(trainData) - 1)
oob_errors <- numeric(length(mtry_values))

for (i in mtry_values) {
  rf_model <- randomForest(FED ~ ., data = trainData, mtry = i, ntree = 500)
  oob_errors[i] <- rf_model$mse[length(rf_model$mse)]  # Último valor de error OOB
}

# Graficar OOB error vs mtry
df <- data.frame(mtry = mtry_values, oob_error = oob_errors)
ggplot(df, aes(x = mtry, y = oob_error)) +
  geom_point(color = "#118DFF", size = 2) +
  geom_line(color = "#118DFF") +
  ggtitle("Error OOB vs. mtry") +
  xlab("Número de variables seleccionadas en cada división (mtry)") +
  ylab("Error OOB") +
  theme_minimal()

# 6.1 VALIDACIÓN CRUZADA

# Encontrar el mejor mtry
rf_tuned <- train(FED ~ ., data = trainData, 
                  method = "rf", 
                  tuneGrid = tuneGrid,
                  trControl = trainControl(method = "cv", number = 5),  # 5-fold CV
                  ntree = 500)

print(rf_tuned$bestTune)

# Entrenar el modelo con el mejor mtry
model_rf <- randomForest(FED ~ ., 
                         data = trainData, 
                         mtry = rf_tuned$bestTune$mtry,  # Mejor mtry
                         ntree = 500, 
                         nodesize = 5)  # Puedes ajustar `nodesize`

predictions <- predict(model_rf, newdata = testData)

mse <- mean((predictions - testData$FED)^2)
rsq <- 1 - sum((predictions - testData$FED)^2) / sum((testData$FED - mean(testData$FED))^2)

print(paste("MSE: ", mse))
print(paste("R-squared: ", rsq))
