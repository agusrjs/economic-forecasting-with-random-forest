## Librerías

# Análisis financiero
library(quantmod)

# Series temporales
library(tseries)

# Transformación de datos
library(dplyr)
library(tidyr)
library(reshape2)
library(kableExtra)
library(purrr)

# Modelado de datos
library(lmtest)
library(randomForest)
library(caret)

# Gráficos
library(ggplot2)
```

# 1 Carga de datos

first_date <- "1990-01-01"

# Obtener los datos de la tasa de interés de la FED y de la inflación (IPC)
getSymbols("FEDFUNDS", src = "FRED", from = first_date, to = Sys.Date())
getSymbols("CPIAUCNS", src = "FRED", from = first_date, to = Sys.Date())

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

# 2 Relación entre FED & IPC

# 2.1 Correlación

# Correlación entre la tasa de la FED y la inflación mes a mes
merged_df <- merge(fed_df, ipc_df, by = "Date")
correlacion <- cor(merged_df$FED, merged_df$IPC_Monthly, use = "complete.obs")

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

ggplot(merged_df, aes(x = Date)) +
  geom_line(aes(y = FED, color = "FED")) +
  geom_line(aes(y = IPC_Monthly * 2, color = "IPC")) +  
  scale_y_continuous(
    name = "Tasa de la FED (%)",
    sec.axis = sec_axis(~ . / 2, name = "Inflación Mensual (IPC)")
  ) +
  labs(
    title = "Evolución de la Tasa de la FED y la Inflación Mensual",
    x = "Fecha",
    color = "Variable"
  ) +
  theme_minimal()

# 2.2 Test de causalidad de Granger

# Series temporaleS
merged_df <- na.omit(merged_df)
fed_ts <- ts(merged_df$FED, start = c(2000, 1), frequency = 12)
ipc_ts <- ts(merged_df$IPC_Monthly, start = c(2000, 1), frequency = 12)

# Lags a utilizar: 12 meses
granger_test <- grangertest(ipc_ts ~ fed_ts, order = 12) # FED -> IPC
print(granger_test)

# 2.3 Rezagos y valores futuros

# Crear los rezagos de FED

months <- 36

for (i in 1:months) {
  merged_df <- merged_df %>%
    mutate(!!paste("FED_lag", i, sep = "_") := lag(FED, i))
}

# Calcular la correlación entre IPC_Monthly y los rezagos
correlations_1 <- sapply(1:months, function(i) cor(merged_df$IPC_Monthly, merged_df[[paste("FED_lag", i, sep = "_")]], use = "complete.obs"))

# Crear un dataframe para la tabla
correlation_table_1 <- data.frame(
  Lag = paste("FED_lag", 1:months, sep = "_"),
  Correlation = correlations_1
)

correlation_table_1

# Gráfico de correlaciones

# Etiquetas del eje X
correlation_table_1$Lag <- as.numeric(gsub("FED_lag_", "", correlation_table_1$Lag))
correlation_table_1$Lag <- factor(correlation_table_1$Lag, levels = sort(unique(correlation_table_1$Lag)))

ggplot(correlation_table_1, aes(x = Lag, y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "#1ac5ca", high = "#f87970", mid = "#ffffff", midpoint = 0, name = "Correlación", limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlación entre IPC y los rezagos de la tasa de la FED",
       x = "Rezagos de la FED",
       y = "Correlación") +
  scale_y_reverse()

# 3 Otros indicadores de inflación

# IPC Núcleo
getSymbols("CPILFESL", src = "FRED", from = first_date, to = Sys.Date())
core_ipc_df <- data.frame(
  Date = index(CPILFESL),
  IPC_core = as.numeric(CPILFESL[, 1])
)

core_ipc_df$IPC_core_Monthly <- c(NA, diff(core_ipc_df$IPC_core))
core_ipc_df$IPC_core_Monthly <- as.numeric(core_ipc_df$IPC_core_Monthly)

#  Expectativas de inflación de la Universidad de Michigan
getSymbols("MICH", src = "FRED", from = first_date, to = Sys.Date())
expectations_df <- data.frame(
  Date = index(MICH),
  IPC_exp = as.numeric(MICH[, 1])
)

# Incorporación de variables
merged_df <- merge(merged_df, core_ipc_df, by = "Date", all.x = TRUE)
merged_df <- merge(merged_df, expectations_df, by = "Date", all.x = TRUE)
merged_df <- na.omit(merged_df)

# Correlación

# IPC_core
correlations_core_1 <- sapply(1:months, function(i) cor(merged_df$IPC_core_Monthly, merged_df[[paste("FED_lag", i, sep = "_")]], use = "complete.obs"))

# IPC_exp
correlations_exp_1 <- sapply(1:months, function(i) cor(merged_df$IPC_exp, merged_df[[paste("FED_lag", i, sep = "_")]], use = "complete.obs"))

# Tabla de correlaciones
correlation_table_2 <- data.frame(
  Lag = 1:months,
  IPC_Monthly = correlations_1,
  IPC_core = correlations_core_1,
  IPC_exp = correlations_exp_1
)

correlation_table_2

# Tabla con las tres correlaciones
correlation_graph_2 <- data.frame(
  Lag = rep(1:months, 3),
  IPC = c(correlations_1, correlations_core_1, correlations_exp_1),
  Type = rep(c("IPC", "IPC Núcleo", "IPC expectativa"), each = months)
)

# Gráfico de correlaciones

# Etiquetas del eje X
correlation_graph_2$Lag <- as.factor(correlation_table_2$Lag)
correlation_graph_2$Lag <- as.numeric(as.character(correlation_graph_2$Lag)) # Lag a numérico

ggplot(correlation_graph_2, aes(x = Lag, y = IPC, fill = IPC)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient2(low = "#1ac5ca", high = "#f87970", mid = "#ffffff", midpoint = 0, name = "Correlación", limits = c(-1, 1)) +
  scale_y_reverse(limits = c(0, min(correlation_graph_2$IPC))) +  # Invertir el eje Y, empezando en 0
  scale_x_continuous(breaks = seq(0, max(correlation_graph_2$Lag, na.rm = TRUE), by = 6)) +  # Múltiplos de 6
  theme_minimal() +
  labs(title = "Correlación entre IPC y los rezagos de la tasa de la FED",
       x = "Rezagos de la FED",
       y = "Correlación") +
  facet_wrap(~Type, scales = "fixed")  # Mantener el mismo eje en todos los gráficos

# Regresión
fed_lags <- paste("FED_lag", 1:months, sep = "_", collapse = " + ")
model <- lm(paste("IPC_core_Monthly ~", fed_lags), data = merged_df)
summary(model)

# 4 Últimos años

# Filtrar los datos según la fecha
start_date <- as.Date("2000-01-01")
filtered_df <- merged_df[merged_df$Date >= start_date, ]

# Calcular las correlaciones para los tres tipos de IPC (IPC, IPC Núcleo, IPC Expectativa)
correlations_2 <- sapply(1:months, function(lag) cor(filtered_df$IPC, filtered_df[[paste0("FED_lag_", lag)]]))
correlations_core_2 <- sapply(1:months, function(lag) cor(filtered_df$IPC_core, filtered_df[[paste0("FED_lag_", lag)]]))
correlations_exp_2 <- sapply(1:months, function(lag) cor(filtered_df$IPC_exp, filtered_df[[paste0("FED_lag_", lag)]]))

# Tabla de correlaciones
correlation_table_3 <- data.frame(
  Lag = 1:months,
  IPC_Monthly = correlations_2,
  IPC_core = correlations_core_2,
  IPC_exp = correlations_exp_2
)

correlation_table_3

# Tabla con las tres correlaciones
correlation_graph_3 <- data.frame(
  Lag = rep(1:months, 3),
  IPC = c(correlations_2, correlations_core_2, correlations_exp_2),
  Type = rep(c("IPC", "IPC Núcleo", "IPC expectativa"), each = months)
)

# Etiquetas del eje X
correlation_graph_3$Lag <- as.factor(correlation_table_3$Lag)
correlation_graph_3$Lag <- as.numeric(as.character(correlation_graph_3$Lag)) # Lag a numérico

ggplot(correlation_graph_3, aes(x = Lag, y = IPC, fill = IPC)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient2(low = "#1ac5ca", high = "#f87970", mid = "#ffffff", midpoint = 0, name = "Correlación", limits = c(-1, 1)) +
  scale_y_reverse(limits = c(0, min(correlation_graph_3$IPC))) +  # Invertir el eje Y, empezando en 0
  scale_x_continuous(breaks = seq(0, max(correlation_graph_3$Lag, na.rm = TRUE), by = 6)) +  # Múltiplos de 6
  theme_minimal() +
  labs(title = "Correlación entre IPC y los rezagos de la tasa de la FED",
       x = "Rezagos de la FED",
       y = "Correlación") +
  facet_wrap(~Type, scales = "fixed")  # Mantener el mismo eje en todos los gráficos

# 5 Modelo

# Normalizar los datos (excepto 'Date')
filtered_df_norm <- filtered_df %>%
  mutate(across(-Date, ~ as.numeric(scale(.))))

# Definir variables predictoras
fed_lags <- names(filtered_df_norm)[grepl("FED_lag_", names(filtered_df_norm))]

# Crear fórmula dinámica para el modelo
formula_rf <- as.formula(paste("IPC_core_Monthly ~", paste(fed_lags, collapse = " + ")))

# Dividir 'train' y 'test'
set.seed(54)  # Reproducibilidad
trainIndex <- createDataPartition(filtered_df_norm$IPC_core_Monthly, p = 0.8, list = FALSE)
trainData <- filtered_df_norm[trainIndex, ]
testData <- filtered_df_norm[-trainIndex, ]

# Entrenar Random Forest
model_rf <- randomForest(formula_rf, data = trainData, ntree = 500)

# Predicciones
predictions <- predict(model_rf, newdata = testData)

# Métricas: Error cuadrático medio & R^2
mse <- mean((predictions - testData$IPC_core_Monthly)^2)
rsq <- 1 - sum((predictions - testData$IPC_core_Monthly)^2) / sum((testData$IPC_core_Monthly - mean(testData$IPC_core_Monthly))^2)

print(paste("MSE: ", mse))
print(paste("R-squared: ", rsq))

# Importancia de variables
importance(model_rf)

# 6 Hiperparámetros

set.seed(54)

# Definir hiperparámetros a probar
tuneGrid <- expand.grid(mtry = seq(2, ncol(trainData) - 1, by = 2))

# Guardar errores OOB para distintos valores de mtry
mtry_values <- 1:(ncol(trainData) - 1)
oob_errors <- numeric(length(mtry_values))

for (i in mtry_values) {
  rf_model <- randomForest(IPC_core_Monthly ~ ., data = trainData, mtry = i, ntree = 500)
  oob_errors[i] <- rf_model$mse[length(rf_model$mse)]  # Último valor de error OOB
}

# Graficar OOB error vs mtry
df <- data.frame(mtry = mtry_values, oob_error = oob_errors)
ggplot(df, aes(x = mtry, y = oob_error)) +
  geom_point(size = 2) +
  geom_line() +
  ggtitle("Error OOB vs. mtry") +
  xlab("Número de variables seleccionadas en cada división (mtry)") +
  ylab("Error OOB") +
  theme_minimal()

# 6.1 Validación cruzada

# Encontrar el mejor mtry
rf_tuned <- train(IPC_core_Monthly ~ ., data = trainData, 
                  method = "rf", 
                  tuneGrid = tuneGrid,
                  trControl = trainControl(method = "cv", number = 5),  # 5-fold CV
                  ntree = 500)

# Entrenar el modelo con el mejor mtry
model_rf <- randomForest(IPC_core_Monthly ~ ., 
                         data = trainData, 
                         mtry = rf_tuned$bestTune$mtry,  # Mejor mtry
                         ntree = 500, 
                         nodesize = 5)  # Puedes ajustar `nodesize`

# Realizar predicciones
predictions <- predict(model_rf, newdata = testData)

# Calcular MSE y R-squared usando IPC_core_Monthly como variable real
mse <- mean((predictions - testData$IPC_core_Monthly)^2)
rsq <- 1 - sum((predictions - testData$IPC_core_Monthly)^2) / sum((testData$IPC_core_Monthly - mean(testData$IPC_core_Monthly))^2)

# Imprimir los resultados
print(paste("MSE: ", mse))
print(paste("R-squared: ", rsq))

# 6.2 Otros hiperparámetros

# Definir los valores de ntree y nodesize a probar
ntree_values <- c(100, 200, 500, 1000)
nodesize_values <- c(1, 5, 10, 20)

# Crear un data frame vacío para almacenar los resultados
results <- data.frame(mtry = integer(), ntree = integer(), nodesize = integer(), MSE = numeric(), Rsq = numeric())

# Loop sobre los valores de ntree y nodesize
for (ntree_value in ntree_values) {
  for (nodesize_value in nodesize_values) {
    
    # Definir el tuneGrid solo con mtry
    tuneGrid <- expand.grid(mtry = seq(2, ncol(trainData) - 1, by = 2))
    
    # Entrenar el modelo con el valor actual de ntree y nodesize
    rf_tuned <- train(IPC_core_Monthly ~ ., data = trainData, 
                      method = "rf", 
                      tuneGrid = tuneGrid,
                      trControl = trainControl(method = "cv", number = 5),  # 5-fold CV
                      ntree = ntree_value,
                      nodesize = nodesize_value)
    
    # Obtener el mejor mtry
    best_mtry <- rf_tuned$bestTune$mtry
    
    # Entrenar el modelo con los mejores valores de los tres hiperparámetros
    model_rf <- randomForest(IPC_core_Monthly ~ ., 
                             data = trainData, 
                             mtry = best_mtry, 
                             ntree = ntree_value, 
                             nodesize = nodesize_value)
    
    # Realizar predicciones
    predictions <- predict(model_rf, newdata = testData)
    
    # Calcular MSE y R-squared
    mse <- mean((predictions - testData$IPC_core_Monthly)^2)
    rsq <- 1 - sum((predictions - testData$IPC_core_Monthly)^2) / sum((testData$IPC_core_Monthly - mean(testData$IPC_core_Monthly))^2)
    
    # Guardar los resultados
    results <- rbind(results, data.frame(mtry = best_mtry, ntree = ntree_value, nodesize = nodesize_value, MSE = mse, Rsq = rsq))
  }
}

# Imprimir los resultados
print(results)

# 7 Impacto mundial

# Obtener los datos de la tasa de cambio
start_currency_date <- as.Date("2000-01-01")

# Ratio Dólar vs Euro (Euro a Dólar)
getSymbols("DEXUSEU", src = "FRED", from = start_currency_date, to = Sys.Date())
dolar_euro_df <- data.frame(
  Date = index(DEXUSEU),
  EUR_USD = 1 / as.numeric(DEXUSEU[, 1])  # Invertir la tasa de cambio
)

# Ratio Dólar vs Yuan (Chinese Yuan Renminbi to One U.S. Dollar)
getSymbols("DEXCHUS", src = "FRED", from = start_currency_date, to = Sys.Date())
yuan_df <- data.frame(
  Date = index(DEXCHUS),
  USD_CNY = as.numeric(DEXCHUS[, 1])
)

# Ratio Dólar vs Yen (Japanese Yen to One U.S. Dollar)
getSymbols("DEXJPUS", src = "FRED", from = start_currency_date, to = Sys.Date())
yen_df <- data.frame(
  Date = index(DEXJPUS),
  USD_JPY = as.numeric(DEXJPUS[, 1])
)

# Ratio Dólar vs Real (Brazilian Reals to One U.S. Dollar)
getSymbols("DEXBZUS", src = "FRED", from = start_currency_date, to = Sys.Date())
real_df <- data.frame(
  Date = index(DEXBZUS),
  USD_BRL = as.numeric(DEXBZUS[, 1])
)

get_monthly_avg <- function(df, currency_name) {
  # Columna 'Date' esté en formato Date
  df$Date <- as.Date(df$Date)
  
  # Columna con el mes y año
  df$YearMonth <- format(df$Date, "%Y-%m")
  
  # Agrupar por mes y agregar por promedio
  df_monthly_avg <- df %>%
    group_by(YearMonth) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  
  # Renombrar la columna correspondiente a la moneda
  df_monthly_avg <- df_monthly_avg %>%
    rename(!!currency_name := !!sym(currency_name), Date = YearMonth)
  
  return(df_monthly_avg)
}

# Promedio mensual
euro_df_monthly <- get_monthly_avg(dolar_euro_df, "EUR_USD")
yuan_df_monthly <- get_monthly_avg(yuan_df, "USD_CNY")
yen_df_monthly <- get_monthly_avg(yen_df, "USD_JPY")
real_df_monthly <- get_monthly_avg(real_df, "USD_BRL")

# Creación del Data Frame
currency_df <- merge(euro_df_monthly, yuan_df_monthly, by = "Date", all = TRUE)
currency_df <- merge(currency_df, yen_df_monthly, by = "Date", all = TRUE)
currency_df <- merge(currency_df, real_df_monthly, by = "Date", all = TRUE)

fed_df <- fed_df %>%
  mutate(Date = format(as.Date(Date), "%Y-%m"))

currency_df <- merge(currency_df, fed_df, by = "Date", all = TRUE)
currency_df <- na.omit(currency_df)

currency_numeric_df <- currency_df %>% select_if(is.numeric)
colnames(currency_numeric_df) <- gsub("USD|_", "", colnames(currency_numeric_df))

# Correlación
correlation_matrix <- cor(currency_numeric_df)

# Mapa de calor
correlation_melted <- melt(correlation_matrix)

ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#1ac5ca", high = "#f87970", mid = "#ffffff", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlación entre monedas y la tasa de la FED", x = NULL, y = NULL) 

# Lags de la FED
merged_df_lags <- merged_df %>%
  select(Date, starts_with("FED_Lag"))  # Si las columnas de lags empiezan con "FED_Lag"
merged_df_lags <- merged_df_lags %>%
  mutate(Date = format(as.Date(Date), "%Y-%m"))

currency_df <- merge(currency_df, merged_df_lags, by = "Date", all = TRUE)

# Modelo
model_data <- currency_df %>%
  select(USD_CNY, starts_with("FED")) %>%
  na.omit()  # Eliminar filas con NA

set.seed(54)  # Semilla para reproducibilidad
train_index <- sample(1:nrow(model_data), 0.8 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

lm_model <- lm(USD_CNY ~ ., data = train_data)
summary(lm_model)

# Evaluación del modelo
predictions <- predict(lm_model, newdata = test_data)

mse <- mean((test_data$USD_CNY - predictions)^2)
r2 <- summary(lm_model)$r.squared

cat("MSE:", mse, "\nR²:", r2, "\n")