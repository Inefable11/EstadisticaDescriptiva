library(dplyr)  # Para manipulación de datos
library(readr)  # Para lectura de datos desde archivos CSV

ruta_archivo <- 'A:/MAESTRÍA EN BIG DATA Y DATA ANALYTICS/Análisis e interpretación de datos/ACTIVIDAD 1/archive/countries of the world.csv'

datos <- read_csv(ruta_archivo)
print(datos)


especificacion_columnas <- spec(read_csv(ruta_archivo))
print(especificacion_columnas)

# Paso 1: Ordenar los datos por PIB per cápita en orden ascendente
datos_ordenados <- datos[order(datos$`GDP ($ per capita)`), ]

# Paso 2: Seleccionar las primeras tres filas
datos_tres_paises <- head(datos_ordenados, 4)

##diez filas
datos_diez_paises <- head(datos_ordenados, 10)

datos_tres_paises$`Literacy (%)` <- as.numeric(gsub(",", ".", gsub("%", "", datos_tres_paises$`Literacy (%)`)))


datos_tres_paises$`Pop. Density (per sq. mi.)` <- as.numeric(gsub(",", ".", datos_tres_paises$`Pop. Density (per sq. mi.)`))


# Convertir 'Service' a numérico si es posible
datos_tres_paises$Service <- as.numeric(gsub(",", ".", gsub("%", "", datos_tres_paises$Service)))


# Convertir 'GDP ($ per capita)' a numérico
datos_tres_paises$`GDP ($ per capita)` <- as.numeric(gsub(",", "", datos_tres_paises$`GDP ($ per capita)`))

# Calcular la matriz de correlación de Pearson
matriz_cor <- cor(datos_tres_paises[c("GDP ($ per capita)", "Literacy (%)", "Pop. Density (per sq. mi.)", "Service")], method = "pearson", use = "complete.obs")

# Mostrar la matriz de correlación con nombres de columnas
colnames(matriz_cor) <- c("GDP", "Literacy", "Population density", "Service")
print(matriz_cor)

print(datos_tres_paises)

#mediana
mediana_per_capita <- median(datos_tres_paises$`GDP ($ per capita)`)
print(paste("Mediana del PIB per cápita:", mediana_per_capita))

#Resumen descriptivos
summary_por_grupo <- by(datos_tres_paises[, c("GDP ($ per capita)", "Literacy (%)", "Pop. Density (per sq. mi.)", "Service")],
                        datos_tres_paises$Country, summary)
print(summary_por_grupo)

columnas_interes <- c("GDP ($ per capita)", "Literacy (%)", "Pop. Density (per sq. mi.)", "Service")

# Reemplazar comas por puntos en la columna "Pop. Density (per sq. mi.)"
datos_tres_paises$`Pop. Density (per sq. mi.)` <- gsub(",", ".", datos_tres_paises$`Pop. Density (per sq. mi.)`)
# Convertir las columnas a tipos numéricos
datos_tres_paises[, columnas_interes] <- lapply(datos_tres_paises[, columnas_interes], as.numeric)

# Verificar la conversión
summary(datos_tres_paises[, columnas_interes])

#cajas
boxplot(`GDP ($ per capita)` ~ Country, data = datos_tres_paises, col = "lightblue", main = "Comparación de PIB per cápita por País")

#barras
barplot(height = datos_tres_paises$`GDP ($ per capita)`, names.arg = datos_tres_paises$Country,
        col = "lightblue", main = "Comparación de PIB per cápita por País",
        xlab = "País", ylab = "PIB per cápita")


#barras 10 paises mas bajos

barplot(height = datos_diez_paises$`GDP ($ per capita)`, names.arg = datos_diez_paises$Country,
        col = "lightblue", main = "Comparación de PIB per cápita por País",
        xlab = "País", ylab = "PIB per cápita")



barplot(height = datos_diez_paises$`GDP ($ per capita)`,
        names.arg = datos_diez_paises$Country,
        col = "lightblue", main = "Comparación de PIB per cápita por País",
        xlab = "País", ylab = "PIB per cápita", las = 2)  # Las = 2 para orientación vertical