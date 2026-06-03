
# ==============================================================================
# 1. INSTALACIÓN Y CARGA DEL PAQUETE
# ==============================================================================
# Si no tienes instalado el paquete, descomenta la siguiente línea:
install.packages("agricolae")
library(agricolae)

# ==============================================================================
# 2. DEFINICIÓN DE TRATAMIENTOS (Arreglo Factorial 2 x 4 = 8 tratamientos)
# ==============================================================================
# Factor A: 2 niveles
factor_A <- c("Bambu", "Descartable")

# Factor B: 4 niveles
factor_B <- c("Cana", "Platano_Maduro", "Afrecho_Masato", "Rizoma_Platano")

# Crear vector con los 8 tratamientos combinados
tratamientos <- c(
  "Bambu_Cana", "Bambu_Platano_Maduro", "Bambu_Afrecho_Masato", "Bambu_Rizoma_Platano",
  "Descartable_Cana", "Descartable_Platano_Maduro", "Descartable_Afrecho_Masato", "Descartable_Rizoma_Platano"
)

# ==============================================================================
# 3. GENERACIÓN DEL DISEÑO EXPERIMENTAL (DBCA)
# ==============================================================================
# r = 4 repeticiones (bloques)
# serie = 2 (etiquetas de dos dígitos para las parcelas, ej: 01, 02)
# seed = semilla para reproducibilidad de la aleatorización
set.seed(2026) 
diseno <- design.rcbd(trt = tratamientos, r = 4, serie = 2, seed = 2026)

# Ver el libro de campo (diseño aleatorizado)
print("=== LIBRO DE CAMPO (DISEÑO ALEATORIZADO) ===")
print(diseno$book)
View(diseno$book)
# ==============================================================================
# 4. SIMULACIÓN DE DATOS (REEMPLAZAR CON TUS DATOS REALES)
# ==============================================================================
# Simulamos una variable respuesta (ej. número de insectos capturados)
# NOTA: Cuando tengas tus datos reales, cárgalos con read.csv() y omite este paso.
set.seed(456)
datos_simulados <- data.frame(
  Bloque = factor(diseno$book$blocks),
  Tratamiento = factor(diseno$book$trt),
  Capturas = rpois(32, lambda = 12) # Datos simulados de conteo (Poisson)
)

# Unir el diseño con los datos
experimento <- merge(diseno$book, datos_simulados, by.x = c("blocks", "trt"), by.y = c("Bloque", "Tratamiento"))

# Separar el tratamiento en sus dos factores para el ANOVA factorial
experimento$Factor_A <- sapply(strsplit(as.character(experimento$trt), "_"), `[`, 1)
experimento$Factor_B <- sapply(strsplit(as.character(experimento$trt), "_"), function(x) paste(x[-1], collapse = "_"))

# Convertir a factores
experimento$Factor_A <- as.factor(experimento$Factor_A)
experimento$Factor_B <- as.factor(experimento$Factor_B)
experimento$blocks <- as.factor(experimento$blocks)

print("=== ESTRUCTURA DE LOS DATOS (Primeras 6 filas) ===")
head(experimento)

# ==============================================================================
# 5. ANÁLISIS DE VARIANZA (ANOVA) FACTORIAL
# ==============================================================================
# Modelo: Respuesta ~ Bloques + Factor A + Factor B + Interacción A:B
modelo <- aov(Capturas ~ blocks + Factor_A * Factor_B, data = experimento)

print("=== TABLA DE ANOVA ===")
summary(modelo)

# ==============================================================================
# 6. PRUEBAS DE COMPARACIÓN MÚLTIPLE (Tukey / HSD)
# ==============================================================================
# Si el Factor A es significativo, comparamos sus medias
print("=== PRUEBA DE TUKEY PARA FACTOR A (Tipo de Trampa) ===")
prueba_A <- HSD.test(modelo, "Factor_A", group = TRUE, console = TRUE)

# Si el Factor B es significativo, comparamos sus medias
print("=== PRUEBA DE TUKEY PARA FACTOR B (Atrayente) ===")
prueba_B <- HSD.test(modelo, "Factor_B", group = TRUE, console = TRUE)

# Si la interacción (A:B) es significativa, es recomendable evaluar las medias de la interacción
print("=== PRUEBA DE TUKEY PARA LA INTERACCIÓN (A x B) ===")
prueba_AB <- HSD.test(modelo, c("Factor_A", "Factor_B"), group = TRUE, console = TRUE)

# ==============================================================================
# 7. VALIDACIÓN DE SUPUESTOS (Opcional pero recomendado)
# ==============================================================================
# Gráficos de residuos para verificar normalidad y homocedasticidad
par(mfrow = c(2, 2))
plot(modelo, main = "Diagnóstico de Residuos del Modelo")
par(mfrow = c(1, 1)) # Restaurar configuración gráfica