# Cargar librerías
library(car)
library(emmeans)
library(ggplot2)
library(relaimpo)
library(corrplot)
install.packages("performance")
library(performance)
# 1. Preparación de datos
data$tipo_industria <- factor(data$tipo_industria)

# 2. Verificación de supuestos
## a) Relación lineal
ggplot(data, aes(inversion_extranjera, pib_crecimiento, color = tipo_industria)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación lineal entre IED y crecimiento del PIB",
       x = "Inversión extranjera (millones USD)",
       y = "Crecimiento del PIB (%)")

## b) Homogeneidad de pendientes
modelo_interaccion <- lm(pib_crecimiento ~ inversion_extranjera * tipo_industria, data = data)
Anova(modelo_interaccion, type = "III")

## c) Normalidad de residuos
shapiro_test <- shapiro.test(residuals(modelo_interaccion))
cat("\n--- TEST DE NORMALIDAD (Shapiro-Wilk) ---\n")
cat("Estadístico W =", round(shapiro_test$statistic, 4), 
    "| p-value =", format.pval(shapiro_test$p.value, digits = 3), "\n")
if(shapiro_test$p.value > 0.05) cat("Conclusión: Residuos normales (p > 0.05)\n") else 
  cat("Conclusión: Residuos NO normales (p ≤ 0.05)\n")

# Método 2: QQ-plot mejorado
ggqqplot(residuals(modelo_interaccion), 
         title = "QQ-Plot de Residuos", 
         ylab = "Residuos", 
         ggtheme = theme_minimal())

# Método 3: Test visual con histograma
ggplot(data.frame(Residuos = residuals(modelo_interaccion)), aes(Residuos)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "skyblue", color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  stat_function(fun = dnorm, args = list(mean = mean(residuals(modelo_interaccion)), 
                                         sd = sd(residuals(modelo_interaccion))), 
                color = "blue", size = 1) +
  labs(title = "Distribución de Residuos", 
       subtitle = "Línea azul = distribución normal teórica") +
  theme_minimal()
## d) Homocedasticidad
levene_test <- leveneTest(pib_crecimiento ~ tipo_industria, data = data, center = mean)
cat("\n--- TEST DE HOMOCEDASTICIDAD (Levene) ---\n")
print(levene_test)
if(levene_test$`Pr(>F)`[1] > 0.05) cat("\nConclusión: Varianzas homogéneas (p > 0.05)\n") else 
  cat("\nConclusión: Varianzas NO homogéneas (p ≤ 0.05)\n")

# Grafico de residuos vs valores ajustados
plot(modelo_interaccion, which = 1, pch = 16, col = "blue",
     main = "Residuos vs Valores Ajustados",
     sub = "Línea roja debería ser plana")

## e) Multicolinealidad para que :V --------------------------------
cat("\n\n=== DIAGNÓSTICO DE MULTICOLINEALIDAD ===\n")

# Corrección: Convertir a vector numérico
data$inv_centrada <- as.numeric(scale(data$inversion_extranjera, center = TRUE, scale = FALSE))

# Calcular VIF
modelo_ancova_centrado <- lm(pib_crecimiento ~ inv_centrada + tipo_industria, data = data)
vif_results <- vif(modelo_ancova_centrado)
print("Factor de Inflación de Varianza (VIF):")
print(vif_results)

# Matriz de correlación (solo para variables numéricas)
if(sum(sapply(data, is.numeric)) > 0) {
  numericas <- data[, sapply(data, is.numeric), drop = FALSE]
  cor_matrix <- cor(numericas, use = "complete.obs")
  
  # Visualizacion
  corrplot(cor_matrix, method = "number", type = "upper", 
           tl.col = "black", tl.srt = 45,
           title = "Matriz de Correlación entre Variables Numéricas",
           mar = c(0, 0, 2, 0))
}

# Analisis por grupo
ggplot(data, aes(x = tipo_industria, y = inversion_extranjera, fill = tipo_industria)) +
  geom_boxplot() +
  labs(title = "Distribución de Inversión Extranjera por Sector",
       x = "Tipo de Industria",
       y = "Inversión Extranjera (millones USD)") +
  theme_minimal()

# 3. ANCOVA uffff
summary(modelo_ancova_centrado)
Anova(modelo_ancova_centrado, type = "III")

# 4. Medias ajustadas y comparaciones
medias_ajustadas <- emmeans(modelo_ancova_centrado, pairwise ~ tipo_industria, adjust = "tukey")
print(medias_ajustadas)

# 5. resuldos en graficas dof¿g
ggplot(as.data.frame(medias_ajustadas$emmeans), 
       aes(tipo_industria, emmean, fill = tipo_industria)) +
  geom_col(width = 0.7, alpha = 0.8) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1) +
  labs(title = "Crecimiento económico por sector industrial",
       subtitle = "Medias ajustadas controlando por inversión extranjera",
       x = "Sector industrial",
       y = "Crecimiento del PIB (%) ajustado") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Graficos demas 
if(requireNamespace("ggeffects", quietly = TRUE)) {
  library(ggeffects)
  
  # Correcciones 
  plot_data <- ggpredict(modelo_ancova_centrado, 
                         terms = c("inv_centrada", "tipo_industria"))
  
  print(plot(plot_data) +
          labs(title = "Efectos marginales de la inversión por sector",
               x = "Inversión extranjera (centrada)",
               y = "Crecimiento del PIB (%)",
               color = "Tipo de Industria") +
          theme_minimal())
}

# Gráfico de comparaciones post-hoc ufff
plot(medias_ajustadas, comparisons = TRUE) +
  geom_point(size = 3) +
  labs(title = "Comparación entre sectores industriales")

# 6. Analisis de importancia relativa
cat("\n\n=== IMPORTANCIA RELATIVA DE PREDICTORES ===\n")
imp <- calc.relimp(modelo_ancova_centrado, type = "lmg")
print(imp)

# Grafico de importancia
if(!is.null(imp$lmg)) {
  barplot(sort(imp$lmg, decreasing = TRUE), 
          names.arg = names(sort(imp$lmg, decreasing = TRUE)),
          col = "skyblue",
          main = "Importancia Relativa de Predictores",
          ylab = "Porcentaje de Varianza Explicada",
          ylim = c(0, 1))
}

