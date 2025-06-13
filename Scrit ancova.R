# Cargar librerías
library(car)
library(emmeans)
library(ggplot2)
library(ggpubr) 
library(sandwich)
library(lmtest)
library(ggeffects)
library(performance)
library(effects)

# 1. Preparacion de datos
data$tipo_industria <- factor(data$tipo_industria)

# 2. ancova con interaccion ----
modelo_interaccion <- lm(pib_crecimiento ~ inversion_extranjera * tipo_industria, data = data)

# 3. Verificacion de supuestos ancova novita (interacion) ----
## a) Relación lineal 
cat("\n\n=== Linealidad dog, ver imagen  ===\n")

## b) Normalidad de residuos
shapiro_test <- shapiro.test(residuals(modelo_interaccion))
cat("\n--- Salio normalito sengun Shapiro-Wilk? ---\n")
cat("p-value =", format.pval(shapiro_test$p.value, digits = 3), "\n")
if(shapiro_test$p.value > 0.05) cat("\nSimon: Residuos normales (p > 0.05)\n") else 
  cat("Conclusion: Residuos nO normales (p ≤ 0.05)\n")

# QQ-plot
ggqqplot(residuals(modelo_interaccion), 
         title = "QQ-Plot de Residuos", 
         ylab = "Residuos", 
         ggtheme = theme_minimal())

# test visual con histograma
ggplot(data.frame(Residuos = residuals(modelo_interaccion)), aes(Residuos)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "skyblue", color = "black") +
  geom_density(alpha = 0.2, fill = "red") +
  stat_function(fun = dnorm, args = list(mean = mean(residuals(modelo_interaccion)), 
                                         sd = sd(residuals(modelo_interaccion))), 
                color = "blue", size = 1) +
  labs(title = "Distribución de Residuos", 
       subtitle = "Línea azul = distribución normal teórica") +
  theme_minimal()


## c) Homocedasticidad (Levene)
cat("\n--- test #nelson HOMOCEDASTICIDAD (Levene) ---\n")
levene_test <- leveneTest(residuals(modelo_interaccion) ~ tipo_industria, data = data)
print(levene_test)
if(levene_test$`Pr(>F)`[1] > 0.05) cat("\nConclusión: Varianzas homogéneas (p > 0.05)\n") else 
  cat("\nConclusion de nelson: Varianzas NO homogeneas (p ≤ 0.05)\n")

# Gráfico de residuos vs valores ajustados
plot(modelo_interaccion, which = 1, pch = 16, col = "blue",
     main = "Residuos vs Valores Ajustados. Como lo interpretan?")

# d) Prueba de homogeneidad de pendientes con linearHypothesis()
cat("\n--- Homogeneidad de pendientes con linearHypothesis() ---\n")
test_inter <- linearHypothesis(
  modelo_interaccion,
  c("inversion_extranjera:tipo_industriaManufactura = 0",
    "inversion_extranjera:tipo_industriaTecnologia = 0"),
  white.adjust = "hc3"  # usa HC3 si quieres robustez a heterocedasticidad
)
print(test_inter)

pval <- test_inter$`Pr(>F)`[2]

if (pval > 0.05) {
  cat("\nConclusión: homogeneidad de pendientes (p =", format.pval(pval), "> 0.05)\n")
} else {
  cat("\nConclusión: NO homogeneidad de pendientes (p =", format.pval(pval), "≤ 0.05)\n")
}

# 4. INFERENCIA ROBUSTA PARA EL MODELO DE INTERACCIÓN ----
cat("\n\n=== ancovita con interacion ;V ===\n")

#Usamos errores robustos
usar_robustos <- levene_test$`Pr(>F)`[1] < 0.05

if(usar_robustos) {
  cat("\n--- Usando errores HC3 para heterocedasticidad ---\n")
  # Coeficientes con errores robustos
  robust_test <- coeftest(modelo_interaccion, vcov = vcovHC(modelo_interaccion, type = "HC3"))
  print(robust_test)
  
  # ancova robusta
  cat("\n--- Anova con errores tipo Wald Test ---\n")
  robust_anova <- waldtest(modelo_interaccion, vcov = vcovHC(modelo_interaccion, type = "HC3"))
  print(robust_anova)
} else {
  cat("\n--- Ancova con interecciones ---\n")
  print(summary(modelo_interaccion))
  cat("\n--- Ancova tradicional ;C ---\n")
  print(Anova(modelo_interaccion, type = "III"))
}

# 5. grafico y comparacion de interaciones ----
cat("\n\n=== Anisis de interaccion, entienden? print(nelson) ===\n")

## a) grafico de interaccion 
interaction_plot <- ggpredict(modelo_interaccion, 
                              terms = c("inversion_extranjera", "tipo_industria"))

print(plot(interaction_plot) +
        labs(title = "Interaccion entre Inversion Extranjera y Tipo de Industria",
             subtitle = "Efecto en el Crecimiento del PIB",
             x = "Inversion Extranjera (millones USD)",
             y = "Crecimiento del PIB (%)",
             color = "Tipo de Industria") +
        theme_minimal())

## b) Comparacion de pendientes durisima
if(usar_robustos) {
  cat("\n--- HC3 en cada pendiente ---\n")
  pendientes <- emtrends(modelo_interaccion, 
                         specs = pairwise ~ tipo_industria, 
                         var = "inversion_extranjera",
                         vcov. = vcovHC(modelo_interaccion, type = "HC3"))
} else {
  cat("\n--- pendientes con errores estanderes ---\n")
  pendientes <- emtrends(modelo_interaccion, 
                         specs = pairwise ~ tipo_industria, 
                         var = "inversion_extranjera")
}

cat("\n--- medias ajustadas ---\n")
print(pendientes$emtrends)

cat("\n--- Comparaciones por pares con corrección Tukey ---\n")
print(pendientes$contrasts)

## c) Grafico de pendientes
pendientes_df <- as.data.frame(pendientes$emtrends)


# cambios de nombres
names(pendientes_df)[names(pendientes_df) == "inversion_extranjera.trend"] <- "pendiente"

print(
  ggplot(pendientes_df, aes(x = tipo_industria, y = pendiente)) +
    geom_point(size = 4, color = "blue") +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1, color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Pendientes de Inversión por Tipo de Industria",
         subtitle = "Efecto de 1 millón USD en crecimiento del PIB",
         x = "Tipo de Industria",
         y = "Pendiente (Cambio en % del PIB)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)


## d) Efectos simples
cat("\n--- efecto simple por industria ---\n")
puntos_inversion <- c(
  min(data$inversion_extranjera),
  mean(data$inversion_extranjera),
  max(data$inversion_extranjera)
)

simple_effects <- emmeans(modelo_interaccion, 
                          specs = ~ inversion_extranjera | tipo_industria,
                          at = list(inversion_extranjera = puntos_inversion))

print(simple_effects)



# 6. importancia de interacion ----
cat("\n\n=== Fue importante usar interacciones? ;´v ===\n")

# contriducion unica de interaccion
modelo_sin_interaccion <- lm(pib_crecimiento ~ inversion_extranjera + tipo_industria, data = data)
anova_comparacion <- anova(modelo_sin_interaccion, modelo_interaccion)

cat("\n--- Comparemos ---\n")
print(anova_comparacion)

# R-cuadrado incremental
r2_full <- summary(modelo_interaccion)$r.squared
r2_reduced <- summary(modelo_sin_interaccion)$r.squared
incremental_r2 <- r2_full - r2_reduced

cat("\nR² ancovita con interacciones:", round(r2_full, 4))
cat("\nR² sin ;C:", round(r2_reduced, 4))
cat("\nR² incremental por interacción:", round(incremental_r2, 4))
cat("\nPorcentaje de varianza explicada por interacción:", round(incremental_r2*100, 2), "%\n")

# 7. grafico en 3d nelson ----
if(requireNamespace("plotly", quietly = TRUE)) {
  library(plotly)
  
  grid <- expand.grid(
    inversion_extranjera = seq(min(data$inversion_extranjera), 
                               max(data$inversion_extranjera), 
                               length.out = 30),
    tipo_industria = levels(data$tipo_industria)
  )
  grid$pred <- predict(modelo_interaccion, newdata = grid)
  
  plot_3d <- plot_ly(grid, x = ~inversion_extranjera, y = ~tipo_industria, z = ~pred,
                     type = "scatter3d", mode = "lines",
                     color = ~tipo_industria, colors = "Set1") %>%
    layout(scene = list(
      xaxis = list(title = "Inversión Extranjera"),
      yaxis = list(title = "Tipo de Industria"),
      zaxis = list(title = "Crecimiento PIB Predicho")
    ), title = "Superficie de Interacción 3D")
  
  print(plot_3d)
} else {
  cat("\nInstala el paquete 'plotly' para visualización 3D: install.packages('plotly')\n")
}



