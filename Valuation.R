# ============================
# VP de flujo MENSUAL con tasa ANUAL
# (curva + punto EMBI+US10Y + rango Costo de Equity: conservador/razonable/prudente)
# ============================

library(ggplot2)

# Si no lo tenés:
# install.packages("ggrepel")
library(ggrepel)

# ----------------------------
# Parámetros del problema
# ----------------------------
cf_mensual_usd <- 3e6      # 3 millones USD por mes
horizonte_anios <- 15      # <-- Cambiá esto si querés otro horizonte
n_meses <- horizonte_anios * 12

# ----------------------------
# Base macro (punto soberano: US10y + EMBI)
# ----------------------------
us10y <- 0.041
embi_spread <- 0.052
tasa_objetivo_anual <- us10y + embi_spread  # 0.093 (9,3%)

# ----------------------------
# Rango costo de equity (USD) a resaltar
# ----------------------------
tasa_eq_conservador <- 0.148   # ~14,8% (CAPM adj simple; beta=1; CRP=EMBI)
tasa_eq_razonable   <- 0.169   # ~16,9% (beta=1,2; CRP ajustado ~6,2%)
tasa_eq_prudente    <- 0.181   # ~18,1% (beta=1,3; CRP ajustado ~6,8%)

rango_eq_min <- 0.15
rango_eq_max <- 0.18

# ----------------------------
# Grilla de tasas ANUALES para graficar
# ----------------------------
tasas_anuales <- seq(0.00, 0.30, by = 0.001)   # 0% a 30% anual

# ----------------------------
# >>> AJUSTE CLAVE: tasa ANUAL -> tasa MENSUAL <<<
# ----------------------------
# IMPORTANTE:
# El flujo es mensual, pero la tasa es anual.
# Para descontar correctamente mes a mes, convertimos cada tasa anual (efectiva)
# a su equivalente mensual:
#   r_mensual = (1 + r_anual)^(1/12) - 1
#
# Esta línea es EXACTAMENTE donde está incorporado el ajuste:
tasas_mensuales <- (1 + tasas_anuales)^(1/12) - 1

# ----------------------------
# Cálculo de Valor Presente (VP) para una anualidad mensual
# VP = CF * [1 - (1+r_m)^(-n)] / r_m
# (tratamos el caso r_m = 0 para evitar división por cero)
# ----------------------------
vp_usd <- ifelse(
  tasas_mensuales == 0,
  cf_mensual_usd * n_meses,
  cf_mensual_usd * (1 - (1 + tasas_mensuales)^(-n_meses)) / tasas_mensuales
)

df <- data.frame(
  tasa_anual = tasas_anuales,
  vp_musd = vp_usd / 1e6
)

# ----------------------------
# Función auxiliar para VP dado r_anual
# ----------------------------
vp_musd_desde_tasa <- function(r_anual) {
  r_m <- (1 + r_anual)^(1/12) - 1
  vp <- if (r_m == 0) cf_mensual_usd * n_meses else cf_mensual_usd * (1 - (1 + r_m)^(-n_meses)) / r_m
  vp / 1e6
}

# ----------------------------
# Punto soberano (EMBI + US10y)
# ----------------------------
vp_obj_musd <- vp_musd_desde_tasa(tasa_objetivo_anual)

df_punto <- data.frame(
  tipo = "Soberano (US10Y + EMBI)",
  tasa_anual = tasa_objetivo_anual,
  vp_musd = vp_obj_musd,
  label = sprintf("Soberano: %.1f%%\nVP ≈ %.1f MUSD",
                  tasa_objetivo_anual * 100, vp_obj_musd)
)

# Escenarios equity
df_eq <- data.frame(
  escenario = c("Conservador", "Razonable", "Prudente"),
  tasa_anual = c(tasa_eq_conservador, tasa_eq_razonable, tasa_eq_prudente)
)
df_eq$vp_musd <- sapply(df_eq$tasa_anual, vp_musd_desde_tasa)
df_eq$label <- sprintf("%s: %.1f%%\nVP ≈ %.1f MUSD",
                       df_eq$escenario, df_eq$tasa_anual * 100, df_eq$vp_musd)

# ----------------------------
# Estética: paleta simple y consistente
# ----------------------------
colores_escenarios <- c(
  "Conservador" = "#1B9E77",
  "Razonable"   = "#D95F02",
  "Prudente"    = "#7570B3"
)

color_curva <- "#2B2B2B"
color_soberano <- "#E7298A"

# ----------------------------
# Plot
# ----------------------------
ggplot(df, aes(x = tasa_anual * 100, y = vp_musd)) +
  # Rango de costo de equity (15% a 18%)
  annotate(
    "rect",
    xmin = rango_eq_min * 100, xmax = rango_eq_max * 100,
    ymin = -Inf, ymax = Inf,
    alpha = 0.10, fill = "#4C78A8"
  ) +

  # Curva
  geom_line(linewidth = 1.05, color = color_curva) +

  # Línea vertical soberano
  geom_vline(xintercept = tasa_objetivo_anual * 100, linetype = "dashed",
             linewidth = 0.7, color = color_soberano, alpha = 0.9) +

  # Punto soberano
  geom_point(
    data = df_punto,
    aes(x = tasa_anual * 100, y = vp_musd),
    size = 3.2, color = color_soberano
  ) +

  # Etiqueta soberano (repel)
  ggrepel::geom_label_repel(
    data = df_punto,
    aes(x = tasa_anual * 100, y = vp_musd, label = label),
    size = 3.4,
    min.segment.length = 0,
    box.padding = 0.45,
    point.padding = 0.25,
    label.size = 0.2,
    segment.alpha = 0.6,
    seed = 123
  ) +

  # Líneas verticales para los 3 escenarios (costo de equity)
  geom_vline(
    data = df_eq,
    aes(xintercept = tasa_anual * 100, color = escenario),
    linetype = "dotdash",
    linewidth = 0.7,
    alpha = 0.9
  ) +
  # Puntos para los 3 escenarios
  geom_point(
    data = df_eq,
    aes(x = tasa_anual * 100, y = vp_musd, color = escenario),
    size = 3.0
  ) +
  # Etiquetas para los 3 escenarios (evita solapamiento básico con repel)
  ggrepel::geom_label_repel(
    data = df_eq,
    aes(x = tasa_anual * 100, y = vp_musd, label = label, color = escenario),
    size = 3.2,
    min.segment.length = 0,
    box.padding = 0.45,
    point.padding = 0.25,
    label.size = 0.2,
    segment.alpha = 0.6,
    seed = 456,
    show.legend = FALSE
  ) +

  # Texto discreto de la banda
  annotate(
    "label",
    x = ((rango_eq_min + rango_eq_max) / 2) * 100,
    y = max(df$vp_musd) * 0.93,
    label = "Rango costo de equity (USD): 15% – 18%",
    size = 3.6,
    label.size = 0.2,
    alpha = 0.95
  ) +

  scale_color_manual(values = colores_escenarios) +

  labs(
    x = "Tasa de descuento anual (%)",
    y = "Valor Presente (millones de USD)",
    title = sprintf("Valor Presente de flujo mensual constante (%.1f MUSD/mes) — Horizonte: %d años",
                    cf_mensual_usd / 1e6, horizonte_anios),
    subtitle = "Curva VP vs tasa anual + punto soberano (US10Y+EMBI) + escenarios de costo de equity"
  ) +

  # ✅ Ejes visibles y grilla suave
  theme_classic(base_size = 12) +
  theme(
    axis.line = element_line(linewidth = 0.8, color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )