# ============================
# VP de flujo MENSUAL con tasa ANUAL
# (curva + punto EMBI+US10Y + rango Costo de Equity: conservador/razonable/prudente)
# ============================

library(ggplot2)

# ----------------------------
# Parámetros del problema
# ----------------------------
cf_mensual_usd <- 3e6      # 3 millones USD por mes
horizonte_anios <- 10      # <-- Cambiá esto si querés otro horizonte
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
  tasa_anual = tasa_objetivo_anual,
  vp_musd = vp_obj_musd
)

# ----------------------------
# Puntos: costo de equity (3 escenarios)
# ----------------------------
df_eq <- data.frame(
  escenario = factor(
    c("Conservador", "Razonable", "Prudente"),
    levels = c("Conservador", "Razonable", "Prudente")
  ),
  tasa_anual = c(tasa_eq_conservador, tasa_eq_razonable, tasa_eq_prudente),
  vp_musd = c(
    vp_musd_desde_tasa(tasa_eq_conservador),
    vp_musd_desde_tasa(tasa_eq_razonable),
    vp_musd_desde_tasa(tasa_eq_prudente)
  )
)

# Etiquetas amigables (sin depender de colores)
df_eq$label <- sprintf(
  "%s: %.1f%%\nVP ≈ %.1f MUSD",
  as.character(df_eq$escenario),
  df_eq$tasa_anual * 100,
  df_eq$vp_musd
)

# ----------------------------
# Plot
# ----------------------------
ggplot(df, aes(x = tasa_anual * 100, y = vp_musd)) +
  # Rango de costo de equity (15% a 18%)
  annotate(
    "rect",
    xmin = rango_eq_min * 100,
    xmax = rango_eq_max * 100,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.12
  ) +
  geom_line(linewidth = 1) +

  # Punto soberano (US10y + EMBI)
  geom_point(data = df_punto, aes(x = tasa_anual * 100, y = vp_musd), size = 3) +
  geom_vline(xintercept = tasa_objetivo_anual * 100, linetype = "dashed") +
  annotate(
    "text",
    x = tasa_objetivo_anual * 100,
    y = vp_obj_musd,
    label = sprintf("US10y 4,1%% + EMBI 5,2%% = 9,3%%\nVP ≈ %.1f MUSD", vp_obj_musd),
    vjust = -1,
    hjust = 0.5
  ) +

  # Líneas verticales para los 3 escenarios (costo de equity)
  geom_vline(
    data = df_eq,
    aes(xintercept = tasa_anual * 100),
    linetype = "dotdash",
    linewidth = 0.7
  ) +
  # Puntos para los 3 escenarios
  geom_point(
    data = df_eq,
    aes(x = tasa_anual * 100, y = vp_musd, shape = escenario),
    size = 3
  ) +
  # Etiquetas para los 3 escenarios (evita solapamiento básico con vjust)
  geom_text(
    data = df_eq,
    aes(x = tasa_anual * 100, y = vp_musd, label = label),
    vjust = -1,
    hjust = 0.5,
    size = 3.4
  ) +
  # Etiqueta del rango sombreado (sin depender de color)
  annotate(
    "text",
    x = ((rango_eq_min + rango_eq_max) / 2) * 100,
    y = max(df$vp_musd) * 0.92,
    label = "Rango costo de equity (USD) objetivo: 15% – 18%",
    vjust = 0,
    hjust = 0.5,
    size = 4
  ) +

  labs(
    x = "Tasa de descuento anual (%)",
    y = "Valor Presente (millones de USD)",
    title = sprintf(
      "Valor Presente de flujo mensual constante (%.1f MUSD/mes) — Horizonte: %d años",
      cf_mensual_usd / 1e6, horizonte_anios
    )
  ) +
  theme_minimal() +
  guides(shape = guide_legend(title = "Escenario (Costo de Equity)"))