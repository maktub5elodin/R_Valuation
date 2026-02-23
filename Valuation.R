# ============================
# VP de flujo MENSUAL con tasa ANUAL (curva + punto EMBI)
# ============================

# Instalación (si hace falta)
# install.packages("ggplot2")
library(ggplot2)

# ----------------------------
# Parámetros del problema
# ----------------------------
cf_mensual_usd <- 3e6      # 3 millones USD por mes
horizonte_anios <- 10      # <-- Cambiá esto si querés otro horizonte
n_meses <- horizonte_anios * 12

# Tasa objetivo (US10y + EMBI)
us10y <- 0.041
embi_spread <- 0.052
tasa_objetivo_anual <- us10y + embi_spread  # 0.093 (9,3%)

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
# Punto a marcar: 9,3% anual
# ----------------------------
tasa_obj_mensual <- (1 + tasa_objetivo_anual)^(1/12) - 1  # mismo ajuste anual->mensual
vp_obj_usd <- if (tasa_obj_mensual == 0) {
  cf_mensual_usd * n_meses
} else {
  cf_mensual_usd * (1 - (1 + tasa_obj_mensual)^(-n_meses)) / tasa_obj_mensual
}
vp_obj_musd <- vp_obj_usd / 1e6

df_punto <- data.frame(
  tasa_anual = tasa_objetivo_anual,
  vp_musd = vp_obj_musd
)

# ----------------------------
# Plot
# ----------------------------
ggplot(df, aes(x = tasa_anual * 100, y = vp_musd)) +
  geom_line(linewidth = 1) +
  geom_point(data = df_punto, aes(x = tasa_anual * 100, y = vp_musd), size = 3) +
  geom_vline(xintercept = tasa_objetivo_anual * 100, linetype = "dashed") +
  annotate(
    "text",
    x = tasa_objetivo_anual * 100,
    y = vp_obj_musd,
    label = sprintf("EMBI+520 + US10y 4,1%% = 9,3%%\nVP ≈ %.1f MUSD", vp_obj_musd),
    vjust = -1,
    hjust = 0.5
  ) +
  labs(
    x = "Tasa de descuento anual (%)",
    y = "Valor Presente (millones de USD)",
    title = sprintf("Valor Presente de flujo mensual constante (%.1f MUSD/mes) - Horizonte: %d años",
                    cf_mensual_usd/1e6, horizonte_anios)
  ) +
  theme_minimal()
