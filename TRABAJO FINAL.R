# ANÁLISIS COMPARATIVO CIS 3501 Y 3555
# Data Science - Daniela Escudero y Amanda robles
# Estudios: E-3501 "Relaciones sexuales y de pareja" (Enero 2025)
#           E-3555 "Sexualidad: hábitos y opiniones" (Marzo 2026)
#
# Enfoque: Análisis de consistencia conceptual por subgrupos
# Constructos: (A) Importancia de la relación sentimental/pareja
#              (B) Actitudes hacia la sexualidad y las relaciones
#              (C) Orientación sexual como variable de subgrupo


## 0. PAQUETES
library(haven)       
library(tidyverse)   
library(psych)       
library(vcd)        


## 1. CARGA DE DATOS
d3501 <- read_sav("/Users/daniescu/Downloads/MD3501/3501.sav")
d3555 <- read_sav("/Users/daniescu/Downloads/MD3555/3555.sav")

cat("Filas E-3501:", nrow(d3501), "| Variables:", ncol(d3501), "\n")
cat("Filas E-3555:", nrow(d3555), "| Variables:", ncol(d3555), "\n")


# 2. SELECCIÓN Y RENOMBRADO DE VARIABLES

# --- Estudio 3501 ---
e3501 <- d3501 |>
  select(
    sexo       = SEXO,
    edad       = EDAD,
    estudios   = ESTUDIOS,
    religion   = RELIGION,
    ideologia  = ESCIDEOL,
    peso       = PESO,
    # Constructo A: importancia tener pareja (1=Muy imp. … 4=Nada imp.)
    imp_pareja = P1,
    # Constructo B: actitudes hacia la sexualidad — 5 ítems P6 (1=Muy acuerdo … 5=Nada)
    prej_sexo  = P6_1,   # "Sigue habiendo muchos prejuicios en relación al sexo"
    consen     = P6_2,   # "Vale todo si hay acuerdo"
    deseo_gen  = P6_3,   # "Hombres tienen más deseos que mujeres"
    penet      = P6_4,   # "El sexo de verdad incluye penetración"
    fluidez    = P6_5,   # "Una persona puede variar su orientación sexual"
    # Satisfacción sexual
    satisf     = P11,
    # Orientación sexual
    orientac   = P12R,
    # Situación de pareja (para regresión logística)
    pareja_sit = PAREJA
  ) |>
  mutate(estudio = "E-3501 (Ene 2025)")

# --- Estudio 3555 ---
e3555 <- d3555 |>
  select(
    sexo       = SEXO,
    edad       = EDAD,
    estudios   = ESTUDIOS,
    religion   = RELIGION,
    ideologia  = ESCIDEOL,
    peso       = PESO,
    # Constructo A: importancia tener relación sentimental
    # (1=Muy imp., 2=Bastante, 3=Regular[NO LEER], 4=Poco, 5=Nada)
    imp_pareja = P1,
    # Constructo B: ítems equivalentes de P4 (actitudes sobre relaciones hoy vs. hace 50 años)
    # Todos en escala 1=Muy acuerdo … 5=Nada acuerdo
    prej_sexo  = P4_6,   # "Están MENOS influidas por prejuicios" (invertir para comparar)
    igual_gen  = P4_4,   # "Son más igualitarias entre H y M"
    libertad   = P4_2,   # "Permiten mayor libertad individual"
    inestable  = P4_1,   # "Son más inestables que antes"
    # Apertura sexual (solo 3555)
    apertura   = P16,    # 1=Muy abierto … 5=Muy conservador
    # Uso de productos eróticos (para regresión logística)
    uso_prod   = P17,    # 1=Sí, 2=No
    # Orientación sexual
    orientac   = P13
  ) |>
  mutate(estudio = "E-3555 (Mar 2026)")


# 3. RECODIFICACIONES

# ---- 3.1 Sexo: etiquetas uniformes ----
recodificar_sexo <- function(df) {
  df |> mutate(sexo = case_when(
    sexo == 1 ~ "Hombre",
    sexo == 2 ~ "Mujer",
    TRUE      ~ NA_character_
  ))
}
e3501 <- recodificar_sexo(e3501)
e3555 <- recodificar_sexo(e3555)

# ---- 3.2 Grupos de edad (3 tramos comparables) ----
recodificar_edad <- function(df) {
  df |> mutate(grupo_edad = case_when(
    edad >= 18 & edad <= 34 ~ "18-34",
    edad >= 35 & edad <= 54 ~ "35-54",
    edad >= 55              ~ "55 y más",
    TRUE                    ~ NA_character_
  ))
}
e3501 <- recodificar_edad(e3501)
e3555 <- recodificar_edad(e3555)

# ---- 3.3 Nivel educativo (3 niveles) ----
# ESTUDIOS del CIS: 1=Sin estudios, 2=Primaria, 3=Sec.1ª, 4=Sec.2ª, 5=FP, 6=Superiores
recodificar_educ <- function(df) {
  df |> mutate(
    educ3 = case_when(
      estudios %in% c(1, 2, 3) ~ "Bajo",
      estudios %in% c(4, 5)    ~ "Medio",
      estudios == 6             ~ "Alto",
      TRUE                      ~ NA_character_
    ),
    educ_num = case_when(
      estudios %in% c(1, 2, 3) ~ 1,
      estudios %in% c(4, 5)    ~ 2,
      estudios == 6             ~ 3,
      TRUE                      ~ NA_real_
    )
  )
}
e3501 <- recodificar_educ(e3501)
e3555 <- recodificar_educ(e3555)

# ---- 3.4 Constructo A: imp_pareja ----
# 3501: 1-4 (1=Muy imp … 4=Nada); 8,9 → NA
# 3555: 1-5 con "Regular"=3 [NO LEER] → se excluye; 4→3, 5→4 para alinear escalas
e3501 <- e3501 |>
  mutate(imp_pareja = ifelse(imp_pareja %in% c(8, 9), NA, imp_pareja))

e3555 <- e3555 |>
  mutate(imp_pareja = case_when(
    imp_pareja == 1 ~ 1,
    imp_pareja == 2 ~ 2,
    imp_pareja == 3 ~ NA_real_,   # "Regular" → excluido para comparabilidad
    imp_pareja == 4 ~ 3,
    imp_pareja == 5 ~ 4,
    TRUE            ~ NA_real_
  ))

# ---- 3.5 NS/NC → NA en ítems de actitud ----
# En ambos estudios: 8=NS, 9=NC
limpiar_nsn <- function(x) ifelse(x %in% c(8, 9), NA, x)

e3501 <- e3501 |>
  mutate(across(c(prej_sexo, consen, deseo_gen, penet, fluidez, satisf),
                limpiar_nsn))

e3555 <- e3555 |>
  mutate(across(c(prej_sexo, igual_gen, libertad, inestable, apertura),
                limpiar_nsn))

# ---- 3.6 Inversión de prej_sexo en 3555 ----

e3555 <- e3555 |>
  mutate(prej_sexo_inv = 6 - prej_sexo)

# ---- 3.7 Variable dependiente: tiene_pareja (para regresión logística) ----

e3501 <- e3501 |>
  mutate(tiene_pareja = case_when(
    pareja_sit %in% c(2, 3) ~ 1,   # tiene pareja actualmente
    pareja_sit == 4         ~ 0,   # no tiene pareja actualmente
    TRUE                    ~ NA_real_
  ))

# P17 en 3555: 1=Sí ha usado productos eróticos, 2=No
e3555 <- e3555 |>
  mutate(uso_prod_bin = case_when(
    uso_prod == 1 ~ 1,
    uso_prod == 2 ~ 0,
    TRUE          ~ NA_real_
  ))

# ---- 3.8 Sexo numérico para modelos de regresión ----
e3501 <- e3501 |> mutate(sexo_num = ifelse(sexo == "Mujer", 1, 0))
e3555 <- e3555 |> mutate(sexo_num = ifelse(sexo == "Mujer", 1, 0))


# 4. CONSISTENCIA INTERNA — ALPHA DE CRONBACH

cat("\n")
cat("=============================================================\n")
cat(" SECCIÓN 4: CONSISTENCIA INTERNA (ALPHA DE CRONBACH)\n")
cat("=============================================================\n")
cat("Ítems: P6_1 (prejuicios), P6_2 (consentimiento), P6_3 (deseo genero),\n")
cat("       P6_4 (penetración), P6_5 (fluidez sexual)\n")
cat("Escala 1-5: 1=Muy de acuerdo, 5=Nada de acuerdo\n\n")

items_p6 <- e3501 |>
  select(prej_sexo, consen, deseo_gen, penet, fluidez) |>
  drop_na()

alpha_res <- psych::alpha(items_p6)
cat("Alpha de Cronbach:", round(alpha_res$total$raw_alpha, 3), "\n")
cat("Alpha estandarizado:", round(alpha_res$total$std.alpha, 3), "\n\n")
cat("Interpretación: alpha > 0.70 = consistencia aceptable para índice sumativo\n")
cat("Si alpha < 0.60, analizar ítems por separado (aun así los incluimos\n")
cat("como análisis descriptivo comparado)\n")


# 5. CONSTRUCCIÓN DEL ÍNDICE DE ACTITUDES SEXUALES (E-3501)

e3501 <- e3501 |>
  mutate(
    prej_sexo_i = 6 - prej_sexo,   # invertida
    consen_i    = 6 - consen,      # invertida
    fluidez_i   = 6 - fluidez,    # invertida
    # Índice sumativo (rango 5–25; mayor = más conservador)
    idx_actitud = prej_sexo_i + consen_i + deseo_gen + penet + fluidez_i
  )

cat("\n")
cat("=============================================================\n")
cat(" SECCIÓN 5: ÍNDICE DE ACTITUDES SEXUALES (E-3501)\n")
cat("=============================================================\n")
cat("Rango: 5 (liberal) – 25 (conservador)\n\n")

e3501 |>
  summarise(
    N     = sum(!is.na(idx_actitud)),
    Media = round(mean(idx_actitud, na.rm = TRUE), 2),
    SD    = round(sd(idx_actitud,   na.rm = TRUE), 2),
    Min   = min(idx_actitud,  na.rm = TRUE),
    Max   = max(idx_actitud,  na.rm = TRUE)
  ) |> print()


# 6. ANÁLISIS DESCRIPTIVO — DISTRIBUCIONES MARGINALES

cat("\n")
cat("=============================================================\n")
cat(" SECCIÓN 6: DESCRIPTIVOS\n")
cat("=============================================================\n")

cat("\n--- E-3501: Importancia de tener pareja ---\n")
e3501 |>
  filter(!is.na(imp_pareja)) |>
  count(imp_pareja) |>
  mutate(
    etiqueta = recode(as.character(imp_pareja),
                      "1" = "Muy importante",
                      "2" = "Bastante importante",
                      "3" = "Poco importante",
                      "4" = "Nada importante"),
    pct = round(n / sum(n) * 100, 1)
  ) |> print()

cat("\n--- E-3555: Importancia de tener relación sentimental ---\n")
e3555 |>
  filter(!is.na(imp_pareja)) |>
  count(imp_pareja) |>
  mutate(
    etiqueta = recode(as.character(imp_pareja),
                      "1" = "Muy importante",
                      "2" = "Bastante importante",
                      "3" = "Poco importante",
                      "4" = "Nada importante"),
    pct = round(n / sum(n) * 100, 1)
  ) |> print()

cat("\n--- E-3555: Apertura a prácticas sexuales (P16) ---\n")
e3555 |>
  filter(!is.na(apertura)) |>
  count(apertura) |>
  mutate(
    etiqueta = recode(as.character(apertura),
                      "1" = "Muy abierto/a",
                      "2" = "Bastante abierto/a",
                      "3" = "Ni/ni",
                      "4" = "Más bien conservador/a",
                      "5" = "Muy conservador/a"),
    pct = round(n / sum(n) * 100, 1)
  ) |> print()


# 7. ANÁLISIS POR SUBGRUPOS — MEDIAS COMPARADAS

cat("\n")
cat("=============================================================\n")
cat(" SECCIÓN 7: ANÁLISIS POR SUBGRUPOS\n")
cat("=============================================================\n")

calcular_medias <- function(df, var_dep, var_grupo, nombre_estudio) {
  df |>
    filter(!is.na(.data[[var_dep]]), !is.na(.data[[var_grupo]])) |>
    group_by(subgrupo = .data[[var_grupo]]) |>
    summarise(
      n     = n(),
      media = round(mean(.data[[var_dep]], na.rm = TRUE), 2),
      sd    = round(sd(.data[[var_dep]],   na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    mutate(estudio = nombre_estudio, variable_grupo = var_grupo)
}

# Importancia de la pareja por sexo, edad y educación
tabla_imp <- bind_rows(
  calcular_medias(e3501, "imp_pareja", "sexo",       "E-3501"),
  calcular_medias(e3555, "imp_pareja", "sexo",       "E-3555"),
  calcular_medias(e3501, "imp_pareja", "grupo_edad", "E-3501"),
  calcular_medias(e3555, "imp_pareja", "grupo_edad", "E-3555"),
  calcular_medias(e3501, "imp_pareja", "educ3",      "E-3501"),
  calcular_medias(e3555, "imp_pareja", "educ3",      "E-3555")
)

cat("\n--- Importancia pareja/rel. sentimental por subgrupos ---\n")
cat("(Escala 1-4: 1=Muy importante, 4=Nada importante)\n\n")
print(tabla_imp)

cat("\n--- Índice actitudes sexuales (E-3501) por subgrupos ---\n")
cat("(Escala 5-25: mayor = más conservador)\n\n")
bind_rows(
  calcular_medias(e3501, "idx_actitud", "sexo",       "E-3501"),
  calcular_medias(e3501, "idx_actitud", "grupo_edad", "E-3501"),
  calcular_medias(e3501, "idx_actitud", "educ3",      "E-3501")
) |> print()

cat("\n--- Apertura sexual (E-3555) por subgrupos ---\n")
cat("(Escala 1-5: 1=Muy abierto, 5=Muy conservador)\n\n")
bind_rows(
  calcular_medias(e3555, "apertura", "sexo",       "E-3555"),
  calcular_medias(e3555, "apertura", "grupo_edad", "E-3555"),
  calcular_medias(e3555, "apertura", "educ3",      "E-3555")
) |> print()

cat("\n--- Ítem comparable: percepción de prejuicios por subgrupos ---\n")
cat("(Escala 1-5: mayor = más prejuicios percibidos en ambos estudios)\n\n")
bind_rows(
  calcular_medias(e3501 |> mutate(prej_i = prej_sexo_i), "prej_i", "sexo",       "E-3501 P6_1"),
  calcular_medias(e3555 |> mutate(prej_i = prej_sexo_inv), "prej_i", "sexo",     "E-3555 P4_6 inv."),
  calcular_medias(e3501 |> mutate(prej_i = prej_sexo_i), "prej_i", "grupo_edad", "E-3501 P6_1"),
  calcular_medias(e3555 |> mutate(prej_i = prej_sexo_inv), "prej_i", "grupo_edad","E-3555 P4_6 inv.")
) |> print()


# 8. ANOVA
#    ¿Son estadísticamente significativas las diferencias entre subgrupos?

cat("\n")
cat("=============================================================\n")
cat(" SECCIÓN 8: ANOVA\n")
cat("=============================================================\n")

cat("\n--- ANOVA: índice actitudes ~ grupo_edad (E-3501) ---\n")
aov(idx_actitud ~ grupo_edad, data = e3501) |> summary() |> print()

cat("\n--- ANOVA: índice actitudes ~ sexo (E-3501) ---\n")
aov(idx_actitud ~ sexo, data = e3501) |> summary() |> print()

cat("\n--- ANOVA: índice actitudes ~ educ3 (E-3501) ---\n")
aov(idx_actitud ~ educ3, data = e3501) |> summary() |> print()

cat("\n--- ANOVA: apertura sexual ~ grupo_edad (E-3555) ---\n")
aov(apertura ~ grupo_edad, data = e3555) |> summary() |> print()

cat("\n--- ANOVA: apertura sexual ~ sexo (E-3555) ---\n")
aov(apertura ~ sexo, data = e3555) |> summary() |> print()


# 9. TABLAS DE CONTINGENCIA, CHI-CUADRADO Y V DE CRAMÉR

cat("\n")
cat("=============================================================\n")
cat(" SECCIÓN 9: TABLAS DE CONTINGENCIA\n")
cat("=============================================================\n")

calcular_contingencia <- function(var1, var2, nombre) {
  tab <- table(var1, var2, useNA = "no")
  cat("\n---", nombre, "---\n")
  print(round(prop.table(tab, margin = 2) * 100, 1))
  chi <- chisq.test(tab)
  v_cramer <- sqrt(chi$statistic / (sum(tab) * (min(dim(tab)) - 1)))
  cat("Chi² =", round(chi$statistic, 2),
      "| gl =", chi$parameter,
      "| p =", format.pval(chi$p.value, digits = 3),
      "| V Cramér =", round(v_cramer, 3), "\n")
}

# Orientación sexual × sexo
calcular_contingencia(
  e3501 |> filter(orientac %in% c(1,2,3)) |>
    mutate(or = recode(orientac, `1`="Heterosexual", `2`="Homosexual", `3`="Bisexual")) |>
    pull(or),
  e3501 |> filter(orientac %in% c(1,2,3)) |> pull(sexo),
  "E-3501: Orientación sexual × Sexo"
)

calcular_contingencia(
  e3555 |> filter(orientac %in% c(1,2,3,4)) |>
    mutate(or = recode(orientac, `1`="Heterosexual", `2`="Homosexual",
                       `3`="Bisexual", `4`="Asexual")) |>
    pull(or),
  e3555 |> filter(orientac %in% c(1,2,3,4)) |> pull(sexo),
  "E-3555: Orientación sexual × Sexo"
)

# Tiene pareja × grupo de edad (E-3501)
calcular_contingencia(
  e3501 |> filter(!is.na(tiene_pareja)) |>
    mutate(tp = recode(as.character(tiene_pareja), "1"="Con pareja", "0"="Sin pareja")) |>
    pull(tp),
  e3501 |> filter(!is.na(tiene_pareja)) |> pull(grupo_edad),
  "E-3501: Tiene pareja × Grupo de edad"
)

# Uso productos eróticos × sexo (E-3555)
calcular_contingencia(
  e3555 |> filter(!is.na(uso_prod_bin)) |>
    mutate(up = recode(as.character(uso_prod_bin), "1"="Sí", "0"="No")) |>
    pull(up),
  e3555 |> filter(!is.na(uso_prod_bin)) |> pull(sexo),
  "E-3555: Uso productos eróticos × Sexo"
)


# 10. REGRESIÓN LINEAL

cat("\n")
cat("=============================================================\n")
cat(" SECCIÓN 10: REGRESIÓN LINEAL\n")
cat("=============================================================\n")

# --- Modelo 1: Predictores del índice de actitudes sexuales (E-3501) ---
# V. dependiente: idx_actitud (5-25, mayor = más conservador)
# Predictores:    sexo (0=H, 1=M), edad, educ_num (1-3), ideología (1-10)
cat("\n--- Modelo 1: Predictores del índice de actitudes sexuales (E-3501) ---\n")
cat("V. dependiente: idx_actitud (5-25)\n")
cat("Mayor puntuación = actitudes más conservadoras/estereotipadas\n\n")

modelo1 <- lm(idx_actitud ~ sexo_num + edad + educ_num + ideologia,
              data      = e3501,
              na.action = na.exclude)
summary(modelo1)

cat("\nInterpretación: coeficientes negativos en sexo_num indican que las mujeres\n")
cat("puntúan más bajo (actitudes menos conservadoras) controlando por el resto.\n")


# --- Modelo 2: Predictores de la importancia de la pareja — ambos estudios ---
# Se incluye una dummy de estudio (0=3501, 1=3555) para detectar si hay
# diferencia entre encuestas no explicada por el perfil sociodemográfico

cat("\n--- Modelo 2: Predictores de la importancia de la pareja — datos combinados ---\n")
cat("V. dependiente: imp_pareja (1=Muy importante, 4=Nada importante)\n")
cat("El coeficiente de estudio_dummy indica si hay diferencia entre E-3501 y E-3555\n")
cat("una vez controlado el perfil sociodemográfico\n\n")

datos_combinados <- bind_rows(
  e3501 |>
    select(imp_pareja, sexo_num, edad, educ_num, ideologia) |>
    mutate(estudio_dummy = 0),
  e3555 |>
    select(imp_pareja, sexo_num, edad, educ_num, ideologia) |>
    mutate(estudio_dummy = 1)
)

modelo2 <- lm(imp_pareja ~ sexo_num + edad + educ_num + ideologia + estudio_dummy,
              data      = datos_combinados,
              na.action = na.exclude)
summary(modelo2)

cat("\nNota: si estudio_dummy es significativo, las diferencias entre estudios\n")
cat("no se explican solo por el perfil de la muestra → hallazgo metodológico.\n")


# 11. REGRESIÓN LOGÍSTICA BINARIA

cat("\n")
cat("=============================================================\n")
cat(" SECCIÓN 11: REGRESIÓN LOGÍSTICA BINARIA\n")
cat("=============================================================\n")
cat("Motivo: variables dependientes binarias (Sí/No) → la regresión lineal\n")
cat("produciría probabilidades fuera del rango [0,1] y residuos no normales.\n")
cat("La logística usa la función logit y sus coeficientes se interpretan como\n")
cat("log-odds, convertibles a Odds Ratios (OR) con exp().\n")

# --- Modelo 3: ¿Quién tiene más probabilidad de tener pareja? (E-3501) ---
cat("\n--- Modelo 3: Probabilidad de tener pareja actualmente (E-3501) ---\n")
cat("V. dependiente: tiene_pareja (1=Sí, 0=No)\n\n")

modelo3 <- glm(tiene_pareja ~ sexo_num + edad + educ_num + ideologia,
               data      = e3501,
               family    = binomial(link = "logit"),
               na.action = na.exclude)
summary(modelo3)

cat("\n--- Odds Ratios e intervalos de confianza al 95% (Modelo 3) ---\n")
cbind(
  OR    = round(exp(coef(modelo3)), 3),
  round(exp(confint(modelo3)), 3)
) |> print()

cat("\nInterpretación de OR:\n")
cat("  OR > 1 → mayor probabilidad de tener pareja\n")
cat("  OR < 1 → menor probabilidad de tener pareja\n")
cat("  OR = 1 → sin efecto\n")

# --- Modelo 4: ¿Quién tiene más probabilidad de haber usado productos eróticos? (E-3555) ---
cat("\n--- Modelo 4: Probabilidad de haber usado productos eróticos (E-3555) ---\n")
cat("V. dependiente: uso_prod_bin (1=Sí, 0=No)\n\n")

modelo4 <- glm(uso_prod_bin ~ sexo_num + edad + educ_num + ideologia,
               data      = e3555,
               family    = binomial(link = "logit"),
               na.action = na.exclude)
summary(modelo4)

cat("\n--- Odds Ratios e intervalos de confianza al 95% (Modelo 4) ---\n")
cbind(
  OR    = round(exp(coef(modelo4)), 3),
  round(exp(confint(modelo4)), 3)
) |> print()

cat("\nNota comparativa: comparar el efecto del sexo (sexo_num) en los modelos\n")
cat("3 y 4 es en sí mismo un hallazgo: ¿predice igual el sexo la probabilidad\n")
cat("de tener pareja que la de usar productos eróticos?\n")


# 12. VISUALIZACIONES CON GGPLOT2

cat("\n")
cat("=============================================================\n")
cat(" SECCIÓN 12: VISUALIZACIONES\n")
cat("=============================================================\n")

# Paleta de colores para los dos estudios
col_estudios <- c("E-3501 (Ene 2025)" = "#2C7BB6",
                  "E-3555 (Mar 2026)"  = "#D7191C")

# --- Gráfico 1: Importancia de la pareja por sexo y edad (comparación entre estudios) ---
datos_g1 <- bind_rows(
  e3501 |>
    filter(!is.na(imp_pareja), !is.na(sexo), !is.na(grupo_edad)) |>
    group_by(sexo, grupo_edad, estudio) |>
    summarise(media = mean(imp_pareja, na.rm = TRUE), .groups = "drop"),
  e3555 |>
    filter(!is.na(imp_pareja), !is.na(sexo), !is.na(grupo_edad)) |>
    group_by(sexo, grupo_edad, estudio) |>
    summarise(media = mean(imp_pareja, na.rm = TRUE), .groups = "drop")
)

g1 <- ggplot(datos_g1,
             aes(x = grupo_edad, y = media, fill = estudio)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~ sexo) +
  scale_fill_manual(values = col_estudios) +
  labs(
    title    = "Importancia de la pareja/relación sentimental",
    subtitle = "Comparación E-3501 (2025) vs. E-3555 (2026) por sexo y grupo de edad",
    caption  = "Escala: 1 = Muy importante, 4 = Nada importante",
    x        = "Grupo de edad",
    y        = "Media",
    fill     = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(g1)
# ggsave("g1_importancia_pareja.png", g1, width = 9, height = 5, dpi = 150)


# --- Gráfico 2: Índice de actitudes sexuales (E-3501) por edad y sexo con IC ---
datos_g2 <- e3501 |>
  filter(!is.na(idx_actitud), !is.na(grupo_edad), !is.na(sexo)) |>
  group_by(grupo_edad, sexo) |>
  summarise(
    media = mean(idx_actitud, na.rm = TRUE),
    se    = sd(idx_actitud,   na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

g2 <- ggplot(datos_g2,
             aes(x = grupo_edad, y = media, color = sexo, group = sexo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = media - 1.96 * se,
                    ymax = media + 1.96 * se),
                width = 0.2) +
  scale_color_manual(values = c("Hombre" = "#2C7BB6", "Mujer" = "#D7191C")) +
  labs(
    title    = "Índice de actitudes hacia la sexualidad (E-3501)",
    subtitle = "Por grupo de edad y sexo — con intervalos de confianza al 95%",
    caption  = "Escala: 5 = más liberal, 25 = más conservador",
    x        = "Grupo de edad",
    y        = "Media del índice",
    color    = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(g2)
# ggsave("g2_indice_actitudes.png", g2, width = 8, height = 5, dpi = 150)


# --- Gráfico 3: Ítem comparable entre estudios (prejuicios) por edad y sexo ---
datos_g3 <- bind_rows(
  e3501 |>
    filter(!is.na(prej_sexo_i), !is.na(grupo_edad), !is.na(sexo)) |>
    mutate(prej_comp = prej_sexo_i) |>
    group_by(grupo_edad, sexo, estudio) |>
    summarise(media = mean(prej_comp, na.rm = TRUE), .groups = "drop"),
  e3555 |>
    filter(!is.na(prej_sexo_inv), !is.na(grupo_edad), !is.na(sexo)) |>
    mutate(prej_comp = prej_sexo_inv) |>
    group_by(grupo_edad, sexo, estudio) |>
    summarise(media = mean(prej_comp, na.rm = TRUE), .groups = "drop")
)

g3 <- ggplot(datos_g3,
             aes(x = grupo_edad, y = media,
                 color = sexo, linetype = estudio,
                 group = interaction(sexo, estudio))) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Hombre" = "#2C7BB6", "Mujer" = "#D7191C")) +
  labs(
    title    = "Percepción de prejuicios sociales sobre el sexo",
    subtitle = "Ítem comparable entre E-3501 (P6_1) y E-3555 (P4_6 invertida)",
    caption  = "Escala: 1 = pocos prejuicios, 5 = muchos prejuicios",
    x        = "Grupo de edad",
    y        = "Media",
    color    = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(g3)
# ggsave("g3_prejuicios.png", g3, width = 8, height = 5, dpi = 150)


# --- Gráfico 4: Orientación sexual en ambos estudios ---
datos_g4 <- bind_rows(
  e3501 |>
    filter(orientac %in% c(1, 2, 3)) |>
    mutate(or = recode(orientac,
                       `1` = "Heterosexual", `2` = "Homosexual", `3` = "Bisexual")) |>
    count(or, estudio) |>
    mutate(pct = n / sum(n) * 100),
  e3555 |>
    filter(orientac %in% c(1, 2, 3, 4)) |>
    mutate(or = recode(orientac,
                       `1` = "Heterosexual", `2` = "Homosexual",
                       `3` = "Bisexual",     `4` = "Asexual")) |>
    count(or, estudio) |>
    mutate(pct = n / sum(n) * 100)
)

g4 <- ggplot(datos_g4,
             aes(x = reorder(or, -pct), y = pct, fill = estudio)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = col_estudios) +
  labs(
    title    = "Distribución de orientación sexual",
    subtitle = "Comparación E-3501 (2025) vs. E-3555 (2026)",
    x        = NULL,
    y        = "Porcentaje (%)",
    fill     = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(g4)
# ggsave("g4_orientacion.png", g4, width = 7, height = 5, dpi = 150)


# --- Gráfico 5: Odds Ratios de ambos modelos logísticos (comparación visual) ---
extraer_or <- function(modelo, nombre_modelo) {
  coefs <- summary(modelo)$coefficients
  or    <- exp(coef(modelo))
  ci    <- exp(suppressMessages(confint(modelo)))
  tibble(
    variable = rownames(coefs)[-1],   # excluir intercepto
    OR       = or[-1],
    IC_inf   = ci[-1, 1],
    IC_sup   = ci[-1, 2],
    modelo   = nombre_modelo
  )
}

datos_or <- bind_rows(
  extraer_or(modelo3, "Modelo 3: Tiene pareja (E-3501)"),
  extraer_or(modelo4, "Modelo 4: Usa productos eróticos (E-3555)")
) |>
  mutate(variable = recode(variable,
                           "sexo_num"  = "Mujer (vs. Hombre)",
                           "edad"      = "Edad",
                           "educ_num"  = "Nivel educativo",
                           "ideologia" = "Ideología (dcha.)"
  ))

g5 <- ggplot(datos_or,
             aes(x = OR, y = variable, color = modelo, shape = modelo)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup),
                 height = 0.2, position = position_dodge(0.5)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Modelo 3: Tiene pareja (E-3501)"        = "#2C7BB6",
                                "Modelo 4: Usa productos eróticos (E-3555)" = "#D7191C")) +
  labs(
    title    = "Odds Ratios — Modelos de regresión logística",
    subtitle = "Modelo 3 (tener pareja, E-3501) vs. Modelo 4 (uso productos eróticos, E-3555)",
    caption  = "Intervalos de confianza al 95%. OR = 1: sin efecto.",
    x        = "Odds Ratio",
    y        = NULL,
    color    = NULL,
    shape    = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(g5)
# ggsave("g5_odds_ratios.png", g5, width = 9, height = 5, dpi = 150)


# FIN DEL SCRIPT
cat("\n\n*** Análisis completado correctamente ***\n")
cat("Secciones ejecutadas:\n")
cat("  4.  Alpha de Cronbach\n")
cat("  5.  Índice sumativo de actitudes sexuales\n")
cat("  6.  Descriptivos\n")
cat("  7.  Medias por subgrupos\n")
cat("  8.  ANOVA\n")
cat("  9.  Tablas de contingencia + Chi² + V de Cramér\n")
cat("  10. Regresión lineal (2 modelos)\n")
cat("  11. Regresión logística binaria (2 modelos + Odds Ratios)\n")
cat("  12. Visualizaciones ggplot2 (5 gráficos)\n")

