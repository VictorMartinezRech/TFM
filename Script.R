library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)
library(RColorBrewer)

#### PAISES

# Cargar los datos
dades <- read_excel("E:/TFM/Anàlisis R/Base de dades_def.xlsx", sheet='Llista articles')
#dades <- read_excel("C:/Users/vmartinezr/Documents/TFM/Final/Base de dades_def.xlsx", sheet='Llista articles')

# Países y continentes
separate_countries <- function(country_string) {
  countries <- unlist(str_split(country_string, ",\\s*|\\s+and\\s+"))
  return(countries)
}

dades_country <- dades %>%
  mutate(Country = map(`Country of the data`, separate_countries)) %>%
  unnest(Country) %>%
  mutate(Country = str_trim(Country))  # Eliminar espacios en blanco al principio y al final


paises <- dades_country$Country
df_paises <- data.frame(Pais = paises)
df_paises$Conteo <- 1

tabla_conteo <- df_paises %>%
  group_by(Pais) %>%
  summarise(Conteo = sum(Conteo))


# Tabla de países y continentes
info_paises <- data.frame(
  Country = c("Australia", "New Zealand", "Belgium", "Austria", "Ireland", "Iceland", 
              "Norway", "Switzerland", "UK", "Sweden", "Denmark", "Finland", 
              "France", "Germany", "Italy", "The Netherlands", "Luxembourg", "Spain",
              "Brazil", "Argentina", "USA", "Canada", "Japan", "China", "Russia", "Korea", "Qatar", 
              "Not reported", "."),
  Continent = c(rep("Oceania", 2), rep("Europe", 16), rep("America", 4), 
                rep("Asia", 5), rep("Unknown", 2))
)

tabla_final <- merge(tabla_conteo, info_paises, by.x = "Pais", by.y = "Country", all.x = TRUE)
print(tabla_final)

continentes <- tabla_final %>%
  group_by(Continent) %>%
  summarise(Conteo = sum(Conteo))

print(tabla_conteo)

# Gráficos
ggplot(tabla_final, aes(x = Continent, y = Conteo, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Conteo de países por Continente",
       x = "Continente",
       y = "Conteo") +
  theme_minimal()

# 10 países con mayor frecuencia
top_paises <- tabla_final %>% arrange(desc(Conteo)) %>% head(10)


ggplot(top_paises, aes(x = reorder(Pais, -Conteo), y = Conteo)) +
  geom_bar(stat = "identity", fill = "#1f77b4", width = 0.7) +
  labs(
    title = "Top 10 Countries by Article Frequency",
    x = "Country",
    y = "Number of Articles"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#### Publication Year

(pyear <- table(dades$`Publication Year`))
(pyear_prop <- prop.table(pyear))

pyear_df <- as.data.frame(pyear)
pyear_prop_df <- as.data.frame(pyear_prop)

colnames(pyear_df) <- c("Year", "Frequency")

pyear_df$Year <- as.numeric(as.character(pyear_df$Year))

ggplot(pyear_df, aes(x = Year, y = Frequency)) +
  geom_line(color = "darkblue", size = 1) + 
  geom_point(color = "darkblue", size = 1.5) +
  geom_text(aes(label = Frequency), vjust = -1, color = "darkblue", size = 3) +
  scale_x_continuous(breaks = seq(min(pyear_df$Year), max(pyear_df$Year), 1), expand = c(0.05, 0.05)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 2), expand = c(0, 0)) +
  labs(x = 'Year', y = 'Frequency', title = 'Trend of Articles Published Per Year') + 
  theme_minimal() +
  theme(text = element_text(family = "Calibri", size = 12), 
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold", color = "black"),
        axis.title.x = element_text(vjust = -0.5, size = 10),
        axis.title.y = element_text(vjust = 1, size = 10),
        axis.text = element_text(size = 10),  # Axis text size adjusted
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10))


#### Journal Name

(journal <- table(dades$`Journal Name`))
(journal_prop <- prop.table(journal))

journal_df <- as.data.frame(journal)
journal_prop_df <- as.data.frame(journal_prop)


#### Sports

separate_sport <- function(sport_string) {
  sport <- unlist(str_split(sport_string, ",\\s*|\\s+and\\s+"))
  return(sport)
}

dades_sport <- dades %>%
  mutate(Sport = map(`Sport`, separate_sport)) %>%
  unnest(Sport) %>%
  mutate(Sport = str_trim(Sport))

sport <- dades_sport$Sport
df_sport <- data.frame(Sport = sport)
df_sport$Conteo <- 1

tabla_sport_conteo <- df_sport %>%
  group_by(Sport) %>%
  summarise(Conteo = sum(Conteo))

print(tabla_sport_conteo)

sum(tabla_sport_conteo$Conteo)

tabla_sport_conteo <- tabla_sport_conteo %>%
  mutate(Sport = ifelse(Conteo %in% c(1, 2, 3), "Others", Sport)) %>%
  group_by(Sport) %>%
  summarise(Conteo = sum(Conteo), .groups = "drop") %>%
  mutate(Percentage = Conteo / sum(Conteo) * 100)

tabla_sport_conteo$Sport <- str_to_title(tabla_sport_conteo$Sport)

ordered_levels <- tabla_sport_conteo %>%
  filter(!(Sport %in% c("Others", "Not Sport"))) %>%
  arrange(desc(Conteo)) %>%
  pull(Sport)

ordered_levels <- c(ordered_levels, "Others", "Not Sport")

tabla_sport_conteo <- tabla_sport_conteo %>%
  mutate(Sport = factor(Sport, levels = ordered_levels))

ggplot(tabla_sport_conteo, aes(x = Sport, y = Conteo)) +
  geom_bar(stat = "identity", fill = "#1f77b4", width = 0.7) +
  labs(
    title = "Distribution of Sports in Articles",
    x = "Sport",
    y = "Number of Articles"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#### Gender

(gender <- table(dades$Gender))
(gender_prop <- prop.table(gender))

gender_df <- as.data.frame(gender)
gender_prop_df <- as.data.frame(gender_prop)


#### Category Participants

(cat_part <- table(dades$`Category participants`))
(cat_part_prop <- prop.table(cat_part))

cat_part_df <- as.data.frame(cat_part)
cat_part_prop_df <- as.data.frame(cat_part_prop)


#### Category Classification

(cat_class <- table(dades$`Category classification`))
(cat_class_prop <- prop.table(cat_class))

cat_class_df <- as.data.frame(cat_class)
cat_class_prop_df <- as.data.frame(cat_class_prop)


#### Type of model

(type_mod <- table(dades$`Type of model`))
(type_mod_prop <- prop.table(type_mod))

type_mod_df <- as.data.frame(type_mod)
type_mod_prop_df <- as.data.frame(type_mod_prop)


#### Kaplan-Meier

(K_Meier <- table(dades$`Kaplan-Meier`))
(K_Meier_prop <- prop.table(K_Meier))

K_Meier_df <- as.data.frame(K_Meier)
K_Meier_prop_df <- as.data.frame(K_Meier_prop)


#### Cox

(Cox_var <- table(dades$`Cox`))
(Cox_var_prop <- prop.table(Cox_var))

Cox_var_df <- as.data.frame(Cox_var)
Cox_var_prop_df <- as.data.frame(Cox_var_prop)


#### Type of paradigm

(Type_par <- table(dades$`Type of paradigm`))
(Type_par_prop <- prop.table(Type_par))

Type_par_df <- as.data.frame(Type_par)
Type_par_prop_df <- as.data.frame(Type_par_prop)


#### SOFTWARE

separate_software <- function(software_string) {
  software <- unlist(str_split(software_string, ",\\s*|\\s+and\\s+"))
  return(software)
}

dades_software <- dades %>%
  mutate(Software = map(`Statistical software`, separate_software)) %>%
  unnest(Software) %>%
  mutate(Software = str_trim(Software))

software <- dades_software$Software
df_software <- data.frame(Software = software)
df_software$Conteo <- 1

tabla_software_conteo <- df_software %>%
  group_by(Software) %>%
  summarise(Conteo = sum(Conteo))

print(tabla_software_conteo)


tabla_software_conteo <- tabla_software_conteo %>%
  mutate(Software = ifelse(Software == "Not reported", "zzz_Not reported", Software)) %>%
  arrange(desc(Conteo)) %>%
  mutate(Software = factor(Software, levels = c(setdiff(Software, "zzz_Not reported"), "zzz_Not reported")))

ggplot(tabla_software_conteo, aes(x = Software, y = Conteo)) +
  geom_bar(stat = "identity", fill = "#1f77b4", width = 0.7) +
  labs(
    title = "Statistical Software Used in the Articles",
    x = "Software",
    y = "Number of Articles"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_discrete(labels = ~ gsub("zzz_", "", .x))

#### Macro/package

(macro <- table(dades$`Macro/package`))
(macro_prop <- prop.table(macro))

# Convertir las tablas a data frame
macro_df <- as.data.frame(macro)
macro_prop_df <- as.data.frame(macro_prop)


#### Data sharing

(data_share <- table(dades$`Data sharing`))
(data_share_prop <- prop.table(data_share))

# Convertir las tablas a data frame
data_share_df <- as.data.frame(data_share)
data_share_prop_df <- as.data.frame(data_share_prop)


#### Code sharing

(code_share <- table(dades$`Code sharing`))
(code_share_prop <- prop.table(code_share))

# Convertir las tablas a code frame
code_share_df <- as.data.frame(code_share)
code_share_prop_df <- as.data.frame(code_share_prop)


#### Purpose of the analysis

(purpose <- table(dades$`Purpose of the analysis`))
(purpose_prop <- prop.table(purpose))

# Convertir las tablas a code frame
purpose_df <- as.data.frame(purpose)
purpose_prop_df <- as.data.frame(purpose_prop)


#### Identify dates or events

(dates_events <- table(dades$`Identify dates or events`))
(dates_events_prop <- prop.table(dates_events))

# Convertir las tablas a code frame
dates_events_df <- as.data.frame(dates_events)
dates_events_prop_df <- as.data.frame(dates_events_prop)


#### Circumstances data censored

(d_censored <- table(dades$`Circumstances data censored`))
(d_censored_prop <- prop.table(d_censored))

# Convertir las tablas a code frame
d_censored_df <- as.data.frame(d_censored)
d_censored_prop_df <- as.data.frame(d_censored_prop)


#### Statistical methods rates

(stat_met <- table(dades$`Statistical methods rates`))
(stat_met_prop <- prop.table(stat_met))

# Convertir las tablas a code frame
stat_met_df <- as.data.frame(stat_met)
stat_met_prop_df <- as.data.frame(stat_met_prop)


#### Assumptions of survival analysis

(assumptions <- table(dades$`Assumptions of survival analysis`))
(assumptions_prop <- prop.table(assumptions))

# Convertir las tablas a code frame
assumptions_df <- as.data.frame(assumptions)
assumptions_prop_df <- as.data.frame(assumptions_prop)


#### Estimated survival probability

(surv_prob <- table(dades$`Give the estimated survival probability`))
(surv_prob_prop <- prop.table(surv_prob))

# Convertir las tablas a code frame
surv_prob_df <- as.data.frame(surv_prob)
surv_prob_prop_df <- as.data.frame(surv_prob_prop)


#### Nº participants in risk

(risk_part <- table(dades$`Nº participants en risk`))
(risk_part_prop <- prop.table(risk_part))

# Convertir las tablas a code frame
risk_part_df <- as.data.frame(risk_part)
risk_part_prop_df <- as.data.frame(risk_part_prop)


#### Plot cumulative

(plot_cum <- table(dades$`Plot cumulative`))
(plot_cum_prop <- prop.table(plot_cum))

# Convertir las tablas a code frame
plot_cum_df <- as.data.frame(plot_cum)
plot_cum_prop_df <- as.data.frame(plot_cum_prop)


#### Median survival times

(med_surv_t <- table(dades$`Reporting median survival times`))
(med_surv_t_prop <- prop.table(med_surv_t))

# Convertir las tablas a code frame
med_surv_t_df <- as.data.frame(med_surv_t)
med_surv_t_prop_df <- as.data.frame(med_surv_t_prop)


#### Results in a graph/table

(graph_table <- table(dades$`Results in a graph or table`))
(graph_table_prop <- prop.table(graph_table))

# Convertir las tablas a code frame
graph_table_df <- as.data.frame(graph_table)
graph_table_prop_df <- as.data.frame(graph_table_prop)


#### Statistical methods curves

(stat_met_curve <- table(dades$`Statistical methods curves`))
(stat_met_curve_prop <- prop.table(stat_met_curve))

# Convertir las tablas a code frame
stat_met_curve_df <- as.data.frame(stat_met_curve)
stat_met_curve_prop_df <- as.data.frame(stat_met_curve_prop)


#### Report P value

(p_value <- table(dades$`Report the P value`))
(p_value_prop <- prop.table(p_value))

# Convertir las tablas a code frame
p_value_df <- as.data.frame(p_value)
p_value_prop_df <- as.data.frame(p_value_prop)


#### Regression Model

(reg_mod <- table(dades$`Regression model`))
(reg_mod_prop <- prop.table(reg_mod))

# Convertir las tablas a code frame
reg_mod_df <- as.data.frame(reg_mod)
reg_mod_prop_df <- as.data.frame(reg_mod_prop)


#### Measure of risk

(measure_risk <- table(dades$`Measure of risk`))
(measure_risk_prop <- prop.table(measure_risk))

# Convertir las tablas a code frame
measure_risk_df <- as.data.frame(measure_risk)
measure_risk_prop_df <- as.data.frame(measure_risk_prop)


#### Censoring type

(cens_type <- table(dades$`Tipo de censura`))
(cens_type_prop <- prop.table(cens_type))

# Convertir las tablas a code frame
cens_type_df <- as.data.frame(cens_type)
cens_type_prop_df <- as.data.frame(cens_type_prop)


#### Truncated

(trunc <- table(dades$Truncament))
(trunc_prop <- prop.table(trunc))

# Convertir las tablas a code frame
trunc_df <- as.data.frame(trunc)
trunc_prop_df <- as.data.frame(trunc_prop)


#### Random effects

(rand_eff <- table(dades$`Random effects`))
(rand_eff_prop <- prop.table(rand_eff))

# Convertir las tablas a code frame
rand_eff_df <- as.data.frame(rand_eff)
rand_eff_prop_df <- as.data.frame(rand_eff_prop)


#### Time dependent

(time_dep <- table(dades$`Time dependent`))
(time_dep_prop <- prop.table(time_dep))

# Convertir las tablas a code frame
time_dep_df <- as.data.frame(time_dep)
time_dep_prop_df <- as.data.frame(time_dep_prop)


#### Competing risks

(comp_risk <- table(dades$`Competing-risks`))
(comp_risk_prop <- prop.table(comp_risk))

# Convertir las tablas a code frame
comp_risk_df <- as.data.frame(comp_risk)
comp_risk_prop_df <- as.data.frame(comp_risk_prop)


# Cargar paquetes
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(readxl)
library(pheatmap)

# Leer archivo
df <- read_excel("E:/TFM/Anàlisis R/Base de dades_def.xlsx", sheet='Llista articles')
#df <- read_excel("C:/Users/vmartinezr/Documents/TFM/Final/Base de dades_def.xlsx", sheet='Llista articles')

# Solo queremos convertir las columnas de los distintos tipos de métodos y modelos de supervivencia
cols_metodos <- names(df)[match("Kaplan-Meier", names(df)) : match("Exponentiated log-linked general linear models", names(df))]
df_metodos <- df[, cols_metodos]
df_metodos <- df_metodos %>%
  mutate(across(everything(), ~ ifelse(. == "Yes", 1, 0)))

frecuencias <- colSums(df_metodos) %>%
  sort(decreasing = TRUE) %>%
  as.data.frame()

colnames(frecuencias) <- "Número_de_artículos"
frecuencias$Método <- rownames(frecuencias)
rownames(frecuencias) <- NULL
print(frecuencias)


# Gráfico
ggplot(frecuencias, aes(x = reorder(Método, -Número_de_artículos), y = Número_de_artículos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Frecuencia de uso por método de análisis",
       x = "Método", y = "Número de artículos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df_metodos$Total <- rowSums(df_metodos)
sin_metodo <- df_metodos %>% filter(Total == 0)
cat("Número de artículos sin ningún método mencionado:", nrow(sin_metodo), "\n")

# Métodos con al menos 4 artículos
frecuencias_filtradas <- frecuencias %>%
  filter(Número_de_artículos >= 4) %>%
  arrange(desc(Número_de_artículos))

# Gráfico
ggplot(frecuencias_filtradas, aes(x = reorder(Método, Número_de_artículos), y = Número_de_artículos)) +
  geom_bar(stat = "identity", fill = "#1f77b4", width = 0.7) +
  coord_flip() +
  labs(
    title = "Frequency of Use by Statistical Method (>= 4 articles)",
    x = "Method",
    y = "Number of Articles"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 11)
  )

#Miramos los artículos que usan solamente comparación de curvas, sin usar ningún modelo de supervivencia
solo_comparacion <- df_metodos %>%
  filter((`Log-rank` == 1 | Wilcoxon == 1 | `Harrington–Fleming` == 1) &
           rowSums(select(., -`Log-rank`, -Wilcoxon, -`Harrington–Fleming`)) == 0)

cat("Artículos con solo comparación de curvas:", nrow(solo_comparacion), "\n")

# ---- Análisis 5: Kaplan-Meier + Cox ----
km_cox <- df_metodos %>%
  filter(`Kaplan-Meier` == 1 & Cox == 1)

cat("Artículos con Kaplan-Meier + Cox:", nrow(km_cox), "\n")

# Análisis de combinaciones frecuentes
df_metodos$combo <- apply(df_metodos, 1, function(row) {
  paste(names(df_metodos)[which(row == 1)], collapse = " + ")
})

# Contar combinaciones
combinaciones <- df_metodos %>%
  count(combo, sort = TRUE) %>%
  filter(combo != "")

# Ver top combinaciones
print(head(combinaciones, 10))

#Filtraremos por los métodos usados al menos en 5 artículos
frecuencias_2 <- colSums(df_metodos[,1:24])
metodos_frecuentes <- names(frecuencias_2[frecuencias_2 >= 5])

df_filtrado <- df_metodos[, metodos_frecuentes]

matriz_cooc_filtrada <- as.matrix(t(df_filtrado)) %*% as.matrix(df_filtrado)
diag(matriz_cooc_filtrada) <- 0

pheatmap(matriz_cooc_filtrada,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         display_numbers = TRUE,
         color = colorRampPalette(c("white", "steelblue"))(50),
         main = "Heatmap (methods with ≥ 5 articles)")
