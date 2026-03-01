# ==============================================================================
# Section 2.5 — Statistiques descriptives et analyse univariée
# Tableau 1 (construction échantillon) + Tableau 2 (stats desc) +
# Tableau 3 (corrélations) + Graphiques 1 à 5
# ==============================================================================

library(tidyverse)
library(scales)
library(weights)   # wtd.cor() — corrélations pondérées (Tableau 3)

setwd("C:/Users/alexi/Documents/Université/M2 TIDE/Etude Temps de travail des femmes")

dir_out <- "C:/Users/alexi/Documents/Université/M2 TIDE/Etude Temps de travail des femmes/graphiques"
dir.create(dir_out, showWarnings = FALSE)


# ==============================================================================
# PARAMÈTRES GRAPHIQUES COMMUNS
# ==============================================================================

palette_y <- c("Non-Emploi"    = "#4C72B0",
               "Temps Partiel" = "#DD8452",
               "Temps Plein"   = "#55A868")

theme_memoire <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


# ==============================================================================
# TABLEAU 1 — CONSTRUCTION DE L'ÉCHANTILLON
# Effectifs bruts et pondérés à chaque étape de filtre
# ==============================================================================

df_brut <- readRDS("base_annuelle_brute.rds")

etapes <- tribble(
  ~Etape, ~Description,
  1, "Population totale EEC",
  2, "Femmes (SEXE == 2)",
  3, "En couple (COUPL_LOG == 1)",
  4, "25-54 ans",
  5, "France métropolitaine hors DROM et Corse",
  6, "Après suppression des NA (base modèle finale)"
)

n_brut <- function(data) nrow(data)
n_pond <- function(data) round(sum(data$EXTRI / 4, na.rm = TRUE))

df_e1 <- df_brut
df_e2 <- df_e1 %>% filter(SEXE == 2)
df_e3 <- df_e2 %>% filter(COUPL_LOG == 1)
df_e4 <- df_e3 %>% filter(AGE >= 25 & AGE <= 54)
df_e5 <- df_e4 %>% filter(!REGIO %in% c(1, 2, 3, 4, 94))

df <- readRDS("base_modele_clean.rds")
n6_brut <- nrow(df)
n6_pond <- round(sum(df$POIDS))

tableau1 <- etapes %>%
  mutate(
    N_brut   = c(n_brut(df_e1), n_brut(df_e2), n_brut(df_e3),
                 n_brut(df_e4), n_brut(df_e5), n6_brut),
    N_pondere = c(n_pond(df_e1), n_pond(df_e2), n_pond(df_e3),
                  n_pond(df_e4), n_pond(df_e5), n6_pond),
    Pct_retention = round(100 * N_brut / N_brut[1], 1)
  )

write.csv(tableau1, file.path(dir_out, "tableau1_construction_echantillon.csv"), row.names = FALSE)
print(tableau1)


# ==============================================================================
# TABLEAU 2 — STATISTIQUES DESCRIPTIVES COMPLÈTES PONDÉRÉES
# Variables continues : moyenne, écart-type, min, max
# Variables catégorielles : fréquences pondérées
# Global + ventilation par modalité de Y_OFFRE
# ==============================================================================

wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
wsd   <- function(x, w) {
  m <- wmean(x, w)
  sqrt(sum(w * (x - m)^2, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE))
}

vars_cont <- c("AGE", "NB_ENF_0_2", "NB_ENF_3_5", "NB_ENF_6PLUS", "TX_CHOMAGE")

stats_cont <- function(data, modalite = "Global") {
  map_dfr(vars_cont, function(v) {
    x <- data[[v]]; w <- data$POIDS
    tibble(
      Variable   = v,
      Modalite   = modalite,
      Moyenne    = round(wmean(x, w), 3),
      Ecart_type = round(wsd(x, w), 3),
      Min        = round(min(x, na.rm = TRUE), 3),
      Max        = round(max(x, na.rm = TRUE), 3),
      N_obs      = sum(!is.na(x))
    )
  })
}

tableau2_cont <- bind_rows(
  stats_cont(df, "Global"),
  map_dfr(levels(df$Y_OFFRE), ~ stats_cont(df %>% filter(Y_OFFRE == .x), .x))
)

vars_cat <- c("DIPLOME", "CSP_CONJOINT", "IMMIGREE")

freq_pond <- function(data, var, modalite = "Global") {
  data %>%
    mutate(cat = as.character(.data[[var]])) %>%
    group_by(cat) %>%
    summarise(N_pond = sum(POIDS), .groups = "drop") %>%
    mutate(Variable = var, Modalite_Y = modalite,
           Pct = round(100 * N_pond / sum(N_pond), 1)) %>%
    rename(Modalite_var = cat)
}

tableau2_cat <- bind_rows(
  map_dfr(vars_cat, ~ freq_pond(df, .x, "Global")),
  map_dfr(levels(df$Y_OFFRE), function(y)
    map_dfr(vars_cat, ~ freq_pond(df %>% filter(Y_OFFRE == y), .x, y))
  )
)

write.csv(tableau2_cont, file.path(dir_out, "tableau2_continu.csv"),   row.names = FALSE)
write.csv(tableau2_cat,  file.path(dir_out, "tableau2_categoriel.csv"), row.names = FALSE)
print(tableau2_cont %>% filter(Modalite == "Global"))


# ==============================================================================
# TABLEAU 3 — MATRICE DE CORRÉLATIONS PONDÉRÉES
# Variables continues : AGE, NB_ENF_0_2, NB_ENF_3_5, NB_ENF_6PLUS, TX_CHOMAGE
# Méthode : corrélation de Pearson pondérée (wtd.cor, package weights)
# ==============================================================================

mat_cor <- wtd.cor(
  x      = df %>% select(all_of(vars_cont)) %>% as.matrix(),
  weight = df$POIDS
)

tableau3 <- as.data.frame(round(mat_cor$correlation, 3))

write.csv(tableau3, file.path(dir_out, "tableau3_correlations.csv"), row.names = TRUE)
print(tableau3)


# ==============================================================================
# GRAPHIQUE 1 — Distribution de Y_OFFRE (pondérée)
# ==============================================================================

distrib_y <- df %>%
  group_by(Y_OFFRE) %>%
  summarise(N_pond = sum(POIDS), .groups = "drop") %>%
  mutate(
    Pct   = N_pond / sum(N_pond),
    label = paste0(round(Pct * 100, 1), "%\n(n=", format(round(N_pond), big.mark = " "), ")")
  )

g1 <- ggplot(distrib_y, aes(x = Y_OFFRE, y = Pct, fill = Y_OFFRE)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = palette_y) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(distrib_y$Pct) * 1.25)) +
  labs(
    title    = "Graphique 1 — Distribution de l'offre de travail (Y_OFFRE)",
    subtitle = "Fréquences pondérées — Femmes en couple, 25-54 ans, France métropolitaine hors Corse, EEC 2024",
    x = NULL, y = "Part pondérée (%)"
  ) +
  theme_memoire

ggsave(file.path(dir_out, "graphique1_distrib_Y.png"), g1, width = 7, height = 5, dpi = 300)


# ==============================================================================
# GRAPHIQUE 2 — Profil de l'offre de travail selon l'âge (courbes LOESS)
# Motive l'inclusion de AGE_CARRE dans le modèle
# ==============================================================================

courbe_age <- df %>%
  group_by(AGE, Y_OFFRE) %>%
  summarise(N_pond = sum(POIDS), .groups = "drop") %>%
  group_by(AGE) %>%
  mutate(Pct = N_pond / sum(N_pond)) %>%
  ungroup()

g2 <- ggplot(courbe_age, aes(x = AGE, y = Pct, color = Y_OFFRE)) +
  geom_smooth(method = "loess", span = 0.4, se = TRUE, alpha = 0.15, linewidth = 1.1) +
  scale_color_manual(values = palette_y) +
  scale_fill_manual(values  = palette_y) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(25, 54, by = 5)) +
  labs(
    title    = "Graphique 2 — Profil de l'offre de travail selon l'âge",
    subtitle = "Proportions pondérées lissées (LOESS) — Femmes en couple, 25-54 ans, EEC 2024",
    x = "Âge", y = "Part pondérée (%)",
    caption  = "Note : bandes de confiance à 95 %. Source : EEC INSEE 2024. Calculs de l'auteur."
  ) +
  theme_memoire

ggsave(file.path(dir_out, "graphique2_courbe_age.png"), g2, width = 8, height = 5, dpi = 300)


# ==============================================================================
# GRAPHIQUE 3 — Distribution de Y selon NB_ENF_0_2
# ==============================================================================

distrib_enf02 <- df %>%
  mutate(NB_ENF_0_2_f = factor(NB_ENF_0_2, labels = c("0", "1", "2", "3+"))) %>%
  group_by(NB_ENF_0_2_f, Y_OFFRE) %>%
  summarise(N_pond = sum(POIDS), .groups = "drop") %>%
  group_by(NB_ENF_0_2_f) %>%
  mutate(Pct = N_pond / sum(N_pond))

g3 <- ggplot(distrib_enf02, aes(x = NB_ENF_0_2_f, y = Pct, fill = Y_OFFRE)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(
    aes(label = ifelse(Pct > 0.04, paste0(round(Pct * 100, 1), "%"), "")),
    position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = palette_y) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Graphique 3 — Offre de travail selon le nombre d'enfants de 0-2 ans",
    subtitle = "Proportions pondérées — EEC 2024",
    x = "Nombre d'enfants de 0 à 2 ans", y = "Part pondérée (%)"
  ) +
  theme_memoire

ggsave(file.path(dir_out, "graphique3_Y_par_enf02.png"), g3, width = 7, height = 5, dpi = 300)


# ==============================================================================
# GRAPHIQUE 4 — Distribution de Y selon DIPLOME
# ==============================================================================

distrib_dip <- df %>%
  group_by(DIPLOME, Y_OFFRE) %>%
  summarise(N_pond = sum(POIDS), .groups = "drop") %>%
  group_by(DIPLOME) %>%
  mutate(Pct = N_pond / sum(N_pond))

g4 <- ggplot(distrib_dip, aes(x = DIPLOME, y = Pct, fill = Y_OFFRE)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(
    aes(label = ifelse(Pct > 0.04, paste0(round(Pct * 100, 1), "%"), "")),
    position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = palette_y) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels = c("Aucun/\nCEP", "CAP/\nBEP", "Bac", "Sup\nCourt", "Sup\nLong")) +
  labs(
    title    = "Graphique 4 — Offre de travail selon le niveau de diplôme",
    subtitle = "Proportions pondérées — EEC 2024",
    x = "Niveau de diplôme", y = "Part pondérée (%)"
  ) +
  theme_memoire

ggsave(file.path(dir_out, "graphique4_Y_par_diplome.png"), g4, width = 7, height = 5, dpi = 300)


# ==============================================================================
# GRAPHIQUE 5 — Distribution de Y selon CSP_CONJOINT
# ==============================================================================

ordre_csp  <- c("SansEmploi", "Ouvrier/Employe", "Independant", "Intermediaire", "Cadre/Sup")
labels_csp <- c("Sans emploi", "Ouvrier/\nEmployé (réf.)", "Indépendant", "Intermédiaire", "Cadre/\nSup.")

distrib_csp <- df %>%
  mutate(CSP_CONJOINT = factor(CSP_CONJOINT, levels = ordre_csp)) %>%
  group_by(CSP_CONJOINT, Y_OFFRE) %>%
  summarise(N_pond = sum(POIDS), .groups = "drop") %>%
  group_by(CSP_CONJOINT) %>%
  mutate(Pct = N_pond / sum(N_pond))

g5 <- ggplot(distrib_csp, aes(x = CSP_CONJOINT, y = Pct, fill = Y_OFFRE)) +
  geom_col(position = "stack", width = 0.65) +
  geom_text(
    aes(label = ifelse(Pct > 0.04, paste0(round(Pct * 100, 1), "%"), "")),
    position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = palette_y) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels = labels_csp) +
  labs(
    title    = "Graphique 5 — Offre de travail selon la CSP du conjoint",
    subtitle = "Proportions pondérées — EEC 2024",
    x = "CSP du conjoint", y = "Part pondérée (%)"
  ) +
  theme_memoire

ggsave(file.path(dir_out, "graphique5_Y_par_CSP.png"), g5, width = 7, height = 5, dpi = 300)
