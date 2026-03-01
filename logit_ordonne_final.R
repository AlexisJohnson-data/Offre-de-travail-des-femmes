# ==============================================================================
# Partie IV — Modèle Logit Ordonné
# (1) Estimation principale — polr() de {MASS}
# (2) Effets marginaux moyens (AME) — marginaleffects
# (3) Test de Brant — brant()
# (4) Robustesse : sans chômeuses / probit ordonné
# ==============================================================================

library(MASS)             # chargé avant tidyverse pour éviter les conflits de masquage
library(brant)            # idem
library(tidyverse)        # tidyverse en dernier : ses verbes (select, filter, rename) prennent le dessus
library(sandwich)
library(lmtest)
library(marginaleffects)
library(scales)

setwd("C:/Users/alexi/Documents/Université/M2 TIDE/Etude Temps de travail des femmes")

dir_out <- "C:/Users/alexi/Documents/Université/M2 TIDE/Etude Temps de travail des femmes/graphiques"
dir.create(dir_out, showWarnings = FALSE)

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

df <- readRDS("base_modele_clean.rds")

# Normalisation des poids : ramène la somme à N observations.
# polr() interprète weights comme des fréquences — sans normalisation,
# la log-vraisemblance est gonflée et les critères d'information (AIC/BIC) non comparables.
df <- df %>% dplyr::mutate(POIDS_norm = POIDS / sum(POIDS) * n())

cat("Dimensions :", nrow(df), "obs.,", ncol(df), "variables\n")
cat("Distribution Y_OFFRE :\n")
print(prop.table(table(df$Y_OFFRE)))


# ==============================================================================
# 1. MODELE PRINCIPAL
# ==============================================================================

formule <- Y_OFFRE ~ AGE + AGE_CARRE + DIPLOME +
  NB_ENF_0_2 + NB_ENF_3_5 + NB_ENF_6PLUS +
  CSP_CONJOINT + IMMIGREE + TX_CHOMAGE

modele <- polr(formule, data = df, weights = POIDS_norm,
               Hess = TRUE, method = "logistic")

cat("\n--- Résumé du modèle principal ---\n")
print(summary(modele))

# Calcul des erreurs standard robustes (HC3)
# En cas d'échec de vcovHC, bascule sur l'estimateur sandwich puis sur la vcov classique
vcov_rob <- tryCatch(
  vcovHC(modele, type = "HC3"),
  error = function(e) {
    tryCatch(sandwich(modele),
             error = function(e2) vcov(modele))
  }
)
coef_rob <- coeftest(modele, vcov = vcov_rob)

cat("\n--- Coefficients avec ES robustes ---\n")
print(coef_rob)

# Tableau 4 — coefficients, odds-ratios et intervalles de confiance robustes
coef_df <- as.data.frame(coef_rob[, 1:4])
colnames(coef_df) <- c("Coefficient", "ES_robuste", "z", "p_valeur")
coef_df$Variable  <- rownames(coef_df)

tableau4 <- coef_df %>%
  dplyr::mutate(
    OR     = ifelse(!grepl("\\|", Variable), round(exp(Coefficient), 3), NA),
    IC_inf = ifelse(!grepl("\\|", Variable), round(exp(Coefficient - 1.96 * ES_robuste), 3), NA),
    IC_sup = ifelse(!grepl("\\|", Variable), round(exp(Coefficient + 1.96 * ES_robuste), 3), NA),
    Coefficient = round(Coefficient, 4),
    ES_robuste  = round(ES_robuste, 4),
    z           = round(z, 3),
    p_valeur    = round(p_valeur, 4)
  ) %>%
  dplyr::select(Variable, Coefficient, ES_robuste, z, p_valeur, OR, IC_inf, IC_sup)

# Indicateurs d'ajustement
modele_nul <- polr(Y_OFFRE ~ 1, data = df, weights = POIDS_norm,
                   Hess = TRUE, method = "logistic")
pseudoR2 <- round(1 - as.numeric(logLik(modele)) / as.numeric(logLik(modele_nul)), 4)
aic_val  <- round(AIC(modele), 2)
bic_val  <- round(BIC(modele), 2)

cat("\nPseudo-R2 McFadden :", pseudoR2, "| AIC :", aic_val, "| BIC :", bic_val, "\n")

goodness <- tibble::tibble(
  Indicateur = c("Pseudo-R2 McFadden", "AIC", "BIC", "N observations"),
  Valeur     = c(pseudoR2, aic_val, bic_val, nrow(df))
)

write.csv(tableau4, file.path(dir_out, "tableau4_coefficients.csv"),    row.names = FALSE)
write.csv(goodness, file.path(dir_out, "tableau4_goodness_of_fit.csv"), row.names = FALSE)


# ==============================================================================
# 2. EFFETS MARGINAUX MOYENS (AME) — Tableau 5 + Graphiques 6 et 7
# ==============================================================================

vars_modele <- c("AGE", "AGE_CARRE", "DIPLOME",
                 "NB_ENF_0_2", "NB_ENF_3_5", "NB_ENF_6PLUS",
                 "CSP_CONJOINT", "IMMIGREE", "TX_CHOMAGE")

# La matrice vcov_rob est passée explicitement à avg_slopes() pour obtenir
# des intervalles de confiance cohérents avec les ES robustes du modèle principal.
ame <- avg_slopes(modele, variables = vars_modele, vcov = vcov_rob)

tableau5 <- ame %>%
  tibble::as_tibble() %>%
  dplyr::select(term, contrast, group, estimate, std.error, conf.low, conf.high, p.value) %>%
  dplyr::rename(Variable = term, Contraste = contrast, Modalite_Y = group,
                AME = estimate, ES = std.error, IC_inf = conf.low,
                IC_sup = conf.high, p_valeur = p.value) %>%
  dplyr::mutate(dplyr::across(c(AME, ES, IC_inf, IC_sup), ~ round(.x, 4)),
                p_valeur = round(p_valeur, 4))

write.csv(tableau5, file.path(dir_out, "tableau5_AME.csv"), row.names = FALSE)
cat("\n--- AME aperçu ---\n")
print(tableau5)

# --- Graphique 6 : AME des variables enfants ---
vars_enfants   <- c("NB_ENF_0_2", "NB_ENF_3_5", "NB_ENF_6PLUS")
labels_enfants <- c("NB_ENF_0_2" = "Enfants 0-2 ans",
                    "NB_ENF_3_5" = "Enfants 3-5 ans",
                    "NB_ENF_6PLUS" = "Enfants 6 ans et +")

ame_enfants <- tableau5 %>%
  dplyr::filter(Variable %in% vars_enfants) %>%
  dplyr::mutate(Variable   = factor(Variable, levels = rev(vars_enfants), labels = rev(labels_enfants)),
                Modalite_Y = factor(Modalite_Y, levels = c("Non-Emploi", "Temps Partiel", "Temps Plein")))

g6 <- ggplot(ame_enfants, aes(x = AME, y = Variable, color = Modalite_Y)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup),
                 position = position_dodge(width = 0.5), height = 0.2, linewidth = 0.7) +
  scale_color_manual(values = palette_y) +
  labs(title    = "Graphique 6 — Effets marginaux moyens des variables enfants",
       subtitle = "AME sur P(Y=0), P(Y=1), P(Y=2) — IC à 95% par delta-method",
       x = "Effet marginal moyen", y = NULL,
       caption = "Source : EEC INSEE 2024. Calculs de l'auteur.") +
  facet_wrap(~ Modalite_Y, nrow = 1) +
  theme_memoire + theme(legend.position = "none")

ggsave(file.path(dir_out, "graphique6_AME_enfants.png"), g6, width = 10, height = 4, dpi = 300)

# --- Graphique 7 : AME de CSP_CONJOINT ---
ame_csp <- tableau5 %>%
  dplyr::filter(Variable == "CSP_CONJOINT") %>%
  dplyr::mutate(Contraste  = str_remove(Contraste, " - Ouvrier/Employe"),
                Contraste  = factor(Contraste, levels = c("SansEmploi", "Independant",
                                                          "Intermediaire", "Cadre/Sup")),
                Modalite_Y = factor(Modalite_Y, levels = c("Non-Emploi", "Temps Partiel", "Temps Plein")))

g7 <- ggplot(ame_csp, aes(x = AME, y = Contraste, color = Modalite_Y)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup),
                 position = position_dodge(width = 0.5), height = 0.2, linewidth = 0.7) +
  scale_color_manual(values = palette_y) +
  labs(title    = "Graphique 7 — Effets marginaux moyens de la CSP du conjoint",
       subtitle = "AME sur P(Y=0), P(Y=1), P(Y=2) — réf. : Ouvrier/Employé — IC à 95%",
       x = "Effet marginal moyen", y = NULL,
       caption = "Source : EEC INSEE 2024. Calculs de l'auteur.") +
  facet_wrap(~ Modalite_Y, nrow = 1) +
  theme_memoire + theme(legend.position = "none")

ggsave(file.path(dir_out, "graphique7_AME_CSP.png"), g7, width = 10, height = 4, dpi = 300)


# ==============================================================================
# 3. TEST DE BRANT — Tableau 6
# Vérifie l'hypothèse de pentes parallèles variable par variable
# ==============================================================================

cat("\n--- Test de Brant (pentes parallèles) ---\n")
brant_result <- brant(modele)
print(brant_result)

tableau6 <- as.data.frame(brant_result) %>%
  rownames_to_column("Variable") %>%
  dplyr::rename(Chi2 = X2, p_valeur = probability) %>%
  dplyr::mutate(Chi2         = round(Chi2, 3),
                p_valeur     = round(p_valeur, 4),
                Significatif = ifelse(p_valeur < 0.05, "Oui *", "Non"))

write.csv(tableau6, file.path(dir_out, "tableau6_brant.csv"), row.names = FALSE)


# ==============================================================================
# 4. TESTS DE ROBUSTESSE — Tableau 7
# Spécification (1) : exclusion des chômeuses (Y=0 = inactives uniquement)
# Spécification (2) : probit ordonné sur la base principale
# ==============================================================================

# --- Robustesse 1 : sans chômeuses ---
# La base df ne conserve pas les identifiants individuels. On reconstruit
# l'échantillon depuis df_brut en appliquant les mêmes recodages que dans
# le script de construction, avec le filtre ACTEU != 2 en sus.
df_brut <- readRDS("base_annuelle_brute.rds")

taux_chomage_raw <- read.csv(
  "taux_chomage_region.csv",
  header = TRUE, skip = 3,
  stringsAsFactors = FALSE, check.names = FALSE
)
taux_chomage_rob <- taux_chomage_raw %>%
  dplyr::filter(!is.na(Code), !Code %in% c("00","94","01","02","03","04"),
                nchar(trimws(Code)) == 2) %>%
  dplyr::mutate(dplyr::across(c(T1_2024, T2_2024, T3_2024, T4_2024), as.numeric),
                TX_CHOMAGE = rowMeans(dplyr::select(., T1_2024, T2_2024, T3_2024, T4_2024), na.rm = TRUE),
                REGIO      = as.integer(trimws(as.character(Code)))) %>%
  dplyr::select(REGIO, TX_CHOMAGE) %>%
  dplyr::bind_rows(dplyr::filter(., REGIO == 52) %>% dplyr::mutate(REGIO = 6L))

df_rob <- df_brut %>%
  dplyr::filter(
    SEXE == 2, COUPL_LOG == 1,
    AGE >= 25, AGE <= 54,
    !REGIO %in% c(1, 2, 3, 4, 94),
    ACTEU != 2   # exclusion des chômeuses : Y=0 = inactives uniquement
  ) %>%
  dplyr::mutate(
    Y_rob = dplyr::case_when(
      ACTEU == 3                ~ 0L,
      ACTEU == 1 & TPPRED == 2 ~ 1L,
      ACTEU == 1 & TPPRED == 1 ~ 2L
    ),
    Y_rob = factor(Y_rob, levels = c(0L, 1L, 2L),
                   labels = c("Non-Emploi", "Temps Partiel", "Temps Plein"),
                   ordered = TRUE),
    NB_ENF_0_2   = pmin(NBENFLOG_A, 3),
    NB_ENF_3_5   = pmin(NBENFLOG_B, 3),
    NB_ENF_6PLUS = pmin(NBENFLOG_C, 3),
    AGE_CARRE    = AGE^2,
    DIPLOME = factor(DIP5, levels = c(5, 4, 3, 2, 1),
                     labels = c("Aucun/CEP", "CAP/BEP", "Bac", "Sup Court", "Sup Long"),
                     ordered = TRUE),
    IMMIGREE = factor(dplyr::if_else(IMMI == 1, 1, 0),
                      levels = c(0, 1), labels = c("Non", "Oui")),
    STAT_CS_BITPRL   = as.character(STAT_CS_BITPRL),
    STAT_CS_BITPRLCJ = as.character(STAT_CS_BITPRLCJ),
    CODE_BRUT_MARI = dplyr::case_when(
      LPRL == 1 ~ STAT_CS_BITPRLCJ,
      LPRL == 2 ~ STAT_CS_BITPRL,
      TRUE      ~ NA_character_
    ),
    CSP_CONJOINT = dplyr::case_when(
      str_detect(CODE_BRUT_MARI, "^[12]3")    ~ "Cadre/Sup",
      str_detect(CODE_BRUT_MARI, "^[12]4")    ~ "Intermediaire",
      str_detect(CODE_BRUT_MARI, "^[12][12]") ~ "Independant",
      str_detect(CODE_BRUT_MARI, "^[12][56]") ~ "Ouvrier/Employe",
      str_detect(CODE_BRUT_MARI, "^[23]")     ~ "SansEmploi",
      TRUE                                    ~ NA_character_
    ),
    CSP_CONJOINT = factor(CSP_CONJOINT),
    CSP_CONJOINT = relevel(CSP_CONJOINT, ref = "Ouvrier/Employe"),
    POIDS        = EXTRI / 4,
    POIDS_norm   = POIDS / sum(POIDS, na.rm = TRUE) * n()
  ) %>%
  dplyr::left_join(taux_chomage_rob, by = "REGIO") %>%
  drop_na(Y_rob, AGE, AGE_CARRE, DIPLOME,
          NB_ENF_0_2, NB_ENF_3_5, NB_ENF_6PLUS,
          CSP_CONJOINT, IMMIGREE, TX_CHOMAGE, POIDS_norm)

cat("\nDimensions base robustesse (sans chômeuses) :", nrow(df_rob), "obs.\n")

formule_rob <- Y_rob ~ AGE + AGE_CARRE + DIPLOME +
  NB_ENF_0_2 + NB_ENF_3_5 + NB_ENF_6PLUS +
  CSP_CONJOINT + IMMIGREE + TX_CHOMAGE

modele_rob1 <- polr(formule_rob, data = df_rob, weights = POIDS_norm,
                    Hess = TRUE, method = "logistic")
vcov_rob1 <- tryCatch(
  vcovHC(modele_rob1, type = "HC3"),
  error = function(e) { sandwich(modele_rob1) }
)
ame_rob1 <- avg_slopes(modele_rob1, variables = vars_modele, vcov = vcov_rob1)

# --- Robustesse 2 : probit ordonné ---
modele_rob2 <- polr(formule, data = df, weights = POIDS_norm,
                    Hess = TRUE, method = "probit")
vcov_rob2 <- tryCatch(
  vcovHC(modele_rob2, type = "HC3"),
  error = function(e) { sandwich(modele_rob2) }
)
ame_rob2 <- avg_slopes(modele_rob2, variables = vars_modele, vcov = vcov_rob2)

# --- Tableau 7 : comparaison des AME sur P(Y = Temps Plein) ---
vars_cles <- c("NB_ENF_0_2", "NB_ENF_3_5", "NB_ENF_6PLUS", "CSP_CONJOINT", "DIPLOME")

extraire_ame_y2 <- function(ame_obj, nom_modele) {
  ame_obj %>%
    tibble::as_tibble() %>%
    dplyr::filter(group == "Temps Plein", term %in% vars_cles) %>%
    dplyr::select(term, contrast, estimate, conf.low, conf.high) %>%
    dplyr::mutate(Modele = nom_modele,
                  estimate  = round(estimate, 4),
                  conf.low  = round(conf.low, 4),
                  conf.high = round(conf.high, 4)) %>%
    dplyr::rename(Variable = term, Contraste = contrast,
                  AME = estimate, IC_inf = conf.low, IC_sup = conf.high)
}

tableau7 <- dplyr::bind_rows(
  extraire_ame_y2(ame,      "Logit ordonne (base)"),
  extraire_ame_y2(ame_rob1, "Logit ordonne sans chomeuses"),
  extraire_ame_y2(ame_rob2, "Probit ordonne")
) %>% dplyr::arrange(Variable, Contraste, Modele)

write.csv(tableau7, file.path(dir_out, "tableau7_robustesse.csv"), row.names = FALSE)
cat("\n--- Tableau 7 : AME sur P(Y=Temps Plein) ---\n")
print(tableau7)
