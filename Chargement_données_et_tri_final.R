# ==============================================================================
# Offre de travail des femmes en couple — Construction de la base (EEC 2024)
# M2 TIDE — Logit Ordonné
# ==============================================================================

library(tidyverse)

setwd("C:/Users/alexi/Documents/Université/M2 TIDE/Etude Temps de travail des femmes")

df_global <- readRDS("base_annuelle_brute.rds")


# ==============================================================================
# 1. TABLE DES TAUX DE CHÔMAGE RÉGIONAUX (moyenne annuelle 2024)
# Source : INSEE, estimations de taux de chômage localisés, données CVS
# ==============================================================================

taux_chomage_raw <- read.csv(
  "taux_chomage_region.csv",
  header           = TRUE,
  skip             = 3,
  stringsAsFactors = FALSE,
  check.names      = FALSE
)

# On ne garde que les 12 régions de métropole hors Corse
codes_exclus <- c("00", "94", "01", "02", "03", "04")

taux_chomage <- taux_chomage_raw %>%
  filter(!is.na(Code), !Code %in% codes_exclus, nchar(trimws(Code)) == 2) %>%
  mutate(across(c(T1_2024, T2_2024, T3_2024, T4_2024), as.numeric)) %>%
  mutate(
    TX_CHOMAGE = rowMeans(select(., T1_2024, T2_2024, T3_2024, T4_2024), na.rm = TRUE),
    REGIO      = as.integer(trimws(as.character(Code)))
  ) %>%
  select(REGIO, TX_CHOMAGE)

# Pays de la Loire : code région 6 dans l'EEC (au lieu de 52 dans la nomenclature actuelle)
taux_chomage <- taux_chomage %>%
  bind_rows(
    taux_chomage %>% filter(REGIO == 52) %>% mutate(REGIO = 6L)
  )


# ==============================================================================
# 2. SÉLECTION ET FILTRES
# ==============================================================================

# On extrait uniquement les variables nécessaires pour alléger le dataframe de travail
df_analyse <- df_global %>%
  select(
    IDENT, NOI, EXTRI,
    SEXE, AGE, COUPL_LOG,
    ACTEU, TPPRED,
    NBENFLOG_A, NBENFLOG_B, NBENFLOG_C,  # nb enfants par tranche d'âge (0-2, 3-5, 6+)
    DIP5, IMMI,
    REGIO,
    LPRL, STAT_CS_BITPRL, STAT_CS_BITPRLCJ  # nécessaires pour récupérer la CSP du conjoint
  ) %>%
  filter(
    SEXE      == 2,          # femmes
    COUPL_LOG == 1,          # en couple (marié ou PACS)
    AGE >= 25 & AGE <= 54,
    !REGIO %in% c(1, 2, 3, 4, 94)  # exclusion DROM (codes 1-4) et Corse (94)
  )


# ==============================================================================
# 3. CONSTRUCTION DES VARIABLES
# ==============================================================================

df_final <- df_analyse %>%
  mutate(
    
    # --- Variable dépendante ---
    # ACTEU : statut d'activité (1 = emploi, 2 = chômage, 3 = inactivité)
    # TPPRED : temps de travail (1 = temps plein, 2 = temps partiel)
    # Chômeuses et inactives sont regroupées en modalité 0 : toutes deux sont
    # hors emploi, ce qui correspond à l'objet d'étude (intensité de participation).
    Y_OFFRE = case_when(
      ACTEU %in% c(2, 3)       ~ 0,  # hors emploi
      ACTEU == 1 & TPPRED == 2 ~ 1,  # temps partiel
      ACTEU == 1 & TPPRED == 1 ~ 2,  # temps plein
      TRUE                     ~ NA_real_
    ),
    
    # --- Variables enfants ---
    # NBENFLOG_A/B/C = nb d'enfants du ménage par tranche d'âge EEC.
    # Plafonné à 3 pour limiter le poids des configurations très rares (4 enfants+).
    NB_ENF_0_2   = pmin(NBENFLOG_A, 3),  # 0-2 ans
    NB_ENF_3_5   = pmin(NBENFLOG_B, 3),  # 3-5 ans
    NB_ENF_6PLUS = pmin(NBENFLOG_C, 3),  # 6 ans et +
    
    AGE_CARRE = AGE^2,  # terme quadratique pour capter l'effet non-linéaire de l'âge
    
    # --- CSP du conjoint ---
    # Dans l'EEC, chaque ménage a une personne de référence (PR).
    # La femme peut être soit la PR (LPRL == 1), soit le conjoint de la PR (LPRL == 2).
    # Selon ce rôle, l'information sur le conjoint se trouve dans une colonne différente.
    STAT_CS_BITPRL   = as.character(STAT_CS_BITPRL),
    STAT_CS_BITPRLCJ = as.character(STAT_CS_BITPRLCJ),
    
    CODE_BRUT_MARI = case_when(
      LPRL == 1 ~ STAT_CS_BITPRLCJ,  # femme est PR → conjoint dans PRLCJ
      LPRL == 2 ~ STAT_CS_BITPRL,    # femme est conjoint → mari est la PR
      TRUE      ~ NA_character_
    ),
    
    # Codes EEC : 1XX = en emploi (XX = CS à 2 chiffres), 2XX = chômeur ayant travaillé,
    # 3XX = inactif ayant travaillé, 200/300 = jamais travaillé, 999 = non classé.
    CSP_CONJOINT = case_when(
      str_detect(CODE_BRUT_MARI, "^[12]3")    ~ "Cadre/Sup",
      str_detect(CODE_BRUT_MARI, "^[12]4")    ~ "Intermediaire",
      str_detect(CODE_BRUT_MARI, "^[12][12]") ~ "Independant",
      str_detect(CODE_BRUT_MARI, "^[12][56]") ~ "Ouvrier/Employe",
      str_detect(CODE_BRUT_MARI, "^[23]")     ~ "SansEmploi",
      CODE_BRUT_MARI == "999"                 ~ NA_character_,
      TRUE                                    ~ NA_character_
    ),
    
    # --- Autres variables de contrôle ---
    # DIP5 : diplôme le plus élevé obtenu, codé de 5 (aucun) à 1 (bac+5 et plus)
    DIPLOME = factor(
      DIP5,
      levels  = c(5, 4, 3, 2, 1),
      labels  = c("Aucun/CEP", "CAP/BEP", "Bac", "Sup Court", "Sup Long"),
      ordered = TRUE
    ),
    
    # IMMI == 1 : née étrangère à l'étranger (définition INSEE de l'immigrée)
    IMMIGREE = factor(if_else(IMMI == 1, 1, 0), levels = c(0, 1), labels = c("Non", "Oui")),
    
    # EXTRI : poids de sondage trimestriel. Divisé par 4 car la base est annuelle
    # (4 trimestres empilés), afin de ramener les effectifs pondérés à leur niveau réel.
    POIDS = EXTRI / 4
    
  ) %>%
  
  # Ajout du taux de chômage régional 2024 via jointure sur le code région
  left_join(taux_chomage, by = "REGIO") %>%
  
  mutate(
    Y_OFFRE      = factor(Y_OFFRE, levels = c(0, 1, 2),
                          labels = c("Non-Emploi", "Temps Partiel", "Temps Plein"),
                          ordered = TRUE),
    CSP_CONJOINT = factor(CSP_CONJOINT),
    CSP_CONJOINT = relevel(CSP_CONJOINT, ref = "Ouvrier/Employe")  # modalité de référence
  )


# ==============================================================================
# 4. NETTOYAGE ET SÉLECTION FINALE
# ==============================================================================

vars_modele <- c(
  "Y_OFFRE", "AGE", "AGE_CARRE", "DIPLOME",
  "NB_ENF_0_2", "NB_ENF_3_5", "NB_ENF_6PLUS",
  "CSP_CONJOINT", "IMMIGREE", "TX_CHOMAGE", "POIDS"
)

# Suppression des observations avec au moins un NA sur les variables du modèle
df_final <- df_final %>%
  drop_na(all_of(vars_modele)) %>%
  select(all_of(vars_modele))

cat("\nDimensions :", nrow(df_final), "obs. /", ncol(df_final), "variables\n")
cat("\nDistribution Y_OFFRE :\n")
print(prop.table(table(df_final$Y_OFFRE)))
cat("\nCSP_CONJOINT :\n")
print(table(df_final$CSP_CONJOINT))
cat("\nTX_CHOMAGE :\n")
print(summary(df_final$TX_CHOMAGE))


# ==============================================================================
# 5. SAUVEGARDE
# ==============================================================================

saveRDS(df_final, "base_modele_clean.rds")
write.csv(df_final, "base_modele_clean.csv", row.names = FALSE)
