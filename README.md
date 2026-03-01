# Offre de travail des femmes en couple — Logit Ordonné sur l'EEC 2024

> **Mémoire de M2 TIDE** — Université · 2024–2025
> **Auteurs :** Romaric Essessinou & Alexis Johnson

---

## 🎯 Présentation du projet

Ce projet analyse les **déterminants de l'offre de travail des femmes en couple** en France métropolitaine, à partir des données de l'**Enquête Emploi en Continu (EEC) 2024** de l'INSEE.

La question centrale : quels facteurs — enfants, diplôme, CSP du conjoint, territoire — influencent le fait qu'une femme en couple soit en emploi à temps plein, en emploi à temps partiel, ou hors emploi ?

---

## 📊 Données

- **Source :** Enquête Emploi en Continu (EEC) — INSEE, vague annuelle 2024
- **Population ciblée :** Femmes en couple (marié·e ou pacsé·e), âgées de 25 à 54 ans, résidant en France métropolitaine hors Corse et DROM
- **Taille de l'échantillon final :** plusieurs dizaines de milliers d'observations pondérées représentant environ 6 millions de femmes
- **Taux de chômage régionaux :** appariés depuis les estimations localisées CVS de l'INSEE (moyenne des 4 trimestres 2024)

> ⚠️ Les données brutes EEC ne sont pas incluses dans ce dépôt (accès restreint INSEE). Seuls les scripts de traitement et le mémoire final sont mis à disposition.

---

## 🔬 Méthodologie

### Variable dépendante ordonnée
L'offre de travail est modélisée comme une variable à **trois modalités ordonnées** :

| Modalité | Définition |
|---|---|
| `0 — Non-Emploi` | Femme au chômage ou inactive |
| `1 — Temps Partiel` | Femme en emploi à temps partiel |
| `2 — Temps Plein` | Femme en emploi à temps plein |

### Modèle
**Logit ordonné** (ordered logit / proportional odds model) estimé par maximum de vraisemblance, avec :
- Poids de sondage normalisés (EXTRI / 4)
- Erreurs standard robustes (HC3, sandwich)
- **Effets marginaux moyens (AME)** calculés via le package `marginaleffects`
- **Test de Brant** pour vérifier l'hypothèse de pentes parallèles
- Tests de robustesse : exclusion des chômeuses · probit ordonné

### Variables explicatives
| Variable | Description |
|---|---|
| `AGE`, `AGE²` | Effet non-linéaire de l'âge |
| `NB_ENF_0_2` | Nombre d'enfants de 0 à 2 ans (plafonné à 3) |
| `NB_ENF_3_5` | Nombre d'enfants de 3 à 5 ans |
| `NB_ENF_6PLUS` | Nombre d'enfants de 6 ans et plus |
| `DIPLOME` | Niveau de diplôme (5 modalités ordonnées) |
| `CSP_CONJOINT` | Catégorie socio-professionnelle du conjoint (réf. : Ouvrier/Employé) |
| `IMMIGREE` | Statut d'immigrée (définition INSEE) |
| `TX_CHOMAGE` | Taux de chômage régional moyen 2024 |

---

## 📁 Structure du dépôt

```
.
├── Chargement_données_et_tri_final.R      # Étape 1 — Construction de la base de travail
├── stats_desc_complet_final.R             # Étape 2 — Statistiques descriptives & graphiques 1 à 5
├── logit_ordonne_final.R                  # Étape 3 — Estimation, AME, test de Brant, robustesse
└── Mémoire_Microéconométrie_ESSESSINOU_JOHNSON.pdf   # Mémoire complet
```

### Ordre d'exécution des scripts

```
1. Chargement_données_et_tri_final.R
      → produit : base_modele_clean.rds / .csv

2. stats_desc_complet_final.R
      → produit : graphiques 1–5, tableaux 1–3 (dans /graphiques/)

3. logit_ordonne_final.R
      → produit : tableaux 4–7, graphiques 6–7 (dans /graphiques/)
```

---

## 🛠️ Environnement technique

**Langage :** R (≥ 4.3)

**Packages principaux :**

| Package | Usage |
|---|---|
| `tidyverse` | Manipulation des données, visualisation (ggplot2) |
| `MASS` | Estimation du modèle logit/probit ordonné (`polr`) |
| `marginaleffects` | Calcul des effets marginaux moyens (AME) |
| `sandwich` / `lmtest` | Erreurs standard robustes (HC3) |
| `brant` | Test des pentes parallèles |
| `weights` | Corrélations pondérées |
| `scales` | Mise en forme des axes graphiques |

---

## 📈 Principaux résultats

- La présence d'**enfants en bas âge (0–2 ans)** réduit fortement la probabilité d'emploi à temps plein et augmente celle du non-emploi — effet le plus marqué de l'étude.
- Le **diplôme** joue un rôle protecteur fort : les femmes les plus diplômées sont nettement plus présentes en emploi à temps plein.
- La **CSP du conjoint** a un effet ambigu : les conjoints cadres sont associés à une moindre participation des femmes, ce qui peut refléter un revenu du ménage suffisant (income effect).
- Le **taux de chômage régional** agit négativement sur l'offre de travail des femmes, conformément aux prédictions théoriques du modèle de recherche d'emploi.

Pour l'analyse complète, les tableaux de résultats et la discussion, voir le **[mémoire PDF](./Mémoire_Microéconométrie_ESSESSINOU_JOHNSON.pdf)**.

---

## 👥 Travail d'équipe

Ce projet a été mené à **deux**, dans le cadre du Master 2 TIDE. La collaboration a été au cœur de la démarche : choix conjoint de la problématique, construction commune de la base de données, discussions régulières sur les choix méthodologiques (traitement des chômeuses, modalité de référence de la CSP, normalisation des poids), et rédaction partagée du mémoire.

Travailler en binôme sur un projet de cette ampleur a permis d'aller plus loin — notamment dans la partie robustesse et dans la rigueur des commentaires de code — qu'un travail individuel ne l'aurait permis.

---

## 📬 Contact

- **Alexis Johnson** — [alexis.travail1@gmail.com](mailto:alexis.travail1@gmail.com) · [GitHub](https://github.com/AlexisJohnson-data)
- **Romaric Essessinou** — M2 TIDE
