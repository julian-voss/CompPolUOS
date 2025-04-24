library(tidyverse)
library(haven)
library(sjlabelled)

## Data management -------------------------------------------------------------

politbaro <-
  haven::read_dta("./data-raw/politbarometer/ZA2391_v15-2-0.dta", encoding = "utf-8") %>%
  # Rename
  rename(
    studiennummer = v1,
    befragtennummer = v2,
    erhebungsmonat = v3,
    erhebungsjahr = v4,
    erhebungsgebiet = V4a,
    wahlbeteiligung_absicht = v5,
    parteienwahl_absicht = v6,
    wahl_rueckerinnerung = v7,
    skalom_spd = v8,
    skalom_cdu = v9,
    skalom_csu = v10,
    skalom_fdp = v11,
    skalom_gruene = v12,
    skalom_republikaner = v13,
    skalom_pds = v14,
    skalom_regierung = v15,
    skalom_opposition = v16,
    skalom_gruene_opposition = v17,
    demokratiezufriedenheit = v18,
    vertrauen_fuehrungspersonal = v19,
    politikinteresse = v20,
    politikinteresse_staerke = v21,
    links_rechts_selbsteinstufung = v22,
    linksorientierung = v23,
    rechtsorientierung = v24,
    beurteilung_wirtschaft_brd = v25,
    wirtschaftslage_brd_1jahr = v26,
    eigene_wirtschaftslage = v27,
    eigene_wirtschaftslage_1jahr = v28,
    beurteilung_wiedervereinigung = v29,
    beurteilung_asylrecht = v30,
    einstellung_auslaender = v31,
    beurteilung_abtreibung = v32,
    wichtigstes_problem_1 = v33,
    wichtigstes_problem_2 = v34,
    kompetenz_arbeitsmarkt = v35,
    kompetenz_wirtschaft = v36,
    kompetenz_wirtschaft_ost = v37,
    kompetenz_umweltschutz = v38,
    einstellung_kernkraft = v39,
    kompetenz_renten = v40,
    bedrohung_kriminalitaet = v41,
    haltung_eg_mitgliedschaft = v42,
    einheit_vs_verantwortung = v43,
    zustand_gesellschaft = v44,
    gesellschaft_im_vergleich = v45,
    spd_parteivorsitz_unterstuetzung = v46,
    cdu_parteivorsitz_unterstuetzung = v47,
    subjektives_bedrohungsgefuehl = v48,
    frieden_sicherer = v49,
    jahresrueckblick = v50,
    jahresausblick = v51,
    konfession = v52,
    kirchgang = v53,
    geschlecht = v54,
    alter_jahre = v55,
    alter_kategorien = v56,
    familienstand = v57,
    zusammenleben_partner = v58,
    schulabschluss_9kat = v59,
    schulabschluss_5kat = v60,
    berufsausbildung_abgeschlossen = v61,
    staatsexamen_oder_aequivalent = v62,
    angestrebter_schulabschluss = v63,
    berufstaetigkeit = v64,
    berufsgruppe = v65,
    anzahl_personen_haushalt = v66,
    personen_ab_18_im_haushalt = v67,
    haushaltsvorstand = v68,
    hhv_berufstaetig = v69,
    hhv_berufsgruppe = v70,
    arbeitsplatz_gefaehrdet = v71,
    parteineigung = v72,
    staerke_parteineigung = v73,
    gewerkschaftsmitglied_haushalt = v74,
    bundesland = v75,
    ortsgroesse = v76,
    gemeindegroesse = v77,
    gewicht_faktor = v78,
    versionskennung = v79,
    erhebungswoche = v80,
    version = V81)

recode_missing_labels <- function(x) {
  na_labels <- c("nicht erhoben", "weiß nicht", "KA", "KA, verweigert", "KA, TNZ", "TNZ", "TNZ/nicht erhoben",
                 "kein weiteres Problem")
  labels <- get_labels(x)
  values <- get_values(x)
  na_values <- values[labels %in% na_labels]
  x[x %in% na_values] <- NA
  x
}

politbaro <- politbaro %>%
  mutate(across(wahlbeteiligung_absicht:gemeindegroesse, recode_missing_labels)) %>%
  mutate(
    schulabschluss_neu = case_when(
      !is.na(schulabschluss_9kat) ~ case_when(
        schulabschluss_9kat %in% c(3)                    ~ 1, # Mittelschule ohne Abschluss
        schulabschluss_9kat %in% c(1, 2)                 ~ 2, # Hauptschule mit/ohne Lehre
        schulabschluss_9kat %in% c(4)                    ~ 3, # Mittlere Reife
        schulabschluss_9kat %in% c(5, 6, 7)              ~ 4, # Höhere Schule / Fachschule / Abitur
        schulabschluss_9kat %in% c(8, 9)                 ~ 5, # Hochschule
        TRUE                                             ~ NA_real_
      ),
      !is.na(schulabschluss_5kat) ~ case_when(
        schulabschluss_5kat == 1 ~ 1,  # kein Hauptschulabschluss
        schulabschluss_5kat == 2 ~ 2,  # Hauptschulabschluss
        schulabschluss_5kat == 3 ~ 3,  # Realschule
        schulabschluss_5kat %in% c(4, 6) ~ 4,  # Abitur, Fachschule
        schulabschluss_5kat == 7 ~ 5,  # Hochschulabschluss
        schulabschluss_5kat == 5 ~ NA_real_,  # noch in Schule
        TRUE ~ NA_real_
      ),
      TRUE ~ NA_real_
    ),
    schulabschluss_neu = factor(
      schulabschluss_neu,
      levels = 1:5,
      labels = c(
        "Ohne Abschluss",
        "Hauptschule",
        "Realschule / mittlerer Abschluss",
        "Abitur / Fachschule",
        "Hochschule / Universität"
      ),
      ordered = TRUE
    ),
    gemeindegroesse_neu = case_when(
      !is.na(ortsgroesse) ~ case_when(
        ortsgroesse %in% c(1)                 ~ 1, # unter 2.000
        ortsgroesse %in% c(2, 3)              ~ 2, # 2.000 – 4.999
        ortsgroesse %in% c(4)                 ~ 3, # 5.000 – 19.999
        ortsgroesse %in% c(5, 6)              ~ 4, # 20.000 – 49.999
        ortsgroesse %in% c(7, 8)              ~ 5, # 50.000 – 199.999
        ortsgroesse %in% c(9, 10)             ~ 6, # 200.000+
        TRUE                                  ~ NA_real_
      ),
      !is.na(gemeindegroesse) ~ case_when(
        gemeindegroesse == 1                  ~ 1, # bis 2.000
        gemeindegroesse == 2                 ~ 2, # bis 5.000
        gemeindegroesse == 3                 ~ 2, # bis 10.000
        gemeindegroesse == 4                 ~ 3, # bis 20.000
        gemeindegroesse == 5                 ~ 4, # bis 50.000
        gemeindegroesse == 6                 ~ 5, # bis 100.000
        gemeindegroesse == 7                 ~ 5, # bis 500.000
        gemeindegroesse == 8                 ~ 6, # über 500.000
        TRUE                                  ~ NA_real_
      ),
      TRUE ~ NA_real_
    ),
    gemeindegroesse_neu = factor(
      gemeindegroesse_neu,
      levels = 1:6,
      labels = c(
        "unter 2.000",
        "2.000 – 4.999",
        "5.000 – 19.999",
        "20.000 – 49.999",
        "50.000 – 199.999",
        "200.000 und mehr"
      ),
      ordered = TRUE
    ),
    parteiID_spd        = if_else(parteineigung == 1, 1L, 0L, missing = NA_integer_),
    parteiID_cducsu     = if_else(parteineigung %in% c(2, 3, 4), 1L, 0L, missing = NA_integer_),  # merged CDU + CSU
    parteiID_fdp        = if_else(parteineigung == 5, 1L, 0L, missing = NA_integer_),
    parteiID_gruene     = if_else(parteineigung %in% c(6, 12), 1L, 0L, missing = NA_integer_),    # merged Greens + B90
    parteiID_linke      = if_else(parteineigung == 8, 1L, 0L, missing = NA_integer_),
    parteiID_afd        = if_else(parteineigung == 11, 1L, 0L, missing = NA_integer_),
    
    wahl_spd    = if_else(parteienwahl_absicht == 2, 1L, 0L, missing = NA_integer_),
    wahl_cducsu = if_else(parteienwahl_absicht == 1, 1L, 0L, missing = NA_integer_),
    wahl_fdp    = if_else(parteienwahl_absicht == 3, 1L, 0L, missing = NA_integer_),
    wahl_gruene = if_else(parteienwahl_absicht %in% c(4, 43), 1L, 0L, missing = NA_integer_),
    wahl_linke  = if_else(parteienwahl_absicht %in% c(6, 34), 1L, 0L, missing = NA_integer_),
    wahl_afd    = if_else(parteienwahl_absicht == 49, 1L, 0L, missing = NA_integer_)
  )

# ## Subset consistent variables
# 
# stetige_variablen <- c(
#   "studiennummer",
#   "befragtennummer",
#   "erhebungsmonat",
#   "erhebungsjahr",
#   "wahlbeteiligung_absicht",
#   "parteienwahl_absicht",
#   "wahl_rueckerinnerung",
#   "skalom_spd",
#   "skalom_cdu",
#   "skalom_csu",
#   "skalom_fdp",
#   "skalom_gruene",
#   "skalom_regierung",
#   "skalom_opposition",
#   "demokratiezufriedenheit",
#   "vertrauen_fuehrungspersonal",
#   "links_rechts_selbsteinstufung",
#   "beurteilung_wirtschaft_brd",
#   "wirtschaftslage_brd_1jahr",
#   "eigene_wirtschaftslage",
#   "eigene_wirtschaftslage_1jahr",
#   "wichtigstes_problem_1",
#   "wichtigstes_problem_2",
#   "kompetenz_arbeitsmarkt",
#   "kompetenz_wirtschaft",
#   "kompetenz_umweltschutz",
#   "kompetenz_renten",
#   "geschlecht",
#   "alter_kategorien",
#   "familienstand",
#   "berufstaetigkeit",
#   "berufsgruppe",
#   "parteineigung",
#   "staerke_parteineigung",
#   "bundesland",
#   "gemeindegroesse"
#   # "gewicht_faktor"
# )
# 
# politbaro <-
#   select(politbaro, all_of(stetige_variablen))
# 


save(politbaro, file = "./data/politbaro.rda")

