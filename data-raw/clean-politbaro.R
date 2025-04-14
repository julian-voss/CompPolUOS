library(tidyverse)
library(haven)
# library(sjlabelled)
library(labelled)

## Read with proper names

politbaro <-
  haven::read_dta("./data-raw/politbarometer/ZA2391_v15-2-0.dta", encoding = "utf-8") %>%
  # Rename
  rename(
    studiennummer = v1,
    befragtennummer = v2,
    erhebungsmonat = v3,
    erhebungsjahr = v4,
    wahlbeteiligung_absicht = v5,
    parteienwahl_absicht = v6,
    wahl_rueckerinnerung = v7,
    thermom_spd = v8,
    thermom_cdu = v9,
    thermom_csu = v10,
    thermom_fdp = v11,
    thermom_gruene = v12,
    thermom_republikaner = v13,
    thermom_pds = v14,
    thermom_regierung = v15,
    thermom_opposition = v16,
    thermom_gruene_opposition = v17,
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

## Subset consistent variables

stetige_variablen <- c(
  "studiennummer",
  "befragtennummer",
  "erhebungsmonat",
  "erhebungsjahr",
  "wahlbeteiligung_absicht",
  "parteienwahl_absicht",
  "wahl_rueckerinnerung",
  "thermom_spd",
  "thermom_cdu",
  "thermom_csu",
  "thermom_fdp",
  "thermom_gruene",
  "thermom_regierung",
  "thermom_opposition",
  "demokratiezufriedenheit",
  "vertrauen_fuehrungspersonal",
  "links_rechts_selbsteinstufung",
  "beurteilung_wirtschaft_brd",
  "wirtschaftslage_brd_1jahr",
  "eigene_wirtschaftslage",
  "eigene_wirtschaftslage_1jahr",
  "wichtigstes_problem_1",
  "wichtigstes_problem_2",
  "kompetenz_arbeitsmarkt",
  "kompetenz_wirtschaft",
  "kompetenz_umweltschutz",
  "kompetenz_renten",
  "geschlecht",
  "alter_kategorien",
  "familienstand",
  "berufstaetigkeit",
  "berufsgruppe",
  "parteineigung",
  "staerke_parteineigung",
  "bundesland",
  "gemeindegroesse"
  # "gewicht_faktor"
)

politbaro <-
  select(politbaro, all_of(stetige_variablen))

## Recode missing values

# Create a long data frame with variable, value, and label
label_df <- map(
  names(politbaro %>% select(-c(erhebungsjahr, erhebungsmonat, befragtennummer))),
  function(varname) {
    
    counts <- politbaro %>% count(pick(all_of(varname))) %>% rename(value = 1) %>% mutate(label = to_character(value)) %>% mutate(variable = varname)
    
    return(counts)

  }) %>%
  bind_rows()

na_labels <- c("nicht erhoben", "weiß nicht", "KA", "KA, verweigert", "KA, TNZ", "TNZ", "TNZ/nicht erhoben",
               "kein weiteres Problem")
var <- politbaro$demokratiezufriedenheit
labels <- get_labels(var)
values <- get_values(var)
na_values <- values[labels %in% na_labels]

recode_missing_labels <- function(x) {
  na_labels <- c("nicht erhoben", "weiß nicht", "KA", "KA, verweigert", "KA, TNZ", "TNZ", "TNZ/nicht erhoben",
                 "kein weiteres Problem")
  labels <- get_labels(x)
  values <- get_values(x)
  na_values <- values[labels %in% na_labels]
  x[x %in% na_values] <- NA
  x
}

politbaro <- mutate(politbaro, across(wahlbeteiligung_absicht:gemeindegroesse, recode_missing_labels))

save(politbaro, file = "./data/politbaro.rda")