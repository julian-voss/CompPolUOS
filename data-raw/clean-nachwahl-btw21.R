library(tidyverse)
library(haven)

rm(list = ls())

btw21 <- read_dta("./data-raw/NachwalbBTW21.dta")

btw21 <- btw21 %>% select(DATE_A, matches("Q01"), Q02, Q03, Q04, Q18:Q20, Q22, Q22_O, 
                          matches("Q32"), matches("Q40|Q41"), Q49, Q50, Geschlecht:S13, Region_WestOst, Region_Bula,
                          KREIS, GKPOL)

# Example: assuming your data frame is called `df`
labels_df <- data.frame(
  variable = names(btw21),
  label = sapply(btw21, function(x) attr(x, "label")),
  stringsAsFactors = FALSE
)


write_csv(labels_df, "~/Desktop/temp.csv")



btw21 %>% select(matches("Q22")) %>% view()


xlsx::write.xlsx(labels_df, "~/Desktop/temp.xlsx")


# Create a long data frame with variable, value, and label
label_df <- map_dfr(
  names(btw21 %>% select(-c(erhebungsjahr, erhebungsmonat, befragtennummer, gewicht_faktor))),
  function(varname) {
    var <- btw21[[varname]]
    values <- sort(unique(na.omit(var)))
    
    tibble(
      variable = varname,
      value = values    )
  }) %>%
  mutate(label = as_label(value))
