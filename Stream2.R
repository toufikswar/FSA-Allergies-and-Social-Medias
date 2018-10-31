### COUNT OF 14 ALLERGENS AND OTHER ALLERGENS
load("Tweet_allergens.RData")
library(tidyr)
#content.by.source.df <- data.frame(content.dtm)

# 14 allergens by source:
fourteen_allergen_source_dict.dfm <- dfm_lookup(content.by.source.dfm, fourteen_allergens.dict, nomatch = "_unmatched")
# Lookup 14 allergens in content.dtm 
fourteen_allergen_dict.dfm <- dfm_lookup(content.dfm, fourteen_allergens.dict, nomatch = "_unmatched")

# other Allergens by source
other_allergens_source_dict.dfm <- dfm_lookup(content.by.source.dfm, other_allergens.dict, nomatch = "_unmatched")
# Lookup other allergens in content.dtm
other_allergens_dict.dfm <- dfm_lookup(content.dfm, other_allergens.dict, nomatch = "_unmatched")

library(tidyr)
library(forcats)
fourteen_allergen_by_source.long <- data.frame(fourteen_allergen_source_dict.dfm)
fourteen_allergen_by_source.long <- gather(fourteen_allergen_by_source.long, Allergen, "Mentions", 2:15, factor_key = TRUE)
fourteen_allergen_by_source.long$class <- "Fourteen_Allergens"

other_allergen_by_source.long <- data.frame(other_allergens_source_dict.dfm)
other_allergen_by_source.long <- gather(other_allergen_by_source.long, Allergen, "Mentions", 2:25, factor_key = TRUE)
other_allergen_by_source.long$class <- "Other_Allergens"

Allergen.by.source.df <- rbind(fourteen_allergen_by_source.long, other_allergen_by_source.long)

library(ggplot2)
fourteen.bysource <- ggplot(fourteen_allergen_by_source.long, 
                            aes(x = fct_reorder(Allergen, Mentions), y = Mentions, fill = document)) +
  geom_col(position = "identity" ) +
  theme_minimal() +
  scale_fill_brewer(palette="Spectral") 
#coord_flip()
fourteen.bysource

otherallergen.bysource <- ggplot(other_allergen_by_source.long, 
                                 aes(x = fct_reorder(Allergen, Mentions, fun = sum, na.rm = TRUE), y = Mentions, fill = document))+
  geom_col(position = "identity" ) +
  theme_minimal()+
  scale_fill_brewer(palette="Spectral")+
  coord_flip()
otherallergen.bysource

Allergen.by.source <- ggplot(Allergen.by.source.df, 
                             aes(x = fct_reorder(Allergen, Mentions), y = Mentions, fill = document, colour = class))+
  geom_col(position = "identity") +
  theme_minimal()+
  scale_fill_brewer(palette="Spectral")+
  coord_flip() 
Allergen.by.source

