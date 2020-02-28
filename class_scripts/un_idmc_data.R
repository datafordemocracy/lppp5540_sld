# UN and IDMC data
# More graphing, joins, and pivot

# setup
library(tidyverse)
library(haven)

idmc <- read_dta("IDMC_Displacements.dta")
un <- read_dta("UNmonadic.dta")

names(idmc)
names(un)

# # un: what is totalpopulation?
# un <- un %>% mutate(tots = select(., refugeesinclrefugeelikesituation,
#                                   asylumseekerspendingcases,
#                                   internallydisplacedpersonsidps,
#                                   othersofconcern) %>% 
#                       rowSums(na.rm = TRUE))
# sum(un$totalpopulation == un$tots)

# un ----
# idps, select one country by time
un %>% filter(iso == "AFG") %>% mutate(year = as.integer(year)) %>% 
  ggplot(aes(x = year, y = internallydisplacedpersonsidps)) + 
  geom_line() 

# idps, all countries by time
un %>% mutate(year = as.integer(year)) %>% 
  ggplot(aes(x = year, y = internallydisplacedpersonsidps, color = iso)) +
  geom_line() + guides(color = FALSE)

# refugees vs idps
ggplot(un, aes(x = refugeesinclrefugeelikesituation, y = internallydisplacedpersonsidps)) + 
  geom_point() + geom_text(aes(label=iso), hjust=0, vjust=0)


# idmc ----
# idps, select one country by time
idmc %>% filter(iso3 == "AFG") %>% mutate(year = as.integer(year)) %>% 
  ggplot(aes(x = year, y = conflictstockdisplacement)) + geom_line()

# idps, all countries by time
idmc %>% mutate(year = as.integer(year)) %>% 
  ggplot(aes(x = year, y = conflictstockdisplacement, color = iso3)) + 
  geom_line() + guides(color = FALSE)

# conflict new vs disaster new
ggplot(idmc, aes(x = conflictnewdisplacements, y = disasternewdisplacements)) + 
  geom_point() + geom_text(aes(label=iso3), hjust=0, vjust=0)


# Joined data ----
# Join un and idmc by iso, year
# make iso character
un <- un %>% mutate(iso = as.character(iso))
idmc <- idmc %>% mutate(iso3 = as.character(iso3))

# join
df <- full_join(un, idmc, by = c("iso"="iso3", "year"="year"))

# keep only analagous variables
idp <- df %>% 
  select(origin, iso, year, internallydisplacedpersonsidps, conflictstockdisplacement)

# un idp vs idmc stock displacement
ggplot(idp, aes(x = conflictstockdisplacement, y = internallydisplacedpersonsidps)) +
  geom_point()

# # calculate differences
# idp <- idp %>% mutate(compare = internallydisplacedpersonsidps - conflictstockdisplacement,
#                       per_idmc = (compare/conflictstockdisplacement)*100)
# summary(idp$per_idmc)
# summary(idp$compare)

# graph both sources for a country together by time
# make the data long (pivot_longer)
idp_long <- idp %>% select(iso, year, un=internallydisplacedpersonsidps, 
                           idmc = conflictstockdisplacement) %>% 
    pivot_longer(c("un", "idmc"), names_to = "source", values_to = "idp")

# find countries with highest idp values
idp_long %>% group_by(iso) %>% 
  summarize(maxidp = max(idp, na.rm = TRUE)) %>% 
  arrange(desc(maxidp)) %>% head(10)

# graph, selecting by iso (those with max idp): e.g. COL, SYR, SDN, COD, IRQ
idp_long %>% filter(iso == "IRQ") %>% 
  ggplot(aes(x = year, y = idp, color = source)) + 
  geom_line()

