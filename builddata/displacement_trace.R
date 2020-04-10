# #######################################
# Saving Lives with Data 2
# Spring 2020
# Michele Claibourn

# Displacement trajectories
#######################################

library(tidyverse)
library(ggrepel)
library(plotly)

load("displacement.rdata") 


# a. displacement by year ----
ggplot(df, aes(x = year, y = size, color = iso)) + 
  geom_line() + guides(color = FALSE)


# b. displacement by time (onset) ----
df_curve <- df %>% 
  select(iso, year, size) %>% 
  drop_na(iso) %>% 
  group_by(iso) %>% 
  arrange(year) %>% 
  filter(max(size) > 24999) %>% 
  mutate(years_elapsed = year - min(year),
         end_label = ifelse(year == max(year), iso, NA),
         iso3 = iso) %>% 
  ungroup()

ggplot(df_curve, aes(x = years_elapsed, y = size, color = iso, label = end_label)) + 
  geom_line() + 
  geom_text_repel(nudge_x = 1.1, nudge_y = 0.1, segment.color = NA, size = 3) +
  guides(color = FALSE)


# c. a little interactive ----
p <- ggplot(df_curve, aes(x = years_elapsed, y = size, color = iso, label = year)) + 
  geom_line(size = 0.25) + 
  theme(legend.position = "none")
ggplotly(p, tooltip = c("iso", "size", "year"))


# d. highlight one trajectory ----
table(df_curve$iso)
target <- df_curve %>% filter(iso == "SOM") # choose target

# plot
df_curve %>% 
  ggplot(aes(x = years_elapsed, y = size)) + 
  # line traces for each country in all panels
  geom_line(aes(group = iso3),
            size = 0.2, color = "grey70") +
  # line trace in red for key country in panel
  geom_line(data = target, aes(y = size), color = "firebrick") + 
  annotate("text", x = max(target$years_elapsed)+3, y = max(target$size), 
           label = paste(target$iso), color = "firebrick", 
           size = 3) +
  labs(x = "Years since Displacement Onset (or 1960)", 
       y = "Estimated Size of Displacement", 
       title = "Displacement Size by Country of Origin", 
       caption = "Data Source: UNHCR at http://popstats.unhcr.org/")
  