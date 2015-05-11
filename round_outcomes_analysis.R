# Analyze map outcomes
library(dplyr)
library(ggplot2)
library(tidyr)

# Load data ---------------------------------------------------------------

# Outcome Data
raw <- read.csv("data/CSGOMaps02_RoundOutcomes.csv", stringsAsFactors = FALSE)
tdat <- raw %>%
          gather(map, Twin, de_dust2:de_overpass) %>%
          mutate(
            Twin = as.numeric(sub("%", "", Twin)) / 100,
            skill.group = gsub("[a-zA-Z ]", "", Skill.Group.Bin),
            skill.group = factor(skill.group, c("1-6", "7-10", "11-14", "15-18")),
            round.group = gsub("[a-zA-Z ]", "", Round.Group),
            round.group = factor(round.group, c("1", "2-3", "4-15"))
            ) %>%
          select(-Winner, -Skill.Group.Bin, -Round.Group)

# Bomb plant data
raw2 <- read.csv("data/Maps02_BombPlantOutcomes.csv", stringsAsFactors = FALSE)

# Account for weird layering by splitting into 2 data.frames
planted <- raw2 %>%
            select(-starts_with("X"), -contains(".1")) %>%
            filter(row_number() != 1) %>%   # Remove extra header
            gather(map, planted, de_dust2:de_overpass)

notplanted <- raw2 %>%
                select(1:4, ends_with(".1"), -starts_with("X")) %>%
                filter(row_number() != 1) %>%   # Remove extra header
                gather(map, notplanted, de_dust2.1:de_overpass.1) %>%
                mutate(map = gsub(".1", "", map))

# Merge and make pretty
bdat <- planted %>%
          left_join(notplanted) %>%
          mutate(
            skill.group = gsub("[a-zA-Z ]", "", Skill.Group.Bin),
            skill.group = factor(skill.group, c("1-6", "7-10", "11-14", "15-18")),
            round.group = gsub("[a-zA-Z ]", "", Round..group.),
            round.group = factor(round.group, c("1", "2-3", "4-15")),
            planted = as.numeric(sub("%", "", planted)) / 100,
            notplanted = as.numeric(sub("%", "", notplanted)) / 100
          ) %>%
          select(skill.group, round.group, winner = Round.Winner,
                 endreason = RoundEndReason, map, planted, notplanted) %>%
          filter(!is.na(skill.group))


# Plot --------------------------------------------------------------------
moi <- c("de_dust2", "de_nuke", "de_inferno")

ggplot(filter(tdat, map %in% moi), aes(round.group, Twin, color = skill.group, group = skill.group))+
  geom_line()+
  geom_point()+
  facet_wrap(~map)+
  scale_y_continuous(labels = function(x) paste0(round(x*100, 2), '%')) +
  labs(x = 'Round Group', y = 'Terrorist Win', color = 'Skill Group')

ggplot(filter(bdat, map == 'de_dust2'), aes(round.group, planted, color = skill.group, group = skill.group))+
  geom_line()+
  geom_point()+
  facet_grid(winner ~ endreason)


# Interactive Plot --------------------------------------------------------

library(rCharts)
require(devtools)
install_github('rCharts', 'ramnathv')

# select maps for plotting inclusion; here we're only looking 
# at maps we play:
inc_maps = c('de_dust2', 'de_nuke', 'de_inferno')

# plot terrorist win by round and map:
blah = nPlot(Twin ~ round.group, group = "map", 
             data = filter(tdat, map %in% inc_maps), 
             type = "multiBarChart")
blah$chart(forceY = c(0, 0.7))

print(blah)

blah$print("chart3")

# plot terrorist win by round and map and skill group
blah2 = nPlot(Twin ~ map, group = "skill.group", 
             data = filter(tdat, map %in% inc_maps), 
             type = "multiBarChart")
blah2$chart(forceY = c(0, 0.7))
#blah2$addControls("x", value = "wt", values = names(mtcars))
print(blah2)








