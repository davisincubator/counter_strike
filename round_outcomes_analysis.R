# Analyze map outcomes
library(dplyr)
library(ggplot2)
library(tidyr)

# Load data ---------------------------------------------------------------
raw <- read.csv("data/CSGOMaps02_RoundOutcomes.csv", stringsAsFactors = FALSE)

# Make data tidy
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


# Plot --------------------------------------------------------------------

ggplot(tdat, aes(round.group, Twin, color = skill.group, group = skill.group))+
  geom_line()+
  geom_point()+
  facet_wrap(~map)+
  scale_y_continuous(labels = function(x) paste0(round(x*100, 2), '%')) +
  labs(x = 'Round Group', y = 'Terrorist Win', color = 'Skill Group')



# interactive plot:

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








