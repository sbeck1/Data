#Converting landings (pounds of meat) into shell removed (cubic yards)

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

######################################################
#shell pounds for appalachicola


shell_pounds_app <- landings_50to18 %>% 
  group_by(Year) %>%
  summarize(shell_pounds_app = 3.578*(Landings_app))

#shell pounds for suwannee
shell_pounds_suw <- landings_50to18 %>%
  group_by(Year) %>%
  summarize(shell_pounds_suw = 3.578*(Landings_suw))

#shell_pounds_state
shell_pounds_state <- landings_50to18 %>%
  group_by(Year) %>%
  summarize(shell_pounds_state = 3.578*(Landings_state))


#######################################################

#shell pounds converted to oyster shell cubic feet cubic yards
shell_cubicft_app <- shell_pounds_app %>% 
  group_by(Year) %>%
  summarize(shell_cubicft_app = (shell_pounds_app)/80)


shell_cubicft_suw <- shell_pounds_suw %>%
  group_by(Year) %>%
  summarize(shell_cubicft_suw = (shell_pounds_suw)/80)


shell_cubicft_state <- shell_pounds_state %>%
  group_by(Year) %>%
  summarize(shell_cubicft_state = (shell_pounds_state)/80)

######################################################

#shell cubic feet converted to cubic yards
shell_cubicyd_app <- shell_cubicft_app %>% 
  group_by(Year) %>%
  summarize(shell_cubicyd_app = (shell_cubicft_app)/27)


shell_cubicyd_suw <- shell_cubicft_suw %>%
  group_by(Year) %>%
  summarize(shell_cubicyd_suw = (shell_cubicft_suw)/27)


shell_cubicyd_state <- shell_cubicft_state %>%
  group_by(Year) %>%
  summarize(shell_cubicyd_state = (shell_cubicft_state)/27)

######################################################
#bind together for easier plotting
shell_cubicyd_app <- select(shell_cubicyd_app, Year, shell_cubicyd_app)
shell_cubicyd_suw <- select(shell_cubicyd_suw, shell_cubicyd_suw)
shell_cubicyd_state <- select(shell_cubicyd_state, shell_cubicyd_state)


landings_cubicyd <- cbind(shell_cubicyd_app, shell_cubicyd_suw, shell_cubicyd_state)

landings_cubicyd <- select(landings_cubicyd, Year, shell_cubicyd_app, shell_cubicyd_suw, shell_cubicyd_state)


landing_pre<- subset(landings_cubicyd, landings_cubicyd$Year<1986)
landing_post<- subset(landings_cubicyd, landings_cubicyd$Year>1986)
class(landings_cubicyd$Year)

#landings_cubicyd$Year <- as.Date(landings_cubicyd $Year,origin= "1899-12-30")
#landing_pre$Year <- as.Date(landing_pre$Year,origin= "1899-12-30")
#landing_post$Year <- as.Date(landing_post$Year,origin= "1899-12-30")

ggplot() +
  labs(color="Region", x="Year", y= "Cubic Yard of Shell Hash", title = "Amount of historical shell material removed in cubic yards") +
  geom_point(data= landing_pre, aes(x=Year, y = shell_cubicyd_app, color=" Pre Apalachicola"), pch=16) +
  geom_point(data= landing_post, aes(x=Year,y = shell_cubicyd_app, color="Post Apalachicola")) +
  geom_point(data= landing_pre, aes(x=Year, y = shell_cubicyd_suw , color="Pre Suwannee"), pch=16) +
  geom_point(data= landing_post, aes(x=Year, y = shell_cubicyd_suw , color="Post Suwannee")) +
  geom_point(data= landing_pre, aes(x=Year, y = shell_cubicyd_state , color="Pre Florida State"), pch=16) +
  geom_point(data= landing_post, aes(x=Year, y = shell_cubicyd_state , color=" Post Florida State")) +
  scale_x_continuous(breaks=c(1950,1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990,1995,2000, 2005, 2010,2015,2020)) +
 
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.title=element_text(size=12,face="bold"),
        axis.text.x = element_text(size=8))
  

