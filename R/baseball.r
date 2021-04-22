library(tidyverse)
library(Lahman)

tail(Teams, 3)
help(Teams)


#Studying Runs and its correlation to Wins

#Creating a df of teams from 2000 - Now, only looking at Games, Runs and Runs Against
my_teams <- Teams %>%
  filter(yearID > 2000) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA)

tail(my_teams)

#Calculating Run Differential (RD) and Winning Percentage (Wpct)

my_teams <- my_teams %>%
  mutate(RD = R - RA, Wpct = W / (W+L))

#Plotting a scatter with Wpct on the Y and RD on the X
run_diff <- ggplot(my_teams, aes(x = RD, y = Wpct)) +
  geom_point() +
  scale_x_continuous("Run Differential")+
  scale_y_continuous("Winning Percentage")

linfit <- lm(Wpct ~ RD, data = my_teams)
linfit

run_diff +
  geom_smooth(method = "lm", se = FALSE)


#Plot 
install.packages("ggrepel")
library(ggrepel)
library(broom)
my_teams_aug <- augment(linfit, data = my_teams)

base_plot <- ggplot(my_teams_aug, aes(x = RD, y = .resid)) +
  geom_point(alpha = 0.3)+
  geom_hline(yintercept = 0, linetype = 3)+
  xlab("Run Differential") + ylab("Residual")

highlight_teams <- my_teams_aug %>%
  arrange(desc(abs(.resid))) %>%
  head(4)

base_plot +
  geom_point(data = highlight_teams)+
  geom_text_repel(data = highlight_teams, aes(label=paste(teamID, yearID)))



