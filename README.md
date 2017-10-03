# Assault-Weapon-Count

After the horrible tragedy in Las Vegas on Sunday night, I wanted to know if there was a trend associated with assault weapons and mass shootings. In particular, what happened after the assault weapons ban from 1994-2004? 

Data

The data is from [US Mass Shootings, 1982-2017: Data From Mother Jones’ Investigation](http://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/). I counted the total number of assault rifles for each event and added as a new variable. Since we don't know excactly how many weapons were used during the recent Las Vegas event, I coded the event as three since that is what has been confirmed as assault weapons.


R Code:

```r
library(dplyr)
library(ggplot2)
library(ggthemes)

dat <- read.csv("https://docs.google.com/spreadsheet/pub?key=0AswaDV9q95oZdG5fVGJTS25GQXhSTDFpZXE0RHhUdkE&output=csv", stringsAsFactors = FALSE)

# Clean recent Vegas attacks
dat$Fatalities[1] <- 59
dat$Injured[1] <- 527

# Weapons Count
dat$Weapon.Dummy <- c(3L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 1L, 1L, 1L, 0L, 1L, 0L, 
0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 
0L, 1L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 2L, 1L, 0L, 0L, 0L, 1L, 0L, 0L)

# Clean up data
dat$Fatalities <- as.numeric(dat$Fatalities)
dat$Injured <- as.numeric(dat$Injured)
newdat <- select(dat, Year, Fatalities, Injured, Weapon.Dummy)

# Aggregate to year
newdat <- newdat %>% 
  group_by(Year) %>% 
  summarise(Fatalities = sum(Fatalities, na.rm = TRUE),
            Injured = sum(Injured, na.rm = TRUE),
            AWeapons = sum(Weapon.Dummy))

ggplot(newdat, aes(Year, Fatalities)) + scale_fill_brewer("GnBu")+
  theme_tufte(base_size = 14) + xlab(NULL) +
  geom_point(aes(color = "Fatalities"), alpha = 0.5) + 
  geom_point(aes(Year, AWeapons*10, color = "Assault Weapons"), alpha = 0.5) +
  geom_vline(xintercept = 1994, linetype = "dashed", color = "grey") + 
  geom_vline(xintercept = 2004, linetype = "dashed", color = "grey") + 
  annotate("text", x = 1999, y = 80, label = "Assault Weapons Ban") +
  geom_smooth(aes(color = "Fatalities"),se = FALSE,  size = .5) +
  geom_smooth(aes(Year, AWeapons*10, color = "Assault Weapons"), se = FALSE,  size = .5) +
  scale_y_continuous(sec.axis = sec_axis( ~./10, name = "Assault Weapons Count")) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("segment", x=Inf, xend=-Inf, y=Inf, yend=Inf, color = "grey") +
  annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = seq(1982, 2017, by = 5)) +    
  theme(legend.position="top") + 
  theme(legend.position = c(0,1), legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), legend.key = element_blank(),
        legend.title = element_blank()) 
```
  


![alt text](https://github.com/johnwoodill/Assault-Weapon-Count/blob/master/assault_weapons.png)
