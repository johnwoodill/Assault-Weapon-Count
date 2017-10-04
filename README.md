# Assault-Weapon-Count

After the horrible tragedy in Las Vegas on Sunday night, I wanted to know if there was a trend associated with assault weapons and mass shootings. In particular, what happened after the assault weapons ban from 1994-2004? 

**Data**

The data is from [US Mass Shootings, 1982-2017: Data From Mother Jones’ Investigation](http://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/). I counted the total number of assault rifles for each event and added as a new variable. Since we don't know excactly how many weapons were used during the recent Las Vegas event, I coded the event as three since that is what has been confirmed as assault weapons.

**Variables**

*Assault Weapons*: number of assault weapons used in each year. Data describes weapon as "rifle (assault)"

*Fatalities*: fatalities per 100 million people summed over individual year

*Fatalities (sign of mental health)*: fatalities per 100 million people summed over individual year. Indicator for sign of mental health described in data as "yes" -- does not include "unclear."

*Events*: events over 20 killed are labeled in box by year

*Population*: U.S. population data from FRED. Population for 2017 was estimated based on the growth rate from the last 10 years.

**R Code:**

```r
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)

# Get mass shooting data
dat <- read.csv("https://docs.google.com/spreadsheet/pub?key=0AswaDV9q95oZdG5fVGJTS25GQXhSTDFpZXE0RHhUdkE&output=csv", stringsAsFactors = FALSE)

# Get population data
cens <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?chart_type=line&recession_bars=on&log_scales=&bgcolor=%23e1e9f0&graph_bgcolor=%23ffffff&fo=Open+Sans&ts=12&tts=12&txtcolor=%23444444&show_legend=yes&show_axis_titles=yes&drp=0&cosd=1960-01-01&coed=2016-01-01&height=450&stacking=&range=Max&mode=fred&id=POPTOTUSA647NWDB&transformation=lin&nd=1960-01-01&ost=-99999&oet=99999&lsv=&lev=&mma=0&fml=a&fgst=lin&fgsnd=2009-06-01&fq=Annual&fam=avg&vintage_date=&revision_date=&line_color=%234572a7&line_style=solid&lw=2&scale=left&mark_type=none&mw=2&width=1168", stringsAsFactors = FALSE)
cens$year <- substring(cens$DATE, 1, 4)
cens$DATE <- NULL

# Interpolate growth rate for 2017
# Find average over last 10 years
cens$gr <- ((cens$POPTOTUSA647NWDB - lag(cens$POPTOTUSA647NWDB))/lag(cens$POPTOTUSA647NWDB))

gr2017 <- mean(cens$gr[48:57])   # [1] 0.8000213
pop2017 <- cens$POPTOTUSA647NWDB[57]*(1 + gr2017)

# 2017 estimated population
cens <- rbind(cens, c(pop2017, 2017, gr2017))

names(cens) <- c("pop", "Year", "gr")          
cens$gr <- NULL     
cens$Year <- as.integer(cens$Year)

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
newdat <- select(dat, Year, Fatalities, Injured, Weapon.Dummy, Prior.signs.of.mental.health.issues)
newdat$mh <- ifelse(newdat$Prior.signs.of.mental.health.issues == "Yes", 1, NA)

# Aggregate to year
newdat <- newdat %>% 
  group_by(Year) %>% 
  summarise(Fatalities = sum(Fatalities, na.rm = TRUE),
            Injured = sum(Injured, na.rm = TRUE),
            AWeapons = sum(Weapon.Dummy, na.rm = TRUE),
            mh = mean(mh, na.rm = TRUE))

newdat <- left_join(newdat, cens, by = "Year")


# Weight by population
newdat$Fatalities <- newdat$Fatalities/((newdat$pop/100000000))
newdat$mh <- ifelse(newdat$mh == 1, newdat$Fatalities, NA)
newdat <- arrange(newdat, -Year)

# Add significant events
newdat$event <- NA
newdat$event[1] <- "Stephen Paddock, Las Vegas \n 59+ killed"
newdat$event[2] <- "Omar Mateen, Orlando \n 49 killed"
newdat$event[5] <- "Adam Lanza, Newtown \m 28 killed"
newdat$event[11] <- "Seung-Hui Cho, Virginia Tech \n 32 killed"
newdat$event[26] <- "George Hennard, Killeen TX \n 24 killed"
newdat$event[32] <- "James Huberty, San Ysidro \n 22 killed"


ggplot(newdat, aes(Year, Fatalities)) + 
  geom_label_repel(aes(label = event), color = 'black', size = 3.5, nudge_y = 7) +
  theme_tufte(base_size = 14) + 
  geom_point(aes(color = "Fatalities"), shape = 1, size = 2) + 
  geom_point(aes(Year, mh, color = "Fatalities (signs of mental health)"), shape = 16, size = 2) +
  geom_point(aes(Year, AWeapons*10, color = "Assault Weapons"), size = 2) +
  geom_vline(xintercept = 1994, linetype = "dashed", color = "grey") + 
  geom_vline(xintercept = 2004, linetype = "dashed", color = "grey") + 
  annotate("text", x = 1999, y = 50, label = "Assault Weapons Ban") +
  geom_smooth(aes(color = "Fatalities"), se = FALSE,  size = .5) + 
  geom_smooth(aes(Year, AWeapons*10, color = "Assault Weapons"), se = FALSE,  size = .5) +
  xlab(NULL) + ylab("Fatalaties \n (per 100 million)") +
  scale_y_continuous(sec.axis = sec_axis( ~./10, name = "Assault Weapons Count")) +
  scale_x_continuous(breaks = seq(1982, 2017, by = 5)) +    
  theme(legend.position = c(0,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "grey", fill=NA, size=1)) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf", "#67a9cf"),
                     guide = guide_legend(override.aes = list(linetype = c(rep("blank", 3)),
                                                              shape = c(16, 1, 16))))
```
  


![alt text](https://github.com/johnwoodill/Assault-Weapon-Count/blob/master/assault_weapons.png)
