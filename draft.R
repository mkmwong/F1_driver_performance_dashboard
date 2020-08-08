setwd("~/Desktop/Side_proj/f1_driver_dashboard/data/")
library(dplyr)
library(reshape2)
library(ggpubr)
library(plotly)
library(Hmisc)
library(gridExtra)

driver = 'hamilton'

##### data for plotting qualifying vs results #####
##### remove quali - only need result df for this 
quali <- read.csv("qualifying.csv")
res <- read.csv("results.csv")
sche <- read.csv("schedule.csv")
sche_subset <- sche %>% select(season, round, circuitId)
quali_subset <- quali %>% select(position, driverId, season, round)
res_subset <- res %>% select(driverId, season, round, position) 
summary <- quali_subset %>% full_join(res_subset, by=c("season","round","driverId"))
summary <- summary %>% full_join(sche_subset, by=c("season","round"))
summary <- summary %>% rename(Quali_pos=position.x, Res_pos=position.y )
summary <- summary %>% select(Quali_pos, Res_pos, driverId, season, circuitId)
summary <- melt(summary, id=c("driverId","season","circuitId"))

##### making plot for qualifying vs results #####
##### qualifying position not availabe for very old races
##### need some sort of a fix


plot_list = list()
count = 1
for(i in sort(unique(temp$season))) {
  max_y = max(temp$value)
  temp1 <- temp %>% filter(season == i)
  if( length(temp1$driverId) >= 20) {
    print(i)
    p = ggplot(temp1, aes(x=variable, y=value, color = variable)) + theme_bw() +
      geom_boxplot(width = 0.25, color = 'black', outlier.color = NA) + geom_jitter(width = 0.25)+
      #geom_dotplot(binaxis='y', stackdir='up', aes(fill = variable), dotsize = 0.3, stackratio = 0.5) + 
      #stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), geom="crossbar", width=0.25) + 
      theme(axis.title = element_text(face="bold"), legend.position = "none",
            axis.text = element_text(face="bold")) + ylim(1,max_y) + 
      xlab("") + ylab("Position") + scale_x_discrete(labels = c("Quali","Race")) + 
      scale_color_manual(values = c("grey","firebrick2")) +labs(title=i)
    plot_list[[count]] = ggplotly(p)
    print(p)
    count = count + 1
  }
}
a = do.call(grid.arrange, c(plot_list[(length(plot_list)-2):length(plot_list)], nrow = 1))

### facet_wrap version
temp <- summary %>% filter(driverId == driver)
max_y = max(temp$value)
if(max(temp$season) == 2020) {
  recent_year <- 2017:2019
  if(min(temp$season) >2017) {
    recent_year <- min(temp$season):2019
  }
} else {
  recent_year <- (max(temp$season)-2):max(temp$season)
  if(min(temp$season) > (max(temp$season)-2)) {
    recent_year <- min(temp$season):max(temp$season)
  }
}
temp1 <- temp %>% filter(season %in% recent_year)
p = ggplot(temp1, aes(x=variable, y=value, color = variable, text = paste('Circuit: ', circuitId))) + theme_bw() +
  geom_boxplot(width = 0.25, color = 'black', outlier.color = NA) + geom_jitter(width = 0.25)+
  facet_wrap(~ season) +
  theme(axis.title = element_text(face="bold"), legend.position = "none",
        axis.text = element_text(face="bold")) + ylim(1,max_y) + 
  xlab("") + ylab("Position") + scale_x_discrete(labels = c("Quali","Race")) + 
  scale_color_manual(values = c("grey","firebrick2")) 
p = ggplotly(p, tooltip = "text")
for(i in 1:(length(p)/2)){
  p$x$data[[1]]$marker$opacity = 0 
}
p

##### data for plotting performance at each track #####
res <- read.csv("results.csv")
sche <- read.csv("schedule.csv")
const <-  read.csv("constructors.csv")
const_subset <- const %>% select(constructorId, color)
sche_subset <- sche %>% select(season, round, circuitId)
res_subset <- res %>% select(driverId, season, round, position,constructorId) 
summary <- res_subset %>% full_join(sche_subset, by=c("season","round"))
summary <- summary %>% full_join(const_subset, by="constructorId")
summary <- summary %>% select(position, driverId, season, circuitId, color)

##### making plot for qualifying vs results #####
temp <- summary %>% filter(driverId == driver)
max_y = max(temp$position)
b = ggplot(temp, aes(x=reorder(circuitId,position,FUN = median), y=position)) + theme_bw() +
  geom_boxplot(width = 0.25, color = 'black', outlier.color = NA) + geom_jitter(width = 0.25 ,color = temp$color)+
  theme(axis.title = element_text(face="bold"), legend.position = "none",
        axis.text = element_text(face="bold"), axis.text.x = element_text(angle = 30, hjust = 1)) + 
  ylim(1,max_y) + xlab("Circuit") + ylab("Position") 

##### data for performance against teammate #####
res <- read.csv("results.csv")
res_subset <- res %>% select(driverId, season, round, position, constructorId) 
const <-  read.csv("constructors.csv")
const_subset <- const %>% select(constructorId, color)
summary <- res_subset %>% full_join(res_subset, by = c("season","round","constructorId"))
summary <- summary %>% filter(driverId.x!= driverId.y)
summary <- summary %>% mutate(pos_change = position.x - position.y)
summary <- summary %>% full_join(const_subset, by="constructorId")
temp <- summary %>% filter(driverId.x == driver)
max_y = max(temp$position.x)
c = ggplot(temp, aes(x=reorder(driverId.y ,pos_change,FUN = median), y=pos_change)) + theme_bw() +
  geom_boxplot(width = 0.25, color = 'black', outlier.color = NA) + geom_jitter(width = 0.25,color = temp$color) +  
  theme(axis.title = element_text(face="bold"), legend.position = "none",
        axis.text = element_text(face="bold"), axis.text.x = element_text(angle = 30, hjust = 1)) + 
  ylim(-max_y,max_y) + xlab("Teammate") + ylab("Position relative to teammate") 

##### performance at each team #####
res <- read.csv("results.csv")
res_subset <- res %>% select(driverId, season, round, position, constructorId) 
const <-  read.csv("constructors.csv")
const_subset <- const %>% select(constructorId, color)
summary <- res_subset %>% full_join(const_subset, by="constructorId")
summary <- summary %>% full_join(sche_subset, by=c("season","round"))
temp <- summary %>% filter(driverId == driver)
max_y <- max(temp$position)
summary <- summary %>% mutate(text = paste0("Season:",season,"<br>Round:",round,"<br>Circuit:",circuitId))
d <- ggplot(temp, aes(x=reorder(constructorId ,position,FUN = median), y=position, text = summary$text)) + theme_bw() +
  geom_boxplot(width = 0.25, color = 'black', outlier.color = NA) + geom_jitter(width = 0.25,  color = temp$color) +
  theme(axis.title = element_text(face="bold"), legend.position = "none",
        axis.text = element_text(face="bold")) + 
  ylim(1,max_y) + xlab("Constructor") + ylab("Position") 
d = ggplotly(d, tooltip = "text")
for(i in 1:(length(d)/2)){
  d$x$data[[i]]$marker$opacity = 0 
}
d


do.call(grid.arrange, c(c(b,c,d), nrow = 2))

grid.arrange(a,b,c,d, layout_matrix = rbind(c(1,2),c(3,4)))

###### test gauge chart generation
df <- data.frame(matrix(nrow=5, ncol = 2))

names(df) <- c("variable", "percentage")
df$variable <- c("Carbohydrates", "Warming", "NGTnotPresent", "DrainNotPresent", "DrEaMing")
df$percentage <- c(0.67,0.33,0.86,0.78,0.58)

df <- df %>% mutate(group=ifelse(percentage <0.6, "red",
                                 ifelse(percentage>=0.6 & percentage<0.8, "orange","green")),
                    label=paste0(percentage*100, "%"),
                    title=dplyr::recode(variable, `Carbohydrates`="Preoperative\ncarbohydrate loading",
                                        `Warming`="Intraoperative\nwarming",
                                        `NGTnotPresent`="Patients without a\nnasogastric tube\non arrival in recovery",
                                        `DrainNotPresent`="Patients without an\nabdominal drain\non arrival in recovery",
                                        `DrEaMing`="Patients DrEaMing on\npostoperative day 1"))
ggplot(df, aes(fill = group, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
  geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
  geom_rect() + 
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
  geom_text(aes(x = 0, y = 0, label = label, colour=group), size=6.5) +
  geom_text(aes(x=1.5, y=1.5, label=title), family="Poppins Light", size=4.2) + 
  facet_wrap(~title, ncol = 5) +
  theme_void() +
  scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  scale_colour_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill=FALSE) +
  guides(colour=FALSE) 

##### df for gauge chart(pole position) #####
quali <- read.csv("qualifying.csv")
res <- read.csv("results.csv")
sche <- read.csv("schedule.csv")
const <-  read.csv("constructors.csv")
const_subset <- const %>% select(constructorId, color)
sche_subset <- sche %>% select(season, round, circuitId)
quali_subset <- quali %>% select(position, driverId, season, round)
res_subset <- res %>% select(driverId, season, round, position,FastestLap.rank, constructorId) 
summary <- quali_subset %>% full_join(res_subset, by=c("season","round","driverId"))
summary <- summary %>% full_join(sche_subset, by=c("season","round"))
summary <- summary %>% rename(Quali_pos=position.x, Res_pos=position.y )
summary <- summary %>% full_join(const_subset, by="constructorId")

##### pole plot #####
##### qualifying position not availabe for very old races
##### need some sort of a fix
temp <- summary %>% filter(driverId == driver)
tab <- melt(table(temp$Quali_pos))
df <- as.data.frame(matrix(ncol = 3, nrow = 1))
if (identical(which(tab$Var1==1), integer(0))) {
  df[1,] <- c("black", 0, paste0(0,"/",length(temp$Quali_pos)))
  df <- df %>% mutate(V2 = as.numeric(V2))
} else {
  df[1,] <- c(unique(temp$color[which(temp$season == max(temp$season))])[1],
              tab$value[which(tab$Var1==1)]/length(temp$Quali_pos),
              paste0(tab$value[which(tab$Var1==1)],"/",length(temp$Quali_pos)))
  df <- df %>% mutate(V2 = as.numeric(V2))
}
e = ggplot(df, aes(fill = V1, ymax = V2, ymin = 0, xmax = 2, xmin = 1)) +
  geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="cornsilk") +
  geom_rect(fill = df$V1) + 
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
  geom_text(aes(x = 0, y = 0, label = V3), size=6.5) +
  theme_void() + theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  guides(fill=FALSE) + guides(colour=FALSE) +
  geom_text(aes(x=1.5, y=1.5, label="Pole Positions"), family="Poppins Light", size=4.2) 

##### race win plot #####
temp <- summary %>% filter(driverId == driver)
tab <- melt(table(temp$Res_pos))
df <- as.data.frame(matrix(ncol = 3, nrow = 1))
if (identical(which(tab$Var1==1), integer(0))) {
  df[1,] <- c("black", 0, paste0(0,"/",length(temp$Quali_pos)))
  df <- df %>% mutate(V2 = as.numeric(V2))
} else {
  df[1,] <- c(unique(temp$color[which(temp$season == max(temp$season))])[1],
              tab$value[which(tab$Var1==1)]/length(temp$Quali_pos),
              paste0(tab$value[which(tab$Var1==1)],"/",length(temp$Quali_pos)))
  df <- df %>% mutate(V2 = as.numeric(V2))
}
f = ggplot(df, aes(fill = V1, ymax = V2, ymin = 0, xmax = 2, xmin = 1)) +
  geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="cornsilk") +
  geom_rect(fill = df$V1) + 
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
  geom_text(aes(x = 0, y = 0, label = V3), size=6.5) +
  theme_void() + theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  guides(fill=FALSE) + guides(colour=FALSE) +
  geom_text(aes(x=1.5, y=1.5, label="Race Wins"), family="Poppins Light", size=4.2) 

##### podium plot #####
temp <- summary %>% filter(driverId == driver)
tab <- melt(table(temp$Res_pos))
df <- as.data.frame(matrix(ncol = 3, nrow = 1))
if (identical(which(tab$Var1==1), integer(0))) {
  df[1,] <- c("black", 0, paste0(0,"/",length(temp$Quali_pos)))
  df <- df %>% mutate(V2 = as.numeric(V2))
} else {
  df[1,] <- c(unique(temp$color[which(temp$season == max(temp$season))])[1],
              tab$value[which(tab$Var1==1)]/length(temp$Quali_pos),
              paste0(tab$value[which(tab$Var1==1)],"/",length(temp$Quali_pos)))
  df <- df %>% mutate(V2 = as.numeric(V2))
}
g = ggplot(df, aes(fill = V1, ymax = V2, ymin = 0, xmax = 2, xmin = 1)) +
  geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="cornsilk") +
  geom_rect(fill = df$V1) + 
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
  geom_text(aes(x = 0, y = 0, label = V3), size=6.5) +
  theme_void() + theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  guides(fill=FALSE) + guides(colour=FALSE) +
  geom_text(aes(x=1.5, y=1.5, label="Podiums"), family="Poppins Light", size=4.2) 

##### fastest lap plot #####
##### qualifying position not availabe for very old races
##### need some sort of a fix
temp <- summary %>% filter(driverId == driver)
tab <- melt(table(temp$FastestLap.rank))
df <- as.data.frame(matrix(ncol = 3, nrow = 1))
if (identical(which(tab$Var1==1), integer(0))) {
  df[1,] <- c("black", 0, paste0(0,"/",length(temp$Quali_pos)))
  df <- df %>% mutate(V2 = as.numeric(V2))
} else {
  df[1,] <- c(unique(temp$color[which(temp$season == max(temp$season))])[1],
              tab$value[which(tab$Var1==1)]/length(temp$Quali_pos),
              paste0(tab$value[which(tab$Var1==1)],"/",length(temp$Quali_pos)))
  df <- df %>% mutate(V2 = as.numeric(V2))
}
h = ggplot(df, aes(fill = V1, ymax = V2, ymin = 0, xmax = 2, xmin = 1)) +
  geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="cornsilk") +
  geom_rect(fill = df$V1) + 
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
  geom_text(aes(x = 0, y = 0, label = V3), size=6.5) +
  theme_void() + theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  guides(fill=FALSE) + guides(colour=FALSE) +
  geom_text(aes(x=1.5, y=1.5, label="Fastest Lap in Race"), family="Poppins Light", size=4.2) 

grid.arrange(a,b,c,d,e,f,g,h, layout_matrix = rbind(c(5,6,7,8),c(1,1,2,2),c(3,3,4,4)) )

##### performance per year
####FIX DRIVERID output on driver standing!
####Should normalize by the point system
driv_st <- read.csv("driverStandings.csv")
colnames(driv_st)[5] = 'driverId'
temp <- driv_st %>% filter(driverId == driver)
i = ggplot(temp, aes(x=round, y = points, color = as.factor(season))) + geom_line() + 
  theme_bw() + labs(color = "Year", x = "Round", y = "Points") + 
  theme(axis.title = element_text(face = "bold"))
ggplotly(x)

###
res_subset <- res %>% select(driverId, constructorId, round, season)
temp <- driv_st %>% full_join(res_subset, by=c("driverId", "round", "season"))
temp <- temp %>% filter(driverId == input$driver)
temp <- temp %>% mutate(text = paste0("Season:",season,"<br>Round:",round,"<br>Constructor:",constructorId))
i = ggplot(temp, aes(x=round, y = points, group = season, color = as.factor(season), text = text)) + geom_line() + 
  theme_bw() + labs(color = "Year", x = "Round", y = "Points") + 
  theme(axis.title = element_text(face = "bold"))
ggplotly(i, tooltip = "text")

###### home gp curse
##### take care of the multinational if have a chance
res <- read.csv("results.csv")
nat <- read.csv("nationalityToCountry.csv")
drivers <- read.csv("drivers.csv")
circuits <- read.csv("circuits.csv")
sche <- read.csv("schedule.csv")
constructors <- read.csv("constructors.csv")
dri_subset <- drivers %>% select(driverId, nationality)
cir_subset <- circuits %>% select(circuitId, Location.country)
sche_subset <- sche %>% select (circuitId, season, round)
res_subset <- res %>% select(position, driverId, constructorId, season, round)
const_subset <- constructors %>% select(constructorId, color)
summary <- full_join(res_subset, sche_subset, by=c("round","season"))
summary <- full_join(summary, dri_subset, by ="driverId")
summary <- full_join(summary, cir_subset, by ="circuitId")
summary <- full_join(summary, nat, by="nationality")
summary <- full_join(summary, constructors, by = "constructorId")
summary <- summary %>% mutate(home = Location.country == country)

temp <- summary %>% filter(driverId == driver)
j = ggplot(temp, aes(x=home, y= position)) + geom_boxplot(width = 0.25, outlier.color = NA) + 
  geom_jitter(width = 0.25,  color = temp$color, size = 0.5) + theme_bw() + 
  xlab("Home Grand Prix?") + ylab("Position") + theme(axis.title = element_text(face="bold"))

grid.arrange(a,b,c,d,e,f,g,h,i,j, layout_matrix = rbind(c(9,9,9,9,5,6,7,8),c(9,9,9,9,1,1,4,4),c(2,2,2,2,3,3,10,10)) )

##### age of driver #####
start <- as.Date(drivers$dateOfBirth[which(drivers$driverId==driver)])
end <- as.Date(Sys.Date(), format='%d/%m/%y')
floor(time_length(interval(start,end),"years"))

##### year of racing #####
length(unique(res$season[which(res$driverId==driver)]))

### WDC ###
WDC = list()
for(i in unique(driv_st$season)) {
  tmp = driv_st[which(driv_st$season==i),]
  max_round = max(tmp$round)
  tmp = tmp[which(tmp$round==max_round & tmp$position == 1),]
  print(tmp)
  WDC = c(WDC, tmp$driverId)
  print(WDC)
}
length(which(WDC==driver))

### current team ###
tmp <- full_join(res %>% select (driverId, constructorId, season), 
                 constructors %>% select(constructorId, name, color))
tmp <- tmp %>% filter(season == '2020') %>% distinct()
team <- tmp$name[which(tmp$driverId == 'lauda')]
if(length(team == 0 )) {
  team <- "N/A"
}
return(team)


