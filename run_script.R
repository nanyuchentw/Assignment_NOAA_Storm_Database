library(tidyverse)
temp <- tempfile()
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(URL, temp)
df <- read.csv(temp)
unlink(temp)
#or may directly read in the csv.bz2 file
df <- read.csv("repdata_data_StormData.csv.bz2")

#Data processing
#Calculate total property damage
df$PROPDMGEXP <- str_replace_all(df$PROPDMGEXP, "[Hh]", "2") 
df$PROPDMGEXP <- str_replace_all(df$PROPDMGEXP, "[Kk]", "3") 
df$PROPDMGEXP <- str_replace_all(df$PROPDMGEXP, "[Mm]", "6") 
df$PROPDMGEXP <- str_replace_all(df$PROPDMGEXP, "[Bb]", "9") 
df$PROPDMGEXP <- str_replace_all(df$PROPDMGEXP, "[?+-]", "0")
df$PROPDMGEXP[which(df$PROPDMGEXP=="")] <- "0"
df <- df %>% mutate(Prop_loss= as.numeric(PROPDMG)*(10^as.numeric(PROPDMGEXP)))

#calculate total crop damage
df$CROPDMGEXP <- str_replace_all(df$CROPDMGEXP, "[Kk]", "3") 
df$CROPDMGEXP <- str_replace_all(df$CROPDMGEXP, "[Mm]", "6") 
df$CROPDMGEXP <- str_replace_all(df$CROPDMGEXP, "[Bb]", "9")
df$CROPDMGEXP <- str_replace_all(df$CROPDMGEXP, "[?]", "0")
df$CROPDMGEXP[which(df$CROPDMGEXP=="")] <- "0"
df <- df %>% mutate(Crop_loss= as.numeric(CROPDMG)*(10^as.numeric(CROPDMGEXP)))

#calculate total economic loss
df <- df %>% mutate(Eco_loss= Crop_loss+ Prop_loss)

#find the type of events that cost the top 10 economic loss and then make a plot
p <- df %>% group_by(EVTYPE) %>% summarise(Total_loss= sum(Eco_loss), Crop_loss= sum(Crop_loss), Prop_loss= sum(Prop_loss)) %>% arrange(desc(Total_loss)) %>% slice_head(n=10)
p <- p %>% pivot_longer(Total_loss:Prop_loss, names_to = "Type", values_to = "Amount")
ggplot(p, aes(x=EVTYPE, y=Amount, fill=Type))+ geom_col(position="dodge")+
        labs(x= "Event type", y="Economic loss (dollars)" , title = "Top 10 event types with the greatest economic consequences")+
        coord_flip()


#Evaluate event types that are most harmful to population health
#Find the event types that caused greatest injuries
health_cost <- df %>% group_by(EVTYPE) %>% summarise(inj= sum(INJURIES), death=sum(FATALITIES), total=sum(INJURIES+FATALITIES)) %>% 
        arrange(desc(total)) %>% slice_head(n=10)

p <- health_cost %>% pivot_longer(inj:total, names_to = "Type", values_to = "Number")
ggplot(p, aes(x= EVTYPE, y=Number, fill=Type))+ geom_col(position="dodge")+
        labs(x="Event type", y="Injuries (person)", title= "Top 10 event types causing the greatest number of injuries/fatalities")+
        coord_flip()

max_inj <- health_cost[which(health_cost$inj== max(health_cost$inj)),1:2]
max_death <- health_cost[which(health_cost$death== max(health_cost$death)),c(1,3)]

max_inj
max_death
