library(tidyverse)
df <- read.csv("repdata_data_StormData.csv.bz2")

#Data processing
#Calculate total property damage
df$PROPDMGEXP <- str_replace_all(df$PROPDMGEXP, "[Hh]", "2") 
df$PROPDMGEXP <- str_replace_all(df$PROPDMGEXP, "[Kk]", "3") 
df$PROPDMGEXP <- str_replace_all(df$PROPDMGEXP, "[Mm]", "6") 
df$PROPDMGEXP <- str_replace_all(df$PROPDMGEXP, "[Bb]", "9") 
df$PROPDMGEXP <- str_replace_all(df$PROPDMGEXP, "[?+-]", "0")
df$PROPDMGEXP[which(df$PROPDMGEXP=="")] <- "0"
df <- df %>% mutate(total_PROPDMG= as.numeric(PROPDMG)*(10^as.numeric(PROPDMGEXP)))

#calculate total crop damage
df$CROPDMGEXP <- str_replace_all(df$CROPDMGEXP, "[Kk]", "3") 
df$CROPDMGEXP <- str_replace_all(df$CROPDMGEXP, "[Mm]", "6") 
df$CROPDMGEXP <- str_replace_all(df$CROPDMGEXP, "[Bb]", "9")
df$CROPDMGEXP <- str_replace_all(df$CROPDMGEXP, "[?]", "0")
df$CROPDMGEXP[which(df$CROPDMGEXP=="")] <- "0"
df <- df %>% mutate(total_CROPDMG= as.numeric(CROPDMG)*(10^as.numeric(CROPDMGEXP)))

#calculate total economic loss
df <- df %>% mutate(eco_loss= total_CROPDMG+ total_PROPDMG)

#find the type of events that cost the top 5 economic loss and then make a plot
p <- df %>% group_by(EVTYPE) %>% summarise(Total_loss= sum(eco_loss)) %>% arrange(desc(Total_loss)) %>% slice_head(n=5)
ggplot(p, aes(x=EVTYPE, y=Total_loss))+ geom_col()

