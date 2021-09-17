## Loading libraries and data
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
tuesdata <- tidytuesdayR::tt_load('2021-09-14')
tuesdata <- tidytuesdayR::tt_load(2021, week = 38)
billboard <- tuesdata$billboard

## Filtering for only data for the #1 songs
billboard_1 <- billboard %>% filter(billboard$week_position == 1 )

## Getting weeks with a song at #1 per listed artist
## Not sure how to get all of an artists' songs under one performer title automatically so e.g. Mariah Carey
##  gets all of her #1 song weeks; she's missing ~20 or so here, probably from performing with other artists.
##  Would love to know of possible solutions for this!
countbillboard_1 <- count(billboard_1, performer)

#bbtable  <- data.table(countbillboard_1)
#bbtable[performer %like% "Mariah Carey"]

bb1s <- countbillboard_1 %>% separate(performer, into = c("performer 1", "performer 2"), sep = "&")

## This is convoluted, but for the sake of factoring in all separators, I create 4 extra 'artist' columns
##  based on the separators  found in the data, trim the white space, then aggregate the results.
##  This maybe would have worked better by separating the artists before getting the 'count'
##  but I'm glad to see it's possible to work backwards with the pre-written code!
bb1s <- bb1s %>% separate('performer 1', into = c("performer 1", "performer 3"), sep = "Featuring")
bb1s <- bb1s %>% separate('performer 1', into = c("performer 1", "performer 4", "performer 5"), sep = "/")
bbs_final <- bb1s %>% pivot_longer(
  col = starts_with("performer"),    
  names_to = "olperf",
  values_to = "artist",
  values_drop_na = TRUE
)
bbs_final$artist <- str_trim(bbs_final$artist)
bb_agg <- aggregate(bbs_final$n, by=list(Artist=bbs_final$artist), FUN=sum)
str(bb_agg)

##Sorting by #1 song weeks, descending
sortbb <- bb_agg[order(-bb_agg$x),]

## Selecting the top 10 values (i.e. top 10)
bb1 <- head(sortbb, 10)


## ggplot bar graph based on #1 song weeks, with a gradient. Red = more beacaus it's the HOT 100!
billboardplot <- ggplot(bb1, aes(x=reorder(Artist, -x), y = x, fill=x)) +
  geom_col()+
  scale_fill_gradient(low = "#485BB3", high = "#FF8F00") +
  labs(title = 'Top Performers', subtitle =  '(Cumulative Weeks with Song at #1)')+
  xlab('Performer')+
  ylab('Weeks with Song at #1')+
  theme(legend.title=element_blank())+
  theme(axis.text.x=element_text(angle=45, hjust=1))

billboardplot
ggsave("billboardplot.png")
