data = read.csv("../../data/FluNetInteractiveReport_2007_2019.csv", skip = 2) %>%
  filter(! Year %in% c(2007,2008,2009))
write.csv(data,"../../data/FluNetInteractiveReport_2010_2019.csv",row.names=F)

data %>% glimpse()