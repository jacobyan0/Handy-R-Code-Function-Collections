## Delete NA
data<-data[complete.cases(data$attribute),]

### Correlation matrix
cor(data,use="complete.obs")

### Histograms for all variables
library(purrr)
library(tidyr)
library(ggplot2)

WA_HousingVar %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

### scattorplots for all pairs
pairs(WA_HousingVar[,c("x1","x2")],pch=5,lower.panel = NULL)
