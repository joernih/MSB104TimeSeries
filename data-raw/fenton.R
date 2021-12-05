# Libraries
library(dplyr)




w <- seq(1,20)
pop <- 10000000
m <- 500/pop

illusion <- data.frame(week=w,basepop=pop) %>% dplyr::mutate(population=basepop-5000*week)


illusion
