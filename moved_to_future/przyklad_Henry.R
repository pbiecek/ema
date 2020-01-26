
henry_neighbors <- select_neighbours(titanic[titanic$class == "1st",], 
                                     henry, 
                                     n = 10, 
                                     variables = c("class", "gender"))

henry_neighbors <- rbind(henry, henry_neighbors[,colnames(henry)])
rownames(henry_neighbors)[1] <- "Henry"


cp <-ceteris_paribus(explain_rf_v6, henry_neighbors, variables = "class", )

library(ggplot2)

library(tidyr)
library(dplyr)
cp %>% select(class, `_yhat_`, `_ids_`) %>%  
  spread(class, `_yhat_`) %>% na.omit() %>%
  gather(class, `_yhat_`, -c(`1st`, `_ids_`)) -> cp2

ggplot(cp, aes(class, `_yhat_`)) +
  geom_col() + facet_wrap(~`_ids_`) + 
  coord_flip()

cp2$`_ids_` <- reorder(cp2$`_ids_`, cp2$`1st`, mean)

ggplot(cp2, aes(`_ids_`, y = `1st`, ymin = `1st`, ymax = `_yhat_`, color = factor(sign(`1st`-`_yhat_`)))) +
  geom_errorbar(width = 0) +
  geom_point() + 
  facet_wrap(~class) + theme_drwhy_vertical() + 
  coord_flip() + xlab("neighbour_id") + ylab("prediction") + 
  scale_color_manual("prediction would",values = DALEX:::theme_drwhy_colors(2), labels=c("increase","decrease")) + 
  ggtitle("Local Fidelity Plot", "for titanic_rf_v6 model and Henry's neighbours")
