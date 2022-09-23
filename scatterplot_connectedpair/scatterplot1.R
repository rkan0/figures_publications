library(tidyr)
library(dplyr)
library(ggplot2)
library(lme4)

data <- read.csv("C:\\Users\\user\\Desktop\\scatterplot1_data.csv")
data$group <- as.factor(data$group)
data_long <- data %>%
  gather(id, response, Q1:Q30, factor_key=TRUE) %>%
  drop_na(response)

data2 <- data_long %>%
  group_by(id, group) %>%
  dplyr::summarise(mean=sprintf("%0.2f", mean(response))) %>%
  spread(group,mean)

data2$cond <- ifelse(data2$groupA >= data2$groupB, "groupA>=groupB", "groupB>groupA")
data2 <- gather(data2, group, response, groupA:groupB, factor_key=TRUE)

ggplot(data2, aes(x = id, y = as.numeric(response), color = group)) +
  geom_point()+
  scale_color_manual(values = c("darkblue", "orange")) + 
  theme_classic() + 
  ylab("mean response") + xlab("question") + scale_x_discrete(breaks=NULL) +
  scale_y_continuous(limits = c(3, 8), breaks = seq(3, 8, by = 1)) +
  new_scale_color() + 
  geom_line(aes(group = id, color = cond)) +
  scale_color_manual(values = c("darkblue", "orange"))


# question-by-question comparison between the two groups 

ls_data<-unique(data_long$id)
for (i in 1:length(ls_data)){
  temp <- subset(data_long, id==ls_data[i])
  print(ls_data[i])
  print(summary(lm(response ~ group, data=temp))$coefficients, digits = 2)
}



# assume that each question corresponds to a product, and the response is a rating.
# each product is categorised as receiving 'high' or 'low' ratings, 
# depending on the average response for each product relative to the average response for all products.
# (the products are categorised for the two groups separately.)
# is the rating for each product the same across the two groups? 

data_bygroup <- data %>% 
  gather(id, response, Q1:Q30, factor_key=TRUE) %>% 
  group_by(id, group) %>%
  dplyr::summarise(mean=sprintf("%0.2f", mean(response, na.rm=TRUE))) %>%
  drop_na(mean) # average response by group


# average response across all groups for reference
aa <- data %>% 
  gather(id, response, Q1:Q30, factor_key=TRUE) %>% 
  group_by(id) %>%
  dplyr::summarise(mean_all=sprintf("%0.2f", mean(response, na.rm=TRUE))) %>%
  drop_na(mean_all) 
data_bygroup <- merge(data_bygroup, aa, all.x = T) 

data_bygroup %>% 
  group_by(group) %>%
  dplyr::summarise(mean=sprintf("%0.2f", mean(as.numeric(mean))))


# use the values from the output above
data_bygroup$ranking <- ifelse(data_bygroup$group == "groupA" & data_bygroup$mean > 5.46 | 
                                 data_bygroup$group == "groupB" & data_bygroup$mean > 5.50, 
                     "high", "low") # get ranking within each group

data_bygroup <- data_bygroup[c(1, 2, 3, 5, 4)]

bb <- data_bygroup %>% 
  tidyr::unite(temp, mean:ranking, sep = "_") %>% 
  spread(group, temp) %>% 
  separate(groupA, c("groupA_mean", "groupA_ranking"), sep = "_") %>% 
  separate(groupB, c("groupB_mean", "groupB_ranking"), sep = "_")

bb$check <- ifelse(bb$groupA_ranking == bb$groupB_ranking, 
                   "match", "not_match")
View(bb)

# count products with same ranking
xtabs(~ check, bb)

# print products with different ranking
filter(bb, check == 'not_match')
