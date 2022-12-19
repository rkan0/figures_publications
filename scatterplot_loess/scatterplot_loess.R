library(tidyverse)
library(beanplot)
library(multcomp)
library(lme4)


# create dataframe of 2 groups * 30 participants * 4 conditions 
matrix_apply <- data.frame(n = c(60, 60, 60, 60), 
                           a = c(80, 0, 0, 0), 
                           b = c(100, 100, 90, 50),
                           mean = c(80, 70, 30, 20), 
                           sd = c(5, 15, 15, 15))
plot_data <- apply(matrix_apply, 1, function(x) rtruncnorm(x[1], a=x[2], b=x[3], mean=x[4], sd=x[5])) %>% 
  as.data.frame()

plot_data$id <- rep(1:(nrow(plot_data)), each = 1, length.out = nrow(plot_data))
plot_data$group <- rep(1:2, each = 1, length.out = nrow(plot_data))
plot_data <- with(plot_data, plot_data[order(V4), ]) 
aotdf <- data.frame(aot = runif(n = 60, min = 4, max = 12)) %>%
  arrange(aot)
aotdf$id <- rep(1:60, each = 1)
plot_data <- inner_join(plot_data, aotdf, by = "id")


plot_data <- plot_data %>% 
  pivot_longer(V1:V4, names_to = "Condition", 
               values_to = "accuracy")

data_group1 <- subset(plot_data, group == 1)
data_group2 <- subset(plot_data, group == 2)

my.count <- seq(from = 1, to = nrow
                (data_group1[,1]), by = 1)
# group1
# split dataset by condition; run loess on each 'subset' and save output into a list of predictions
# set alpha (degree of smoothing) to 0.7
data_group1_split <- split(data_group1, data_group1$Condition) 
list_of_pred1 <- vector(mode = "list", length = length(data_group1_split))

for (i in 1:length(data_group1_split)) {     
  list_of_pred1[[i]] <- predict(
    loess(data_group1_split[[i]]$accuracy ~ 
            data_group1_split[[i]]$aot, span = 0.7), 
    my.count, se = TRUE)
}

# group2
data_group2_split <- split(data_group2, data_group2$Condition) 
list_of_pred2 <- vector(mode = "list", length = length(data_group2_split))

for (i in 1:length(data_group2_split)) {     
  list_of_pred2[[i]] <- predict(
    loess(data_group2_split[[i]]$accuracy ~ 
            data_group2_split[[i]]$aot, span = 0.7), 
    my.count, se = TRUE)
}


# plot with base R
par(mfrow = c(2,1)) 
par(xpd = NA)
par(mar = c(4.5, 4.1, 3.5, 8.8), xpd = TRUE) 
plot(data_group1$accuracy ~ data_group1$aot, 
     pch = c(2, 1 ,3, 4)[as.factor(data_group1$Condition)], 
     col = "black",
     xlim = c(3.7, 12.5), ylim = c(-5, 105), 
     ylab = "Accuracy (%)", xlab = "",
     xaxp = c(4, 12, 8), cex.lab = 1.2
     )
title(main = "Heritage speaker group", 
      line = 1, cex.main = 1.5)
legend("topright", inset = c(-.27, 0.7),
       title = "Classifier",
       c("go3", "zoeng1", "baa2", "tiu4"),
       pch = c(1:4), lty = c(1:4), seg.len = 4, text.font = 3)
box()

for (i in 1:length(list_of_pred1)) {        
  lines(list_of_pred1[[i]]$fit, lty = i, lwd = 2)
}


plot(data_group2$accuracy ~ data_group2$aot, 
     pch = c(2, 1 ,3, 4)[as.factor(data_group2$Condition)], 
     col = "black",
     xlim = c(3.7, 12.5), ylim = c(-5, 105), 
     ylab = "Accuracy (%)", xlab = "Age of testing (AOT)",
     xaxp = c(4, 12, 8), cex.lab = 1.2
)
title(main = "Hong Kong group", 
      line = 1, cex.main = 1.5)
box()

for (i in 1:length(list_of_pred2)) {        
  lines(list_of_pred2[[i]]$fit, lty = i, lwd = 2)
}

# beanplot
beanplot(accuracy ~ Condition, data = plot_data,
         col = "white", method = "jitter", bw = "nrd0")
text(c(1, 2), c(1), "AB", col = "blue", cex = 2)
text(c(3), c(1), "B", col = "purple", cex = 2)
text(c(4), c(1), "A", col = "turquoise", cex = 2)


# fit generalised linear model with multiple comparisons
# create dataframe of 2 groups * 30 participants * 4 conditions * 3 trials (binomial)
lm_df <- data.frame(
  response = sample(c(0, 1), replace = TRUE, 
                    size = 720, prob = c(0.3,0.7)),
  trial = rep(1:3, each = 1, length.out = 720), 
  condition = rep(1:4, each = 3, length.out = 720),
  id = rep(1:30, each = 12, length.out = 720),
  group = rep(1:2, each = 360)
)
lm_df %>%
  group_by(condition, response) %>%
  summarise(n = n())


pair1 <- glmer(response ~ condition + group + (1|id) + (1|trial), 
               data = lm_df, family = 'binomial')
print(summary(pair1, correlation = FALSE))
# random-effect variance estimated at (nearly) zero --> use glm
lm_df$condition <- as.factor(lm_df$condition)
pair2 <- glm(response ~ condition + group, 
               data = lm_df, family = 'binomial')
output1 <- summary(glht(pair2, linfct = mcp(condition = "Tukey")))
summary(output1)          # standard display
output1.cld <- cld(output1)   # letter-based display
opar <- par(mai=c(1,1,1.5,1))
plot(output1.cld) 
par(opar)

