library(dplyr)
library(ggplot2)
library(multcomp)
library(truncnorm)
library(Rmisc)

# generate df with n = 50 and 7 additional variables
matrix_apply <- data.frame(n = c(50, 50, 50, 50, 50, 50, 50), 
                           a = c(0, 0, 0, 0, 0, 0, 70), 
                           b = c(100, 100, 10, 100, 100, 10, 110),
                           mean = c(80, 60, 8, 60, 70, 9, 90), 
                           sd = c(15, 15, 1, 15, 15, 1, 20))

data_df <- apply(matrix_apply, 1, function(x) rtruncnorm(x[1], a=x[2], b=x[3], mean=x[4], sd=x[5])) %>% as.data.frame()

data_df <- data_df %>% 
  rename(
    actual_NF = V1,
    reported_NF = V2, 
    time_NF = V3, 
    actual_F = V4, 
    reported_F = V5, 
    time_F = V6, 
    BPVS_norm = V7
  )

# separate into 2 language groups, add unique ID
data_df = cbind(data_df, sample(c(1,2), size = nrow(data_df), replace = TRUE))
names(data_df)[8] <- "langgp"
data_df <- data_df %>%
  mutate(ID = row_number()) 
data_df %>% dplyr::group_by(langgp) %>%
  summarise(ID = n())

t.test (BPVS_norm ~ langgp, data = data_df)

data_df %>% group_by(langgp) %>%
  summarise(mean = sprintf("%0.2f", mean(BPVS_norm)), sd = sprintf("%0.2f",sd(BPVS_norm)))


# original analysis used repeated measures ANCOVAs with posthoc
# fixed effects only linear model
data_long <- gather(data_df, test_type, score, 
                    actual_NF:time_F, factor_key = TRUE)
data_long2 <- subset(data_long, test_type == "actual_NF"|test_type == "actual_F")
anova(lm(score ~ langgp * test_type + BPVS_norm, data = data_long2))
# posthoc
data_long2$in1 <- interaction(data_long2$langgp, data_long2$test_type)
posthoc<- lm(score ~ in1, data = data_long2)
summary(glht(posthoc, linfct = mcp(in1 = "Tukey")), test = adjusted("holm"))

# figure
data_figure <- data_long %>%
  separate((test_type), sep = "_", into = c("scoretype", "testtype")) %>%
  subset(scoretype != "time")

langgp_names <- c(
  '1' = "Monolingual",
  '2' = "EAL"
)

data_figure2 <- Rmisc::summarySE(data_figure, measurevar="score", 
                                 groupvars=c("scoretype", "langgp", "testtype"))
data_figure2$testtype <- factor(data_figure2$testtype, levels = c("NF", "F"))
ggplot(data_figure2, 
       aes(x = scoretype, y = score, fill = factor(testtype))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_grid(~langgp, labeller = as_labeller(langgp_names)) +
  geom_errorbar(aes(ymin = score-sd, ymax = score+sd), size = 0.5,   
                width = 0.25, position = position_dodge(0.9)) +
  xlab("Score Type") + 
  ylab("Mean Reading Comprehension Score (%) ± s.d.") +
  theme_classic() +
  scale_fill_manual(values = c("grey", "white"), 
                    name = c("Test Type"), 
                    labels = c("Non-formulaic","Formulaic")) +
  ylim(c(0,100)) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14),
    axis.text.x = element_text(vjust = 0.5, face = "italic", size = 14), 
    axis.text.y = element_text(size = 11), 
    axis.title.y = element_text(margin = margin(r = 8), size = 14),
    axis.title.x = element_text(margin = margin(t = 10), size = 14))

