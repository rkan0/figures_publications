library(tidyverse)
library(ggplot2)
library(gridExtra)
library(truncnorm)

# create df with 3 target tones * 10 tokens * 10 F0 measurements
matrix_apply <- data.frame(n = c(100, 100, 100), 
                           a = c(230, 210, 210), 
                           b = c(280, 275, 240),
                           mean = c(255, 235, 220), 
                           sd = c(15, 15, 5))
data_df <- apply(matrix_apply, 1, function(x) rtruncnorm(x[1], a=x[2], b=x[3], mean=x[4], sd=x[5])) %>% as.data.frame()

data_df <- data_df %>% 
  rename(
    tone1 = V1,
    tone3 = V2,
    tone6 = V3
  ) %>%
  pivot_longer(tone1:tone6, names_to = "target_tone", 
               values_to = "f0")

data_df <- with(data_df, data_df[order(target_tone), ]) 
data_df$token <- rep(1:(nrow(data_df) / 10), each = 10)
data_df <- with(data_df, data_df[order(token, -f0), ]) 
data_df$normtime <- rep(1:10, each = 1, length.out = nrow(data_df))
min <- data.frame(token = 1:30, min = sample(c(200:220), size = 6, replace = T))
data_df <- inner_join(data_df, min, by = "token")


# calculate chao tone letter with 'global' normalisation (min = 190 Hz)
df_a <- data_df %>% 
  mutate(logtone = log(f0)) %>% 
  mutate(logmin = log(194)) %>%
  mutate(diff = logtone - logmin) %>%
  mutate(ST = diff * 39.86) %>%
  mutate(add = ST/2.5) %>%
  mutate(chao = add + 1)

df_a$target_tone <- as.factor(df_a$target_tone)
tone_lab <- c(
  'tone1' = "T1",
  'tone3' = "T3",
  'tone6' = "T6"
)
# horizontal lines showing conventional Chao values
convent <- data.frame(target_tone = c("tone1", "tone3","tone6"), Z = c(5, 3, 2)) 

p1 <- ggplot(df_a) +
  geom_path(aes(x = normtime, y = chao, group = token)) +
  facet_grid(. ~ target_tone ,labeller = labeller(target_tone = tone_lab)) +
  geom_hline(data = convent, aes(yintercept = Z), size = 1.8) +
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9), limits = c(0.5, 7.5)) +
  scale_x_continuous(breaks = c(1, 10)) +
  theme_classic() +
  labs(title = "", tag = "A") +
  ylab("Chao tone letter") + xlab("Normalised time") +
  theme(
    legend.position="none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, margin = margin(b = 10))
    )


# calculate chao tone letter with local minimalisation
df_b <- data_df %>% 
  mutate(logtone = log(f0)) %>% 
  mutate(logmin = log(min)) %>% 
  mutate(diff = logtone - logmin) %>%
  mutate(ST = diff * 39.86) %>%
  mutate(add = ST/2.5) %>%
  mutate(chao = add + 1)
  
ff$tone <- as.factor(ff$tone)


p2 <- ggplot(df_b) +
  geom_line(aes(x = normtime, y = chao, group = token)) +
  facet_grid(. ~ target_tone,labeller = labeller(target_tone = tone_lab) ) +
  geom_hline(data = convent, aes(yintercept = Z), size = 1.8) +
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9), limits = c(0.5, 7.5)) +
  scale_x_continuous(breaks = c(1, 10)) +
  theme_classic() +
  labs(title = "", tag = "B") +
  ylab("") + xlab("Normalised time") +
  theme(
    legend.position="none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(), 
    strip.text.x = element_text(size = 12, margin = margin(b = 10))
    )

grid.arrange(p1, p2, ncol = 2)



# plot B without facet
ggplot(df_b) +
  geom_path(aes(x = normtime, y = chao, group = interaction(target_tone, token), color = target_tone)) +
  geom_point(aes(x = normtime, y = chao, shape = target_tone, color = target_tone), size = 1)+
  theme_classic() 

