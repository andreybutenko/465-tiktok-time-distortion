library(dplyr)
library(ggplot2)
library(ggthemes)

# 1 Data Cleaning ----

distortion <- read.csv('distortion_raw.csv', stringsAsFactors = F)
colnames(distortion) <- c(
  'tiktok.length.usage', 'tiktok.weekly.usage', 'tiktok.daily.usage',
  'age', 'gender', 'group',
  'perceived.time', 'actual.time', 'time.distortion'
)
write.csv(distortion, 'distortion_clean.csv', row.names = F)

# 2 Helpers ----

distortion <- read.csv('distortion_clean.csv', stringsAsFactors = F)

control <- distortion %>% 
  filter(group == 'with clock')

experimental <- distortion %>% 
  filter(group == 'without clock')

# 3 Averages ----

distortion %>% 
  group_by(group) %>% 
  summarize(
    estimated_mean = mean(perceived.time),
    estimated_sd = sd(perceived.time),
    actual_mean = mean(actual.time),
    actual_sd = sd(actual.time),
    distortion_mean = mean(time.distortion),
    distortion_sd = sd(time.distortion),
  )

# 4 T-Testing ----

t.test(control$time.distortion, experimental$time.distortion, conf.level = 0.95)

# 5 Linear Regression ----

distortion %>%
  rowwise() %>% 
  mutate(group = ifelse(group == 'with clock', 'control (with clock)', 'treatment (without clock)')) %>% 
  ggplot(aes(x = age, y = time.distortion, color = group)) +
  geom_point(size = 5) +
  labs(
    title = 'Time Distortion by Age',
    x = 'Age',
    y = 'Time Distortion',
    color ='Group'
  )

lm(time.distortion~age, data = distortion) %>% 
  summary()


lm(time.distortion~age, data = control) %>% 
  summary()

lm(time.distortion~age, data = experimental) %>% 
  summary()
