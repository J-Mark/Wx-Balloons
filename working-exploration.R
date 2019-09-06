df %>% group_by(DAY) %>%
  filter(HOUR == 12 & DAY == 1) %>%
  ggplot(aes(x = ((TEMP/10)-272.15), y= (CALCGPH/3.28084))) +
  geom_point(aes(col=MONTH))
