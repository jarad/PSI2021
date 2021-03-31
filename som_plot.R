library("tidyverse")

f = function(depth, rate, intercept) {
  intercept * exp(-rate*depth)
}

d = expand.grid(depth = seq(0, 200, length = 1001),
                rate  = c(2,1,1.5)/50,
                intercept = c(2, 4, 5)) %>%
  mutate(som = f(depth, rate, intercept) + 0.1,
         yield = intercept / rate + 200,
         yield = yield / max(yield))

ggplot(d, aes(x = depth, y = som, 
              color = yield, group = rate+intercept)) + 
  geom_line() + 
  theme_bw() + 
  theme(legend.position = c(0.8,0.6)) + 
  labs(x = "Depth (cm)" ,
       y = "Soil organic matter (%)",
       title = "Computer model predicted relative yield")
