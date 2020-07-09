
Bl <- 2
id <- 16
Ans <- 1
No <- 'red'
Yes <- 'blue'


t <- Data %>%
  filter(Block == Bl & ID == id & Answer ==  Ans) 

t %>%
  filter(Trial.x == 8) %>%
  ggplot(aes(x = TrialTime, y = gy)) + 
  geom_point(color = Yes) + 
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))

ggsave('b2p16Yes8.pdf', width = 40, height = 28, units = "cm")


