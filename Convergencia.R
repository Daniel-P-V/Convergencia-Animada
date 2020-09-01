library(reshape2)
library(ggplot2)
library(gganimate)
library(gifski)

# Parameters
a <- 1/3
g <- 0.2
d <- 0.1
n <- 0.02
dp <- n+d

# Stacionary State
ke <- (g/(dp))^{1/(1-a)}
ye <- (g/(dp))^{a/(1-a)}

# Data1
k <- seq(0, 4, by = 0.1)
y <- k^{a}
s <- g*y
d <- (dp)*k
d1 <- data.frame(k, y, s, d)
d1 <- melt(d1, id = "k", variable.name = "Curvas", value.name = "y")
# Data2
kee <- seq(0.5, ke, by = 0.1)
yee <- kee^{a}
see <- g*yee
dee <- (dp)*kee
d2 <- data.frame(kee, yee, see, dee)
d2 <- melt(d2, id = "kee", variable.name = "Puntos", value.name = "y")

# Base plot
plot1 <- ggplot(data = d1, aes()) +
  geom_line(size = 1, aes(x = k, y = y, group = Curvas, color = Curvas)) +
  scale_colour_manual(values = c("orangered3", "blue4", "#66CC99")) +
  geom_segment(aes(x = ke, y = 0, xend = ke, yend = ye), linetype = 'dashed') +
  geom_segment(aes(x = 0, y = ye, xend =ke, yend = ye), linetype = 'dashed') +
  geom_point(color = "black", size = 2, aes(x = ke, y = ye)) +
  geom_point(color = "black", size = 2, aes(x = ke, y = g*ye)) +
  geom_point(color = "black", size = 2, aes(x = 0.5, y = 0.5^{a})) +
  geom_point(color = "black", size = 2, aes(x = 0.5, y = g*0.5^{a})) +
  geom_point(color = "black", size = 2, aes(x = 0.5, y = dp*0.5)) +
  theme_bw()

# Animation 
animacion <- plot1 + geom_point(data = d2, aes(x = kee, y = y, group = Puntos),
                                color = "red", size = 3) +
  transition_states(kee, transition_length = 2, state_length = 1) +
  labs(title = 'Periodo: {previous_state}') 

# Ver
animacion

# Export (Optional)
anim_save("Convergencia.gif", animacion, width = 600, height = 500)
