library(deSolve)
library(ggplot2)
library(gganimate)


#Código por Gabriel Zaffari! Use a vontande! Viva a ciência!
#goodwin modificado! ver drummond (2011)

#z = theta
Goodwin_modificado <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = (-(alpha + theta) + beta*y)*x
    dy = (-x/v-(theta+n)+1/v)*y
    dv = (z - theta)*v
    return(list(c(dx,dy,dv)))
  })
}

#z = (0,06) -- o que drummond chama de variação média de composição orgânica

Pars <- c(alpha =55.600, theta = 0.028, beta = 64.040, n = 0.011, z = 0.06)
State <- c(x = 0.60, y = 0.97, v = 2.590)
Time <- seq(0, 100, by = 1)
out <- as.data.frame(ode(func = Goodwin_modificado, y = State, parms = Pars, times = Time))

#gráfico das soluções
t_special2 = 5000
Time_special2 <- seq(0, t_special2, by = 1)
out_special2 <- as.data.frame(ode(func = Goodwin_modificado, y = State, parms = Pars, times = Time_special2))
plot_solution_gw = ggplot(out_special2, aes(x = out_special2$x, y = out_special2$y)) + geom_point() + 
  xlab("W/Y") +
  ylab("Taxa de Emprego") +
  labs(title = "Ciclo de Goodwin", subtitle = "Mapeando as Soluções do Modelo com Variação da Composição Orgânica Intensa", caption = "Elaborado por Gabriel Zaffari - Marxomática")

#evolução de x contra o tempo
plot_x_gw4 <- ggplot(out, aes(x = out$time, y = out$x)) + geom_path() +
  xlab("Anos") +
  ylab("W/Y") +
  labs(title = "Ciclo de Goodwin", subtitle = "Evolução da Participação do Salário da Renda no Tempo com Variação.\n da Composição Orgânica Intensa", caption = "Elaborado por Gabriel Zaffari - Marxomática")

#evolução de y contra o tempo
plot_y_gw4 <- ggplot(out, aes(x = out$time, y = out$y)) + geom_path() +
  xlab("Anos") +
  ylab("Taxa de Emprego") +
  labs(title = "Ciclo de Goodwin Modificado", subtitle = "Evolução da Taxa de Emprego no Tempo com Variação Intensa da Composição Orgânica de Capital", caption = "Elaborado por Gabriel Zaffari")


