library(tidyverse)
options(scipen = 999)
theme_set(theme_classic())

# =========== SIMULANDO EL PROBLEMA DE MONTY HALL =======

# función replicate
replicate(10, {
  "hola"
})

# definir puertas
puertas <- 1:3

# Replicando problema
replicate(1000, {
  
  puerta_correcta <- sample(puertas, 1)
  puerta_electa <- sample(puertas, 1)
  
  no_cambio <- puerta_correcta == puerta_electa
  
  restantes <- puerta_correcta[puerta_correcta != puerta_electa]
  
  cambio <- puerta_correcta %in% restantes
  
  c("No cambiar de puerta" = no_cambio,
    "Cambiar de puerta"= cambio)
}) %>%
  t() %>%
  as.data.frame() %>%
  gather("eleccion", "resultado", 1:2) %>%
  group_by(eleccion) %>%
  summarize(exito = sum(resultado)/n()) -> monty_data

# Probalidad teórica 2/3, no cambiar 1/3
monty_data

rm(puertas, monty_data)


# ============== PROBLEMA DEL CUMPLEAÑOS ==============

# año de 365 días
# no gemelos
# días de nacimiento equiprobables 

simulaciones <- list()

for (i in 1:100) {
  
  replicate(1000,{
    
    personas <- sample(1:365, i, replace = T)
    
    hay_repetidos <- length(unique(personas)) < length(personas)
    
    hay_repetidos
    
  }) %>%
    sum()/1000 -> p.obs
  
  simulaciones[[i]] <- c("n" = i, "p.obs" = p.obs)
  
}

rm(i, p.obs)

datos <- do.call("rbind", simulaciones) %>% as.data.frame()

# formula teórica = 1 - nCn-k * k!/365^k, P(n=23) = 50.7%

datos$p.real <- 1 - (choose(365, 365 - datos$n) * factorial(datos$n)/365 ^ datos$n)

#graficando
ggplot(datos) +
  aes(x = n) +
  geom_line(aes(y = p.real, color ="Teórica")) +
  geom_point(aes(y = p.obs, color = "Observada")) +
  labs(x="N personas", y= "P(Cumpleaños mismo día)", 
       color = "Probabilidad")

rm(datos, simulaciones)


# ================== ENTENDIENDO CONCEPTOS ESTADÍSTICOS ============
# P VALOR

pop.var <- function(x){
  
  mu <- mean(x)
  
  ((x - mu)^2) %>%
    sum() -> square.diff
  
  var <- square.diff/length(x)
  
  return(
    var
  )
}


# crear vector con poblaciones
set.seed(1234)
cl <- rnorm(10000, 3, .7) %>% round(2)
set.seed(1234)
ar <- rnorm(10000, 2, 1) %>% round(2)

# pasamos poblaciones a DF
cl.df <- data.frame(nac= rep("cl", 10000), otakismo = cl)
ar.df <- data.frame(nac= rep("ar", 10000), otakismo = ar)
pop <- rbind(cl.df, ar.df) %>% as.data.frame()

rm(cl.df, ar.df)

# vector nulo
set.seed(1234)
null <- replicate(10000, {
  a <- sample(pop$otakismo, 30) %>% mean()
  b <- sample(pop$otakismo, 30) %>% mean()
  
  a-b
}) 

# parametros poblacionales de distribución nula
mean(null)
pop.var(null)
sqrt(pop.var(null))

# muestreos o experimentos
set.seed(1234)
replicate(1000, {
  
  chi <-sample(pop$otakismo[pop$nac=="cl"], 30) %>% mean()
  arg <-sample(pop$otakismo[pop$nac=="ar"], 30) %>% mean()
  
  chi - arg
  
}) -> experimentos

# graficar diferencia
ggplot() +
  geom_density(aes(null, color = "Nula")) +
  geom_density(aes(experimentos, color = "Observada")) +
  labs(x="Diferencias medias", y= "Densidad", 
       color = "Distribuciones")

# diferencia media observada
mean(experimentos)

# p.valor
pnorm(
  mean(experimentos),
  mean = mean(null), 
  sd = sqrt(pop.var(null)),
  lower.tail = F
)

# curva p
replicate(10000, {
 
  chilenos <- sample(pop$otakismo[pop$nac=="cl"], 30) %>% mean()
  argentinos <- sample(pop$otakismo[pop$nac=="ar"], 30) %>% mean()
  
  dif <- chilenos - argentinos
  
  pnorm(dif, mean(null), sd(null), lower.tail = F)
  
}) -> p.valores


# graficando curva p
ggplot() +
  geom_density(aes(p.valores)) +
  geom_vline(xintercept = .05, color = "red") 

rm(experimentos, p.valores, pop)


# INTÉRVALO DE CONFIANZA

# formula = x_hat ± z * SE

# z
qnorm(1 - .05/2) -> z 

# forma alternativa
# qnorm(.05/2, 
#       lower.tail = F)


1 - pnorm(z, 
      lower.tail = F) * 2


# replicando experimentos y sus SE
pop.mean.diff <- abs(mean(ar) - mean(cl))

pop.mean.diff

diff_se <- function(a, b){
  
  se_a <- var(a)/(length(a)-1)
  se_b <- var(b)/(length(b)-1)
  
  se_dif <- sqrt(se_a + se_b)
}

iteraciones <- 100

replicate(iteraciones, {
  
  s.cl <- sample(cl, 30)
  s.ar <- sample(ar, 30)
  
  x_hat <- abs(mean(s.cl) - mean(s.ar))
  
  se <- diff_se(s.cl, s.ar)
  
  c(x_hat, se)
}) %>%
  t() %>%
  as.data.frame() %>%
  rename("x_hat" = V1, "se" = V2) -> mean.diff

# asignando si intérvalo contiene la media poblacional
mean.diff <- mean.diff %>%
  mutate(contiene = (x_hat - z * se) < pop.mean.diff &
                    (x_hat + z * se) > pop.mean.diff )

# contando cantidad de veces que intérvalo contuvo la media pob
contiene_intervalo <- sum(mean.diff$contiene)

contiene_intervalo

ggplot(mean.diff) +
  aes(x=1:iteraciones, y = x_hat, color = ifelse(contiene,
                                         "Si", "No")) +
  geom_point() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 3)) +
  geom_pointrange(aes(ymin = x_hat - z * se,
                      ymax = x_hat + z * se)) +
  geom_hline(yintercept = pop.mean.diff, color = "red") +
  labs(x="Iteraeciones",
       y = "Diferencias medias",
       color = "Intérvalo contiene \nmedia poblacional",
       subtitle = paste("Contiene intérvalo en: ", 
                        contiene_intervalo, "de", iteraciones) )

rm(cl, ar, mean.diff, iteraciones, contiene_intervalo, null,
   z, diff_se, pop.var, pop.mean.diff)

# ==================== SIMULANDO ESCENARIOS DEL MUNDO REAL ============

# DESIGUALDAD NATURAL ??

library(ineq)

base <- rep(1000, 1000)

simulaciones <- list()

tiempos <- 100

for (i in 1:tiempos) {
  simulaciones[[i]] <- base
  base <- base * (1 + runif(1000, 0.0000001, 1))
}

rm(i, base)

datos <- do.call("rbind", simulaciones) %>% 
  t() %>% 
  as.data.frame() %>% 
  gather("simulation", "value", 1:tiempos)

datos$simulation <- str_replace_all(datos$simulation, "V", "") %>%
  as.numeric()

ginis <- datos %>%
  group_by(simulation) %>%
  summarize(gini = ineq(value, type = "Gini"))

ggplot(ginis, aes(simulation, gini)) +
  geom_line() +
  labs(x="Nº simulación", y ="Gini")


rm(datos, lorenz, simulaciones, ginis, tiempos)


# LOTOCRACIA

# tamaño población
n = 100000

# simulando minoría segregada en una población o concentrada en un 
# mismo "estrato"
# La minoría representa el 10% total de la población

set.seed(1234)
data <- data.frame(person = 1:n, 
                   strata = c(rep("strat1", n*.5),
                              rep("strat2", n*.4),
                              rep("strat3", n*.1)),
                   minoria = c(rbinom(n*.5, 1, ((n*.1)*.05)/(n*.5)),
                               rbinom(n*.4, 1, ((n*.1)*.15)/(n*.4)),
                               rbinom(n*.1, 1, ((n*.1)*.8)/(n*.1))))

sum(data$minoria)/n # % observado

data %>%
  group_by(strata) %>%
  summarize(p= sum(minoria)/n()) %>%
  ggplot(aes(strata, p, fill = strata)) +
  geom_col() +
  guides(fill="none") +
  scale_y_continuous(labels = scales::percent) +
  labs(x="", y="")

# cantidad de escaños a repartir
escaños <- 100

# Asignando escaños bajo 2 tipos de sorteo
set.seed(1234)
replicate(1000, {
  
  temp.dat <- sample_n(data, escaños)
  sum(temp.dat$minoria)/nrow(temp.dat)
  
}) -> simple


set.seed(1234)
replicate(1000, {
  
  temp.dat_1 <- sample_n(data[data$strata=="strat1",], escaños * .5)
  temp.dat_2 <- sample_n(data[data$strata=="strat2",], escaños * .4)
  temp.dat_3 <- sample_n(data[data$strata=="strat3",], escaños * .1)
  
  temp.dat <- bind_rows(temp.dat_1, temp.dat_2, temp.dat_3)
  
  sum(temp.dat$minoria)/nrow(temp.dat)
  
}) -> estratificado


# graficamos caso minoría aleatoria
ggplot() +
  geom_density(aes(estratificado, color= "estratificado", y=..scaled..)) +
  geom_density(aes(simple, color= "simple", y=..scaled..)) +
  geom_vline(xintercept = .1)  +
  scale_x_continuous(limits = c(0,.5),
                     breaks = seq(0, .5, .1),
                     labels = scales::percent)+
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  labs(x="% escaños de minorías por iteración",y="", 
       caption = paste0("N= ", n, ", n= ", escaños, ", rep= 1000"),
       title = "Representación de minoría")


rm(escaños, n, data, simple, estratificado)

