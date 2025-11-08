
library(dplyr)

set.seed(123)
n_schools <- 50
students_per_school <- 60
n <- n_schools * students_per_school

# Criar data frame com uma linha por aluno
df <- data.frame(
  student_id = 1:n,
  school = rep(1:n_schools, each = students_per_school)
)

# Atribuição aleatória do grupo dentro de cada escola
df <- df %>%
  group_by(school) %>%
  mutate(group = sample(1:3, size = n(), replace = TRUE)) %>%
  ungroup()

# Criar variáveis de tratamento
df <- df %>%
  mutate(
    x1 = as.integer(group == 2),  # small class
    x2 = as.integer(group == 3)   # teacher's aide
  )

# Criar matriz de efeitos fixos por escola
school_factors <- model.matrix(~ factor(school), data = df)
school_factors <- school_factors[,-1]

# criando y
beta <- c(1, runif(49), 1.5, .75)
erro <- rnorm(n, 0, 3)
y <- cbind(rep(1, nrow(df)), school_factors, df$x2, df$x1) %*% beta + erro
fit <- lm(y ~ x1 + x2 + as.factor(school), data=df)
summary(fit)

vec_beta1 <- numeric()
vec_beta2 <- numeric()
for( i in 1:10000) {
  erro <- rnorm(n, 0, 3)
  y <- cbind(rep(1, nrow(df)), school_factors, df$x2, df$x1) %*% beta + erro
  sim_fit <- lm(y ~ x1 + x2 + as.factor(school), data=df)
  vec_beta1[i] <- coef(sim_fit)[2]
  vec_beta2[i] <- coef(sim_fit)[3]
  if(i %% 500 == 0) print(i)
}

mean(vec_beta1) - beta[52]
mean(vec_beta2) - beta[51]

# Agora, assignment é condicionalmente ignorável
# Atribuição aleatória do grupo dentro de cada escola
softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
}

library(randomizr)
set.seed(123)

n_schools <- 2
students_per_school <- 600
n <- n_schools * students_per_school

df <- data.frame(
  student_id = 1:n,
  school = rep(1:n_schools, each = students_per_school)
)



vec_prob <- list(c(.5, .05, .45), c(.1, .45, .45))

lista_tratamento <- list()

for (i in 1:n_schools)  {
  
  lista_tratamento[[i]] <- complete_ra(N = students_per_school, conditions = c("T1","T2","T3"), 
                            prob_each = vec_prob[[i]])  
}
aux <- unlist(lista_tratamento)

df <- df %>%
  mutate(tratamento = aux)


# Criar variáveis de tratamento
df <- df %>%
  mutate(
    x1 = as.integer(tratamento == "T2"),  # small class
    x2 = as.integer(tratamento == "T3")   # teacher's aide
  )

# Criar matriz de efeitos fixos por escola
school_factors <- model.matrix(~ factor(school), data = df)

# criando y
beta1 <- c(runif(1), 0, 0)
beta2 <- c(runif(1), 0, 1)
erro <- rnorm(n, 0, 3)
y <- cbind(school_factors[,1], df$x1, df$x2) %*% beta1 + 
  cbind( school_factors[,2], df$x1, df$x2)%*% beta2 +  erro
fit <- lm(y ~ x1 + x2 + as.factor(school), data=df)
summary(fit)

vec_beta1 <- numeric()
vec_beta2 <- numeric()
for( i in 1:2000) {
  erro <- rnorm(n, 0, 3)
  y <- cbind(school_factors[,1], df$x1, df$x2) %*% beta1 + 
    cbind( school_factors[,2], df$x1, df$x2)%*% beta2 +  erro
  sim_fit <- lm(y ~ x1 + x2 + as.factor(school), data=df)
  vec_beta1[i] <- coef(sim_fit)[2]
  vec_beta2[i] <- coef(sim_fit)[3]
  if(i %% 500 == 0) print(i)
}

mean(vec_beta1) - beta1[2]
mean(vec_beta2) - beta2[2]
