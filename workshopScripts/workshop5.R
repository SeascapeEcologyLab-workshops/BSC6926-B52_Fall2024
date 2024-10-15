#' """ Workshop 5: Density dependent models
#'     @author: BSC 6926 B52
#'     date: 10/14/2023"""

#This workshop cover density dependent population growth models. In these set of models, 
#the change in population size between time points is influenced by the number of 
#individuals in the population. This script will give examples of how to 
#model discrete density dependent population growth models.  

## Discrete density dependent growth models

# The basic formula for the discrete density dependent growth model is represented by 
# $$N_{t+1} = N_t + r_dN_t(1-(N_t/K))$$

#   where $N_t$ is the population size at time $t$, $K$ is the carrying capacity of the population, and $r_d$ is the discrete growth factor. 
#   It is important to know that 
# $r_d = \lambda -1$. \

# We can use a `for` loop to calculate discrete density dependent growth 
library(tidyverse)

N_0 = 10 #Starting population size
years = 50 #fifty time steps
pop = tibble(time = 0:years,
             Nt = NA)
pop$Nt[pop$time == 0] = N_0
pop

rd = 1.2 #geometric growth factor
K = 900 #carrying capacity

#Discrete logistic model using for loop 
for(i in 2:length(pop$time)){
  pop$Nt[i] = pop$Nt[i - 1]  + rd*pop$Nt[i - 1]*(1 - pop$Nt[i - 1] / K)
}
pop

ggplot(pop, aes(time, Nt))+
  geom_line(linewidth = 1) +
  labs(x = 'Time', y = expression(italic(N[t])))+
  theme_bw()

#Since we have many arguments, it might make sense for us to use a custom function for this model. 

## Custom functions
#So far we have used functions that are apart of base R or from different packages, 
#but we can also build custom functions. Custom functions are useful when existing functions will not do the task at hand, 
#or when combining functions over multiple times. These can be used for vector operations.

#The basic structure is 
# function_name = function(arguments){
#   code_here
#   return(output)
# }

# The `arguments` can be empty and therefore required, or set equal to a value. If set to a value, the default is that value, but a new value can be indicated that will override the default value.

# custom funciton 
my_fx = function(x){
  b = x * 23
  return(b)
}

my_fx(5)

t = seq(2,10,2)
my_fx(t)

# custom function with multiple arguments
my_fx2 = function(x,y = 4){
  b = x/y
  return(b)
}

my_fx2(4)

my_fx2(4, 5)
my_fx2(4,5)


# custom function with true false

my_fx3 = function(x = T){ 
  if (x == T){
    cat('x is true \n')
  }else{
    cat('x is not true \n')
  }
}

my_fx3(x = T)
my_fx3()
my_fx3(x = F)
my_fx3(x = 5)


# custom functions with vector operations
mi_km = function(mi){
  km = mi * 1.60934
  return(km)
}

mtcars %>%
  mutate(kmpg = mi_km(mpg))

## Function for density dependent growth
#Using a custom function we can create a function for density dependent population growth.\
#*Note that it is similar to above but instead of * $N_{t-1}$ *to calculate* $N_t$ *we are using* $N_t$ *to calculate* $N_{t+1}$
  
dlogisticD = function(K, rd, N_0, years) {
  logis = tibble(Nt = NA, time = 0:years)
  logis$Nt[logis$time == 0] = N_0
  
  for(i in 1:(length(logis$time)-1)){
    logis$Nt[i+1] = logis$Nt[i]  + rd*logis$Nt[i]*(1 - logis$Nt[i] / K)
  }
  return(logis)
}

dfx = dlogisticD(K = 900, rd = 1.2, N_0 = 10, years = 50) 

dfx

#We can compare the output of the function to the output of the for loop.
library(ggpubr)

a = ggplot(pop, aes(x = time, y = Nt)) +
  geom_line() +
  geom_point(color = "red", size = 2, shape = 1) +
  labs(x = 'Time', y = expression(italic(N[t])),
       title = 'For loop')+
  theme_bw()

b = ggplot(dfx, aes(x = time, y = Nt)) +
  geom_line() +
  geom_point(color = "blue", size = 2, shape = 1) +
  labs(x = 'Time', y = expression(italic(N[t])),
       title = 'Function')+
  theme_bw()


# plot combined
ggarrange(a,b,
          nrow = 1)
## Relation between growth rate and density
# Based on the above population curves, we can see that population growth ($\Delta N_t$) starts out small when both $t$ and $N_t$ are small, 
# accelerates as $N_t$ grows, and then over time, slows down and reaches an asymptote of $K$.
# 
# Using `mutate()` we can calculate the population growth increment ($\Delta N_t$) and the per capita growth ($\Delta N_t/N_t$). 
# The population growth is the $r_dN_t(1-N_t/K)$ of the equation.

pop.1 = pop %>%
  mutate(lambda = lead(Nt) / Nt, pop.growth = (rd*Nt)*(1 - Nt/K),
         per.capita = pop.growth / Nt)

pop.1

#Surplus/Production/Population growth
surplus = ggplot(pop.1, aes(Nt, pop.growth)) + 
  geom_line(color = "red") +
  geom_point(size = 2) +
  labs(x = expression(italic(N[t])),
       y =  expression('population growth ('~italic(Delta~N[t])~')'),
       title = 'Surplus production')+
  theme_bw()

#Per Capita Growth Increment or Density Dependent Effect
per.capita = ggplot(pop.1, aes(Nt, per.capita)) + 
  geom_line(color = "blue") +
  geom_point(size = 2) +
  labs(x = expression(italic(N[t])),
       y =  expression('per capita growth ('~italic(Delta~N[t]/N[t])~')'),
       title = 'Per Capita Growth')+
  theme_bw()

ggarrange(surplus, per.capita,
          nrow = 1)

## Effect of $N_0$
# Here we can see how the $N_0$ affects the output of the model.
# 
# We can do so using a nested `for` loop. 

# set initial parameters
years = 20
rd = 1
K = 900
N0s = floor(runif(15, 0, 1.5*K))

#For loop within a for loop example
for(j in 1:length(N0s)){
  # set up tibble to store data
  popJ = tibble(time = 0:years, Nt = NA, N0 = N0s[j])
  # set initial condition
  popJ$Nt[popJ$time == 0] = N0s[j]
  
  for(i in 2:length(popJ$time)){
    
    popJ$Nt[i] = popJ$Nt[i - 1]  + rd*popJ$Nt[i - 1]*(1 - popJ$Nt[i - 1] / K)
  }
  # combine 
  if(j == 1){
    popAll <- popJ 
  }else{
    popAll <- bind_rows(popAll, popJ)
  }
}
popAll

#This can also be done by using an `if` statement in the `for` loop.

# set up tibble to store data
pop3= tibble(time = rep(0:years, times = length(N0s)),
             Nt = NA, N0 = rep(N0s, each = years + 1))
# set N_0 
pop3$Nt[pop3$time == 0] = N0s

for(i in 1:length(pop3$time)){
  if(pop3$time[i] != 0){
    pop3$Nt[i] = pop3$Nt[i - 1]  + rd*pop3$Nt[i - 1]*(1 - pop3$Nt[i - 1] / K)}
}
pop3

#Plot results
ggplot(popAll, aes(time, Nt, color = as.factor(N0))) +
  geom_line(linewidth = 1)+
  labs(y = expression(italic(N[t])),
       x = 'Time',
       color = expression(italic(N[0])))+
  theme_bw()+
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 8))


  
## Effect of $r_d$
# Now let's see how changing $r_d$ changes the model output

# initial conditions
years = 50
rds = c(1.3, 1.6, 1.9, 2.2, 2.5, 2.8) 
K = 900
N_0 = 100

# set up tibble to store data
pop4= tibble(time = rep(0:years, times = length(rds)),
               Nt = NA, rd = rep(rds, each = years + 1))
# set N_0 
pop4$Nt[pop4$time == 0] = N_0

for(i in 1:length(pop4$time)){
  if(pop4$time[i] != 0){
  pop4$Nt[i] = pop4$Nt[i - 1]  + pop4$rd[i]*pop4$Nt[i - 1]*(1 - pop4$Nt[i - 1] / K)}
}
pop4

#Plot the results 
ggplot(pop4, aes(time, Nt, color = as.factor(rd))) +
  geom_line(size = 1)+
  labs(y = expression(italic(N[t])),
       x = 'Time',
       color = expression(italic(r[d])))+
  facet_wrap(~rd, nrow = 2)+
  theme_bw()+
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 8))

# Why Overshooting $K$?

#rd 1.6-1.9 = dampened oscillation 
#rd 2 - 2.5 = stable two-point cycles
#rd > 2.5 - 2.6 = stable four-point cycles or more
#rd > 2.6 = variable cycles around K through time = Chaos
#Chaos = no random, complex, and non repeating cycles sensitive to initial conditions

# ## Exercises
# 1) Using the sparrows dataset from the primer package (i.e., install and load "primer" package), plot the population size over time. Calculate the average $\lambda$ (hint: you will need to use the geometric mean) for the sparrow population.
# 
# 2) Project the population over the next 50 years for the sparrow population using the average $\lambda$ from exercise 1. For now use the mean sparrow count for $K$, and use the minimum sparrow count as $N_0$. Plot the results. 
# 
# 3) Did it reach $K$? If not, how many years does it take based on your $r_d$? Plot the results.
# 
# 4) Challenge: Simulate/Project the sparrow based on different $K$ = 25, 30, 40, 50. Plot the results. 
