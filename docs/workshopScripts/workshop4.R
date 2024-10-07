# title: "Workshop 4: Density independent models"
# author: "BSC 6926 B53"
# date: "10/7/2024"

## Discrete Population models

# Discrete models assume that there are distinct generations. This is very common for populations that live in seasonal habitats - their reproduction is timed to the season, and they breed altogether in one bout. These populations experience geometric growth. Here the population at a given time point ($N_t$) is defined by the equation
# 
# $$ N_t = N_0\lambda^t$$ where $N_0$ is the starting population size, $\lambda$ is the finite growth rate of the population, and $t$ is the time step.

## Continuous population growth

# Not all populations have discrete reproduction, and therefore are continuously increasing. These populations experience exponential growth, and the population size at a given time ($N_t$) can be found with the equation
# 
# $$ N_t = N_0e^{rt} $$ where $N_0$ is the population size at time = 0, $r$ is the instantaneous per capita growth rate, and $t$ is time.

## Properties of density independent growth

# Comparing the methods for geometric (discrete) and exponential (continuous) growth models we find that $$ \lambda = e^r$$ or $$ ln\lambda = r $$ these relationships can be used to distinguish between $\lambda$ and $r$.
# 
# Based on the properties there are a few general rules\
# 
# -   no change in population: $r = 0$; $\lambda = 1$\
# -   population increase: $r > 0$; $\lambda > 1$\
# -   population decline: $r < 0$; $\lambda < 1$\

## `for` loops

# Because the equations for density independent growth are based on the population size the time step before. `for` loops are very useful when using these models in R.
# 
# For loops repeat a process for a certain number of iterations. These can be useful iterate over a dataset or when using information in a time series. The `for` loop works over the number sequence indicated and does the code within the loop (i.e. the body of the loop; inside of `{}`) for each number in the sequence. The iteration is typically indicated with `i`, but is just an object that is replaced at the beginning of each loop and can be anything.
# 
# Here is a simple example where we print the iterator `i` for each number in the sequence.

for(i in 1:10){ #Sequence
  print(i)      #body
}

# The iterator can be assigned to any object, and the sequence can be any vector of numbers.

for(turtle in 5:10){
  print(turtle)
}


# A common use of the iterator is to use it to index values to combine with other coding to repeatedly do a task.

for(flower in 1:nrow(iris)){
  cat('The species for this iteration is ',     #Adding text per row based on name species
      as.character(iris$Species[flower]), '\n') #note of importance of "\n
}



# The iterator can be combined with math (e.g. subtraction or addition) to call mulitple values in a specific order. This example uses the the *i*th term and the *i-1*th terms to complete a task.\
# **Note** This will be very useful in population models that depend upon previous time steps.

b = 1:10
for (i in 2:10){    #Call a section of vector to start sequence
  z = b[i] - b[i-1]
  
  cat('z =', z, 'b[i] =', b[i], 'b[i-1] =', b[i-1], '\n')
}


## Simulate a population

# Now we can use `for` loops to simulate a simple population.
# 
# We first create indicate our starting population size ($N_0$) and create a dataframe with all of our time steps and a column for the population size at each time step ($N_t$).


library(tidyverse)
N_0 = 10 
years = 10
pop = tibble(time = 0:years, Nt = NA) #Output vector size 10
pop$Nt[pop$time == 0] = N_0
pop


# We can use a `for` loop to go through each time point, randomly generate a number of individuals born from a Poisson distribution and add it to the populations size from the time before.\
# \*Note this simulation will change with each run because growth rate is randomly generated each time step.

for (t in 1:years){ #sequence 
  growth = rpois(1, lambda = 3) #extracting random value from Poisson Dist
  pop$Nt[pop$time == t] = growth + pop$Nt[pop$time == (t-1)]
}


ggplot(pop, aes(time, Nt))+
  geom_point(size = 2)+
  geom_line(linewidth = 1)+
  labs(x = 'Time', y = expression(italic(N[t])))+
  scale_x_continuous(breaks = seq(0,10,1))+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


## Average growth rate

# In real populations $N_t/N_{t-1}$ are not constant. We can calculate the yearly $\lambda$ also referred to as the annual growth rate ($R$) which is $$R = N_{t+1}/N_t$$ This can be calculated within a `for` loop


# create column to store data
pop$R = NA

pop

for (i in 1:length(pop$time)){
  pop$R[i] = pop$Nt[i+1]/pop$Nt[i]
}

pop


# What $R$ is representing if $Nt$ is population size at Time $t$?\
# 
# $\lambda > 1$ for all years so the population is always growing\
# 
# This may be the case depending on the random draws from Poisson Distribution, which may include lower sequential values from starting point\

ggplot(pop, aes(time, R))+
  geom_point(size = 2)+
  geom_hline(yintercept = 1, color = 'red')+
  labs(x = 'Time', y = expression(italic(R)))+
  scale_x_continuous(breaks = seq(0,10,1))+
  theme_classic()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


# This can also be done in `dpylr`\
# **Note the correction. Need to use the `lead()` function to** $N_{t+1}$**. The `lag()` function would be to represent** $N_{t-1}$**.**
  
pop = pop  |>  
  mutate(R2 = lead(Nt) / Nt)

pop


## Random sampling

### Random numbers

# In the above example we generated random growth from a Poisson distribution, but R can handle many other types.
# Since they are randomly generated, these numbers will be different each time. You can also use the `set.seed()` to repeat the answer.
rnorm(5, mean = 0, sd = 1)

runif(5, min = 0, max = 5)

rpois(10, lambda = 10)
rpois(10, lambda = 1:10)

rbinom(5, size = 2, prob = 0.5)
rbinom(5, size = 2, prob = 0.2)

# get same answer
rnorm(5, mean = 0, sd = 1)

set.seed(14)
rnorm(7, mean = 0, sd = 1)

set.seed(16)
rnorm(5, mean = 0, sd = 1)


# Packages like [truncnorm](https://www.rdocumentation.org/packages/truncnorm/versions/1.0-8/topics/truncnorm) can be useful for specialized cases of random number generation. In this case `truncnorm` generates numbers from a random normal distribution with within a minimum and maximum value. Note that there currently is not a


library(truncnorm)
# truncnorm to truncate normal distribution
rnorm(20, mean = 1, sd = 2)
truncnorm::rtruncnorm(20, mean = 1, sd = 2, a=-Inf, b=Inf)
truncnorm::rtruncnorm(20, mean = 1, sd = 2, a=0, b=2)


### Random selection

# Another type of random sampling is drawing from a sample from a vector and this can be done with `sample()` in base R with replacement or without replacement and based on certain probabilities.

# base R 
b = 1:10
sample(b, size =2, replace = F)

c = 1:5 
sample(c, size = 6, replace = T)

# from list
sample(c('good', 'bad'), size = 8, replace = T)

# change probability
sample(c('good', 'bad'), size = 8, replace = T, prob = c(0.2, 0.8))

# sample matrices from list 
m1 = matrix(c(6,5,4,3,2,1), ncol = 2)
m2 = matrix(c(1,2,3,4,5,6), ncol = 2)
m3 = matrix(c(1,2,3,4,5,6), ncol = 2, byrow = T)

m = list(m1, m2, m3)

sample(m, size = 4, replace = T)

sample(m, size = 4, replace = T, prob = c(0.8, 0.1, 0.1))

# `dplyr` has a useful function `sample_n()` that is specialized to work with dataframes and tibbles


#library(tidyverse)
df = tibble(cond = c('good', 'bad', 'ok'), prob = c(0.5, 0.3, 0.2))

df

dplyr::sample_n(df, size = 2, replace = F)

dplyr::sample_n(df, size = 3, replace = T)

dplyr::sample_n(df, size = 10, replace = T, weight = prob)


### Density independent continuous population models

# $r$ is the instantaneous per capita growth rate. It's value determines how population size will change over time.

# What are we doing here?

r = c(-0.03, -0.02, 0, 0.02, 0.03)

N_0 = 10
years= 100 #hundred time steps
pop = tibble(time = rep(0:years, times = length(r)),
              Nt = NA, growth = rep(r, each = years+1))
pop

for(i in seq_along(pop$time)){
  pop$Nt[i] = N_0  * exp(pop$growth[i] * pop$time[i])
}
pop


# Also can be done in `dplyr`

#Using dplyr
pop = pop  |>  
  mutate(Nt2 = N_0 * exp(growth*time), 
         ln.Nt = log(Nt2))
pop


# What ln.Nt is representing?
# 
# Let's see with ggplot


###Let's see it using ggplot
library(ggpubr)

a = ggplot(pop, aes(x = time, y = Nt, color = as.factor(growth))) +
  geom_line(linewidth = 1) +
  labs(x = 'Time', y = expression(italic(N[t])),
       color = expression(italic(r)),
       title = 'Exponential')+
  theme_bw()

b = ggplot(pop, aes(x = time, y = ln.Nt, color = as.factor(growth))) +
  geom_line(linewidth = 1) +
  labs(x = 'Time', y = expression('ln('~italic(N[t])~')'),
       color = expression(italic(r)),
       title = 'Semilogarithmic')+
  theme_bw()

# plot combined
ggarrange(a,b,
          nrow = 1,
          common.legend = T)


# Slope of line in b is $r$ (intrisic rate of increase)

## Exercises

# 1.  Download primer and open primer package. Upload the sparrows dataset from package and plot counts as function of years.
# 
# 2.  Calculate the annual rate of increase ($\lambda$) for each time step and plot $\lambda$ as function of years as a scatterplot\
# *Hint: Look at the `for` loop/dplyr examples above*
#   
# 3.  *Challenge*: Simulate a population with varying $\lambda$ at each time step.
