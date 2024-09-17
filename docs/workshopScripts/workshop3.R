# title: "Workshop 3: Introduction to R"
# author: "BSC 6926 B52"
# date: "9/12/2023"

# ## Conditional statements, for loops, vector operations, quarto/rmardown 
# This workshop continues basics of R working more with conditional statements, for loops, vector operations, and quarto/rmarkdown:
#   
# 1.  Conditional statements
# + base R
# + `dpylr`
# 2.  For loops
# 3.  Vector operations
# + dates with `lubridate`
# + `purr`
# 4.  Quarto/rmarkdown
# 5.  Exercises 
# 


library(tidyverse)

# ## Conditional statements
# In programing there are times that if something is true then you want an operation to occur, but not when a condition is not true. 
# ### Base R
# These can be done with `if` and `if else` statements in base R. These are written if a condition is true then the operation is done. They can be built upon with `else if` if the first condition is false to do test a second condition. If you want it to be If true and if false do something else then `if` and `else` structure can be used. 

b = 5 

if (b == 5){
  cat('b = 5 \n') # \n is carriage return to end line when printing
}

if (TRUE){
  c = 6
}else(
  c = 10
)
c 

if (F){
  c = 6
}else(
  c = 10
)
c 

if (b == 10){
  cat('b = 10 \n')
}else if (b == 5){
  cat('it worked \n')
}else{
  cat('nothing \n')
}


# ### `dplyr` functions
# `dplyr` has two functions that are very useful for conditional statements. Because they are a function they can be vectorized which will be useful as you see below. `if_else()` is a function that based on if the input is `TRUE` or `FALSE` produces a different answer. `case_when()` is more flexible and allows for multple outputs based on conditions being `TRUE`

x = 1:20

if_else(x > 10,
        'x > 10',
        'x < 10')


case_when(x <  6 ~ 'x < 6',
          between(x, 6, 15) ~ '6 <= x <= 15',
          x > 15 ~ 'x > 15')

# ##  For loops
# Another useful tool in programming is `for` loops. For loops repeat a process for a certain number of iterations. These can be useful iterate over a dataset or when using information in a time series. The `for` loop works over the number sequence indicated and does the code within the loop (inside of `{}`) for each number in the sequence. The iteration is typically indicated with `i`, but is just an object that is replaced at the begining of each loop and can be anything.

for(i in 1:10){ #Sequence - i object within the sequence from 1 to 10
  print(i)      #body - the operation to do something (function, operation, etc)
}

#The iterator could be assigned to any letter or word assigment
for(turtle in 5:10){
  print(turtle)
}

for(flower in 1:nrow(iris)){
  cat('The species for this iteration is ',     #Adding text per row based on name species
      as.character(iris$Species[flower]), '\n') #note of importance of "\n
}

d = seq(1,15, 2) #8 elements
d
for(i in 1:length(d)){
  b = d[i] + 1                            #for each i add 1
  cat('d =',d[i], 'b = d + 1 =', b, '\n' )#then add string
}

#Using seq_along
d = seq(1,15, 2) #8 elements
d
for(i in seq_along(d)){
  b = d[i] + 1                            #for each i add 1
  cat('d =',d[i], 'b = d + 1 =', b, '\n' )#then add string
}

b = 1:10
for (i in 2:10){    #Call a section of vector to start sequence
  z = b[i] - b[i-1]
  
  cat('z =', z, 'b[i] =', b[i], 'b[i-1] =', b[i-1], '\n')
}


start = 10 
pop = tibble(time = 0:10, n = NA) #Output vector size 10
pop$n[pop$time == 0] = start
pop
for (t in 1:10){ #sequence 
  growth = rnorm(n =1, mean = 3, sd = 1) #extracting random value normal dist
  pop$n[pop$time == t] = growth + pop$n[pop$time == (t-1)]
}
pop

##  Vector operations 
# As we have seen above, we can do operations over vectors. We sometimes want to do this to vectors stored in dataframes/tibbles, and the `mutate()` function makes this easy. This can be applied to multiple columns at once with `across()`

iris |> 
  mutate(petalArea = Petal.Length*Petal.Width)

iris |>
  mutate(petalArea = Petal.Length*Petal.Width,
         PetalSize = if_else(condition = petalArea > 0.2, true ='big',
                             false = 'small'))

iris |>
  mutate(petalArea = Petal.Length*Petal.Width,
         PetalSize = if_else(condition = petalArea > 0.2, true ='big',
                             false = 'small'))|>
  group_by(PetalSize)|>
  summarize(mean = mean(Petal.Width),
            n = n())

## across can do multiple columns at once
iris |> 
  mutate(across(where(is.numeric), ~.x +1)) # add 1 to all numeric columns


iris |> 
  group_by(Species) |> 
  summarize(across(Sepal.Length:Petal.Length, mean)) # mean of specific columns

# ### dates with `lubridate`
# `lubridate` is a package that is apart of the `tidyverse` that is very useful for dates. Check out the [R for data science](https://r4ds.hadley.nz/datetimes) chapter and for the [R cheat sheet](https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_lubridate.pdf) for working with dates.

## lubridate and dates
df = tibble(ymd = c('1999-01-04', '2005-04-11', '2015-10-01'),
            mdy = c('1/4/99', '4/11/05', '10/1/15'))

# turn into dates
df = df |> 
  mutate(d1 = ymd(ymd),
         d2 = mdy(mdy),
         d3 = dmy(mdy)) #functions to turn into dates order matters

df

# extract information from dates
df = df |> 
  mutate(year = year(d1),
         month = month(d1))

# ### `purr`
# The newest and new standard package with `tidyverse` is `purr` with its set of `map()` functions. Some similarity to `plyr` (and base) and `dplyr` functions but with more consistent names and arguments. Notice that map function can have some specification for the type of output.
# + `map()` makes a list.
# + `map_lgl()` makes a logical vector.
# + `map_int()` makes an integer vector.
# + `map_dbl()` makes a double vector.
# + `map_chr()` makes a character vector.

df = iris  |> 
  select(-Species)
#summary statistics
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

#You can also use map in a tibble to apply to a nested column with `mutate()`. Nested columns are one of the differences between `data.frame()` and `tibble()`.

#Creating models by group
df_cars = mtcars |> 
      as_tibble() |> 
      group_by(cyl) |> 
      nest()

df_cars
df_cars$data[1]

df_cars = df_cars |> 
      mutate(models = map(data, \(x) lm(mpg ~ wt, data = x)),
             summary = map(models, summary),
             r_sq = map_dbl(summary, \(x) x$r.squared))

df_cars

###
#Mapping over multiple arguments
###

mu = list(5, 10, -3)
mu |> 
      map(rnorm, n = 5) |> #rnorm - function to extract values from a normal continuous distribution based on some parameters
      
      str()
#> List of 3

###
#map2 or pmap allows you to iterate over two or more vectors in parallel
###
sigma = list(1, 5, 10)
map2(mu, sigma, rnorm, n = 5) |> str()

#or with pmap
n = list(1, 3, 5)
args2 = list(mean = mu, sd = sigma, n = n)
args2 |> 
      pmap(rnorm) |> 
      str()

# ## Quarto/rmardown
# There are times when we want to inbed R into a document to make it easier to read. This can be done with quarto (`.qmd`) or rmarkdown (`.rmd`) files. These file types are more similar to word documents, have their own syntax, and can embed programming languages (not just R) into the document. These files can be output as html, pdf, or docx files. These are very flexible and can be used to make single documents, books, websites, and presentations. Resources about [quarto](https://quarto.org/docs/reference/) and [rmarkdown](https://rmarkdown.rstudio.com/lesson-1.html)
# 
# For PDFs in [Quarto](https://quarto.org/docs/output-formats/pdf-basics.html) and [Rmarkdown](https://bookdown.org/yihui/rmarkdown/pdf-document.html). You will need to download some form of LaTex to write the PDFs. R recommends the following code

# 
# install.packages('tinytex')
# 
# tinytex::install_tinytex()

# ## Exercises
# 1. Using the iris data create a new tibble or dataframe that calculates the median value of all petal and sepal measurements for each species. 
# 
# 2. Simulate population growth over 25 years of a population that has random growth from a mean of 10 individuals and a sd of 5 individuals that starts at a population size of 100. 
# 
# 3. Plot population size over time
# 
# 4. Create a quarto or rmarkdown document of the exercises.