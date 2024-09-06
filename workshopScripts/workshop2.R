# title: "Workshop 2: Introduction to R"
# author: "BSC 6926 B52"
# date: "9/10/24

## Tidyverse, data wrangling, and `ggplot` 
# This workshop continues basics of R working more with `tidyverse`, data wrangling, and ggplot:
#   
#   1.  Indexing 
# 2.  `tidyverse`
# + tidy data
# + piping Working with dataframes 
# 3.  Combining dataframes and tibbles
# 4.  Figures with `ggplot2` 
# + Combining plots
# 5. Practice Exercises
# 
# [R script of workshop 2](workshopScripts/workshop2.R)
# 
# Resources to create [R project](https://bookdown.org/daniel_dauber_io/r4np_book/starting-your-r-projects.html)

##  Indexing
# Once data is stored in an object, being able to retrieve those values is useful. Referred to as indexing, the syntax is specific to how the data is stored. With indexing specific values within your object can be modified. 

library(tidyverse)
#First lets bring a dataset from R and assign it to an object
mtcars_df = mtcars 

#Convert to data.frame to tibble
mtcars_df = as_tibble(mtcars_df)

#Use "$" to call or index a specific column
mpg = mtcars_df$mpg
gear = mtcars_df$gear

#Use brackets [] to call a specific value position within a vector or 
# or column within a dataframe

#fifth object 
mpg[5]
mpg #Let's check

# vector 
b = 1:15
# 3rd object 
b[3]

# make a character vector 
c = c('a', 'b', 'c')
c

# 2nd object
c[2]

# change 
c[2] = 'new'
c

#Indexing within a table: [rows, columns]

# first row
mtcars[1,]

# Second row and first column
mtcars[2,1]
## [1] 21
# can call specific columns (called as a vector)
mtcars$mpg

# specific row (value) in specific column
mtcars$cyl[1]

### Creating and indexing lists

# List = a collection of objects (e.g., vectors, data.frames, matrices, lists) assigned to a object or list

list_example = list(a = seq(20,30, by = 5), b = c('a', 'b', 'c'), c = mtcars_df)

#Calling a object within the list
list_example$a #Element name a
list_example[1] #Preserving - new list with only with first element
list_example[[1]] #Simplifying - first element of list

list_example$c
list_example[3]

#Get the first value within the second object of the list
list_example[[2]][1]

#First value of first column on the tibble assigned as the third object in the list
list_example[[3]][[1]][1]

colnames(list_example[[3]][1])

## tidyverse
# [tidyverse](https://www.tidyverse.org/) is a collection of packages that use similar syntax and are used for data science in R. Coding in tidyverse is typically easy to read and understand, and has useful functions that have been adopted into newer versions of base R (e.g. piping). Tibbles are the tidyverse version of a dataframe.

library(tidyverse)

c = tibble(c1 = c(1,2,3), c2 = c('a','b','c'))
c


### tidy data
# Data is collected and stored in many different ways, which can make it difficult to analyze. One of the goals of tidyverse is to easily turn messy data into tidy data which can easily be analyzed. In tidy data:
#   
# 1. Every column is a variable.
# 2. Every row is an observation.
# 3. Every cell is a single value.
# 
# Two functions `pivot_longer()` and `pivot_wider()` are useful in manipulating data stored in rows and columns. ***Note that `pivot_longer()` and `pivot_wider()` have replaced `gather()` and `spread()` in newer versions of `tidyverse`

#tidying data 
stock = tibble(name = c('GOOG', 'AMC', 'GME'),
               Jan = c(1000, 2, 4),
               Feb = c(1010, 15, 30),
               March = c(1005, 25, 180))

df = pivot_longer(stock,
                  cols = Jan:March, 
                  names_to = 'Month',
                  values_to = 'Price')

df

# wide format
fish = tibble(species = rep(c('Salmon', 'Cod'),times = 3),
              year = rep(c(1999,2005,2020), each = 2),
              catch = c(50, 60, 40, 50, 60, 100))
fish 


pivot_wider(fish,
            id_cols = species,
            names_from = year,
            values_from = catch)


# ### piping  
# Tidyverse has an operator `%>%` known as a pipe that is useful for when you want to do multiple actions to the same data. It takes the output of the left of the `%>%` and makes it the first argument of what is on the right. Allowing to reduce code and make things tidier. In newer versions of R, there is a base pipe `|>` that can be used as well. To use `|>`you will need to turn it on in global options. 
# *Note you can use `ctrl` + `shift` + `m` as a shortcut for `|>`*
#   

# this code
df = as_tibble(mtcars)
df = filter(df, mpg > 20)
df = mutate(df, color = 'red')
df = select(df, mpg, cyl, color)

head(df)

# can become

df = mtcars %>%
  as_tibble()%>%
  filter(mpg > 20)%>%
  mutate(color = 'red')%>%
  select(mpg, cyl, color)

head(df)

# or with base r
df = mtcars |>
  as_tibble()|>
  filter(mpg > 20)|>
  mutate(color = 'red')|>
  select(mpg, cyl, color)

head(df)


## Renaming and making columns
# There are a few different ways to create a new column. The base R way is to use `$` with the object name of the dataframe on the left and the new column name on the right. This can be used to do vector operations as well. The other way is to the `mutate()` function which is part of the `dplyr` package in tidyverse. This function alows for more flexibility and can be very useful. The easiest way to rename columns is with `dplyr` functions like `rename()` or within function like `select()`.


df = tibble(name = c('GOOG', 'AMC', 'GME'),
            Jan = c(1000, 2, 4),
            Feb = c(1010, 15, 30),
            March = c(1005, 25, 180))

df$new = 'new column'

df$tot = df$Jan + df$Feb + df$March

df 

# using mutate
df = df |> 
  mutate(newCol = 'blue')

# multiple columns at a time
df = df |>
  mutate(sum = Jan + Feb + March, 
         big = sum > 500)
df

# rename columns
df |>
  rename(Name = name, January = Jan, February = Feb)

# rename, reorder, only include certain columns 
df |>
  select(Name = name, January = Jan, sum, everything())

# order data frame
df |> 
  arrange(sum)

df |> 
  arrange(desc(sum))


## Summarizing data
# There are a few different useful ways to summarize the data in a dataframe or tibble. If you want to know everything about the dataframe, then the base function `summary()` is useful. If you would like to have more control to create summary tables, then `dplyr::summarize()` or `dplyr::summarise()` are great. This can be paired with `group_by()` to summarize over specific groups of data.

summary(iris)

iris |> 
  summarize(mean(Petal.Width),
            sd(Petal.Width))

iris |> 
  group_by(Species)|>
  summarize(mean(Petal.Width),
            sd(Petal.Width))


## Combining mulitple dataframes
# Combining data together is very common, and depending on the type of combination needed. 
# 
# ### Binding
# If data has the same column names and needs to paste together, then `rbind()` and `dplyr::bind_rows()` are the tools need. For `rbind()`, the column names need to have the same name. `bind_rows()` does not have this problem.

# bind data together 
sal = tibble(species = rep(c('Salmon'),times = 3),
             year = c(1999,2005,2020),
             catch = c(50, 60, 40))

cod = tibble(species = rep('Cod', times = 3),
             year = c(1999,2005,2020),
             catch = c(50, 60, 100))

crab = tibble(species = rep('Crab', times = 3),
              catch = c(50, 60, 100),
              effort = c(20, 30, 50))

rbind(sal,cod)

#Why error?
rbind(sal, crab)

#Flexibility on the order

bind_rows(sal, cod)
#vs
bind_rows(sal, crab)


### Merge/Join
# If two data frames contain different columns of data, then they can be merged together with the family of join functions.
# 
# +`left_join()` = uses left df as template and joins all matching columns from right df 
# +`right_join()` = uses right df as template and joins all matching columns from left df
# +`inner_join()` = only matches columns contained in both dfs
# +`full_join()` = combines all rows in both dfs

left = tibble(name = c('a', 'b', 'c'),
              n = c(1, 6, 7), 
              bio = c(100, 43, 57))

right = tibble(name = c('a', 'b', 'd', 'e'),
               cals = c(500, 450, 570, 600))

left_join(left, right, by = 'name')

right_join(left, right, by = 'name')

inner_join(left, right, by = 'name')

full_join(left, right, by = 'name')

# multiple matches
fish = tibble(species = rep(c('Salmon', 'Cod'),times = 3),
              year = rep(c(1999,2005,2020), each = 2),
              catch = c(50, 60, 40, 50, 60, 100))

col = tibble(species = c('Salmon', 'Cod'),
             coast = c('West', 'East'))

left_join(fish, col, by = 'species')



# ## Figures with `ggplot2` 
# The `ggplot2` package is part of the packages that load with `tidyverse` and has become the standard in ecology. The syntax builds upon on a base function and is very customizable [see cheat sheet](https://www.rstudio.com/resources/cheatsheets/). 
# [R for data science](https://r4ds.had.co.nz/data-visualisation.html)
# The base of all `ggplot2` begins with `ggplot()` and `geom_...()` are built upon them 
# [R for data science](https://r4ds.had.co.nz/data-visualisation.html) 

# read in data
df = read_csv(url('https://raw.githubusercontent.com/SeascapeEcologyLab-workshops/BSC6926-B52_Fall2024/main/data/LDWFBayAnchovy2007.csv'))

# plot number of Bay anchovy caught per month
ggplot(df, aes(x = date, y = num))+
  geom_point()


# Show color based on basin number and add line connecting dots


ggplot(df, aes(x = date, y = num, color = basin))+
  geom_point()+
  geom_line()


# Change labels and style of plot



ggplot(df, aes(x = date, y = num, color = basin))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  theme_classic()


# Modify the size of axis label text and legend position  

ggplot(df, aes(x = date, y = num, color = basin))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance', color = 'Basin')+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom')


# Only plot specific range of the dates on x axis 

ggplot(df, aes(x = date, y = num, color = basin))+
  geom_point()+
  geom_line()+
  scale_x_date(limits = c(lubridate::ymd('2007-04-01'), lubridate::ymd('2007-10-01')))+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank())


# Split each trial into own grid

ggplot(df, aes(x = date, y = num))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  facet_wrap(~basin)+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank())

# Modify the date labels on x axis ([list of date abbreviations](https://rdrr.io/r/base/strptime.html)) and make 1 column of plots

ggplot(df, aes(x = date, y = num))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  scale_x_date(date_breaks = '2 months', date_labels = '%m/%y')+
  facet_wrap(~basin, ncol = 1)+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank())


# Modify the label and size of strip text


# doesn't change the order
labels = c('Calcasieu' = 'CAL',
           'Vermilion-Teche' = 'VER',
           'Terrebonne' = 'TER',
           'Barataria' = 'BAR',
           'Pontchartrain' = 'PON')

ggplot(df, aes(x = date, y = num))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  scale_x_date(date_breaks = '2 months', date_labels = '%m/%y')+
  facet_wrap(~basin, ncol = 1, labeller = as_labeller(labels))+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text = element_text(size = 12))

# Remake figure with the mean Abundance and min and max values from each basin and the summarized line through the points

ggplot(df, aes(x = date, y = num))+
  geom_pointrange(stat = "summary",
                  fun.min = 'min',
                  fun.max = 'max',
                  fun = 'mean')+
  stat_summary(aes(y = num), fun = mean, geom = 'line')+
  labs(x = 'Date', y = 'Bay anchovy abundance')+
  scale_x_date(date_breaks = '2 months', date_labels = '%m/%y')+
  theme_classic()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Make box plot of number of seines per month within each basin 

ggplot(df, aes(x = basin, y = seines))+
  geom_boxplot()+
  labs(x = NULL, y = '# of seines')+
  theme_bw()

# Change order of x axis (make basin order from west to east) and color of plot. Colors can be both hex code or from names that R has. A help website for picking colors is [here](https://rstudio-pubs-static.s3.amazonaws.com/3486_79191ad32cf74955b4502b8530aad627.html).

df = df %>% 
  mutate(basin = factor(basin, levels = c('Calcasieu',
                                          'Vermilion-Teche',
                                          'Terrebonne',
                                          'Barataria',
                                          'Pontchartrain' )))

colors = c('Calcasieu' = 'darkred',
           'Vermilion-Teche' = 'cadetblue4',
           'Terrebonne' = '#FFC125',
           'Barataria' = '#5d478b',
           'Pontchartrain' = 'grey55')

ggplot(df, aes(x = basin, y = seines, fill = basin))+
  geom_boxplot()+
  labs(x = NULL, y = '# of seines')+
  scale_fill_manual(values = colors)+
  theme_bw()

# Modify the labels and remove the legend

ggplot(df, aes(x = basin, y = seines, fill = basin))+
  geom_boxplot()+
  labs(x = NULL, y = '# of seines')+
  scale_fill_manual(values = colors)+
  theme_bw()+
  theme(axis.title = element_text(size = 18), 
        axis.text.y = element_text(size = 18, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        legend.position = 'none',
        legend.title = element_blank())


### Combining plots 
# Sometimes we would like to combine different sub figures together to make a single figure. There are a few packages that can do this with `ggpubr` and `patchwork` some of the most common. I like `ggpubr` and use this one, but people seem to like `patchwork`. 

library(ggpubr)
library(wesanderson)

a = ggplot(df, aes(x = basin, y = seines, fill = basin))+
  geom_boxplot()+
  labs(x = NULL, y = '# of seines')+
  scale_fill_manual(values = colors)+
  theme_bw()+
  theme(axis.title = element_text(size = 14), 
        axis.text.y = element_text(size = 14, colour = "black"), 
        axis.text.x = element_text(size = 10, colour = "black"), 
        legend.position = 'none',
        legend.title = element_blank())

b = ggplot(df, aes(x = date, y = num, color = basin))+
  geom_point()+
  geom_line()+
  labs(x = 'Date', y = 'Bay anchovy abundance', color = 'Basin')+
  theme_bw()+
  scale_color_manual(values = colors)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = 'bottom')

# plot combined
ggarrange(a,b,
          labels = c('a)','b)'),
          ncol = 1)

# arrange vertically and move position of labels
ggarrange(a,b,
          labels = c('a)','b)'),
          ncol = 1,
          align = 'v',
          hjust=-1.5)

# common legend
a = ggplot(mtcars, aes(wt, fill = as.character(cyl), 
                       color = as.character(cyl)))+
  geom_density(alpha = 0.4)+
  labs(x = 'Weight of car (tonnes)', 
       fill = '# of engine cylinders')+
  scale_color_manual(values = wes_palette('GrandBudapest1'),
                     guide = "none")+
  scale_fill_manual(values = wes_palette('GrandBudapest1'))+
  theme_bw()+
  theme(axis.title = element_text(size = 10), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 10),
        legend.text = element_text(size = 7))

b = ggplot(mtcars, aes(mpg, color = as.character(cyl),
                       fill = as.character(cyl)))+
  geom_density(alpha = 0.4)+
  labs(x = 'Miles/gallon',
       fill = '# of engine cylinders')+
  scale_color_manual(values = wes_palette('GrandBudapest1'),
                     guide = "none")+
  scale_fill_manual(values = wes_palette('GrandBudapest1'))+
  theme_bw()+
  theme(axis.title = element_text(size = 10), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 10),
        legend.text = element_text(size = 7))


c = ggplot(mtcars, aes(wt, mpg, group = cyl, color = as.character(cyl)))+
  geom_point(size = 2)+
  geom_smooth(method = 'lm',size = 1)+
  labs(x = 'Weight of car (tonnes)', 
       y = 'Miles/gallon',
       color = '# of engine cylinders')+
  scale_color_manual(values = wes_palette('GrandBudapest1'))+
  theme_bw()+
  theme(axis.title = element_text(size = 10), 
        axis.text.y = element_text(size = 10, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"), 
        legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 10),
        legend.text = element_text(size = 7))


ggarrange(a,b,c, 
          labels = c('A','B','C'),
          nrow = 2,ncol = 2,
          common.legend = F)

ggarrange(a,b,c, 
          labels = c('A','B','C'),
          nrow = 2, ncol = 2,
          common.legend = T,
          legend = 'top')

ggarrange(ggarrange(a,b, labels = c('A','B'), common.legend = T),c,
          labels = c('','C'),
          nrow = 2,
          legend = 'none')


# 
# ## Exercises 
# 1.    Read in the LDWFBayAnchovy2007.csv and create a column that calculates the catch per unit effort (CPUE) for Bay anchovy within the dataframe.
# 
# 2.    Create a dataframe or tibble that contains the basin names for the LDWFBayAnchovy2007.csv dataset (Barataria, Terrebonne, Ponchartrain, Vermilion-Teche, and Calcasieu) and the and abbreviation for each basin as a new column. 
# 
# 3.    Merge the dataframe/tibbles from exercises 1 and 2. 
# 
# 4.    Plot the CPUE for each basin both over time and as a summary of the entire year using a different color for each basin. 