# """ BSC 6926 B52 
#     Workshop 1: Introduction to R
#     authors: Santos and James
#     date: 8/28/23"""



## Getting to know the basics 

# R is a programming language that has become the standard in Ecology due to its flexibility and open source nature. R can be used from simple math to complex models and is very useful for generating figures. R, like all computer languages, uses a specific syntax to run commands that it is programmed to do. In other words, R will only do what it is commanded to do, and therefore, many common errors are due to errors in syntax (e.g. misspellings, missed commas, or unclosed brackets). 
# 
# This example gives a basic intro into R syntax that can be useful for ecological research. This script gives examples of how to:
#   
#   1.  Basic operations in R 
#   2.  Assigning objects
#   3.  Types of data structures in R
#   4.  Functions in R
#   5.  Using Packages in R
#     + How to install and load packages
#   6.  Working with `dataframes` and `tibbles`
#   7.  Indexing
#   8.  Exercises


## Basic operations in R

# R is useful for basic operations and follows math rules (i.e. PEMDAS). R will all code on a line unless there is a `#` to the left.

# addition 
1+1 

1+1 # + 2 (won't run anything to right of #)

# subtraction
5-2 

# multiplication
4*5

# division
33/5

# exponents can be done 2 ways
2^2
2**2

# follows PEMDAS
1+5*4
# different answer than above
(1+5)*4

# Note the `[1]` appears next to your result. R is just letting you know that this line begins with the first value in your result. Some commands return more than one value, and their results may fill up multiple lines. For example, the command 100:130 returns 31 values; it creates a sequence of integers from 100 to 130. Notice that new bracketed numbers appear at the start of the first and second lines of output. These numbers just mean that the second line begins with that value. You can mostly ignore the numbers that appear in brackets:
  

100:130

## Assigning objects
# When working in R it is useful to store data as an object. Assigning objects can be done in multiple ways, but the most common are `<-` and `=`. These objects are stored in the R environment and can be called. Objects can be assigned multiple times, but only the last assignment is what is stored. Also it is important to know that R is case sensative and capital and lower case numbers are different.

# assign an object
a = 4 
a

b <- 23

a+3 

b/2

a*b

c = 8
c = 14
c

d = 15 
D = 1 
d
D


## Types of data structures in R
# R has 6 basic data types. (In addition to the five listed below, there is also raw which will not be discussed in this workshop.)
# 
#   + integer
#   + numeric (real or decimal)
#   + character
#   + logical
#   + complex
# 
# integers are whole numbers
# 
# numeric are numbers with decimals. Integers and numeric are different because of how the underlying data is stored. Other programming languages can use something similar as decimal, float, or double data types, which all slightly differ in how data is stored but are numbers that include decimals.
# 
# characters are strings of letters and numbers (e.g. `"abc"` and `"b1x"`) and are designated in R by `" "`. When using characters, `" "` are required because in R letters without quotations are objects and `c = 'd'` is different than `c = d`
# 
# logical is `TRUE` or `FALSE`. One thing to note is that `T` is the same as `TRUE` and `F` is the same as `FALSE`. Because `T` and `F` are special in R they cannot be used to name objects (but `t` and `f` are ok because R is case sensative). This is true for other cases as well like `NA` and `NULL`.  
# 
# complex numbers have both real and imaginary parts (`1+4i`)
# 
# Elements of these data types may be combined to form data structures, such as atomic vectors. When we call a vector atomic, we mean that the vector only holds data of a single data type. A vector is the most common and basic data structure in R and is pretty much the workhorse of R. Technically, vectors can be one of two types:
#   + atomic vectors
#   + lists
# although the term “vector” most commonly refers to the atomic types not to lists. Lists differ because they can take on different data structures and can be more complex.
# 
# There are different ways to make vectors

# make a numeric vector
a = c(1.1,5,3,4)
a

# make a integer vector
b = 1:15
b

# make a character vector 
c = c('a', 'b', 'c')
c 


# Because characters can be both letters and numbers, numbers in a vector with letters are stored as a character. These cannot be used for math operations, but integers and numeric data types can be used for math. 
a = 4.4
a / 1 


b = 6L # L can be used to keep a numeric as an integer, R typically defaults to numeric
b*3

# character
c = '1'
c*4


#Another common way to store data is in a dataframe or tibble (special type of dataframe from the `tidyverse` package we will see below). This is a collection of atomic vectors with the same length. 

b = data.frame(c1 = c(1,2,3), c2 = c('a','b','c'))
b


## Functions in R
# R comes with functions that are used to do tasks. Functions take arguments to complete a task. Functions have the general format `function(argument1 = , argument2,...)` The types of data used and output of the function is specific to that function. Below are just a few useful examples. 


# summary statistics of sequence of numbers
a = c(1.1,5,3,4)
mean(a) #mean
median(a) #median
sd(a) #standard deviation
quantile(a, 0.5) # quantile at 0.5 (median)

# make a sequence of numbers
b = 1:15
b
c = seq(1,15,1) #more flexibility than :
c
seq(4,20,2)

# information about objects
d = c('a', 'b', 'c')
typeof(d) 
typeof(c)
length(d)

# dataframe/tibble specific functions
e = data.frame(c1 = c(1,2,3), c2 = c('a','b','c'))
names(e) # column names
nrow(e) # number of rows
length(e) # for dataframe number of columns
str(e)# structure of data

## Using Packages in R
#R comes with a lot of base functions that are available for use when you open R, but this does not contain all of the functions useful to your tasks in R. Since R is open source, many R users have created Packages that contain functions that can be downloaded. Which includes the very common `tidyverse`.

### How to install and load packages
# can be downloaded from CRAN or from Github. To download directly from Github other packages are needed. 

install.packages('tidyverse') #from cran


#Once downloaded, packages can be loaded into the R environment with `library()` function. Packages have to be loaded each R session. In addition functions can be called directly from a package with `::` in the format of `packageName::function()`. 


library(tidyverse)


## tidyverse
#tidyverse (https://www.tidyverse.org/) is a collection of packages that use similar syntax and are used for data science in R. Coding in tidyverse is typically easy to read and understand, and has useful functions that have been adopted into newer versions of base R (e.g. piping). Tibbles are the tidyverse version of a dataframe.

c = tibble(c1 = c(1,2,3), c2 = c('a','b','c'))
c

## Working with `dataframes` and `tibbles`
# Using either `dataframes` or `tibbles` will likely be the most common data structure for ecological data. Making these data structures is easy with the `data.frame()` or `tibble()` functions. Tibbles have more flexibility than dataframes and are part of the `tidyverse`. Dataframes are base R. When reading in tabular data, `read.csv()` will create a dataframe, while `read_csv()` will generate a tibble. `read_csv()` can be paired with `url()` to use data directly from the internet from sites like github. Note that if from github the raw file (click on raw tab when looking at github file) is needed for this to work. Similar to reading in data, `dataframes` and `tibbles` can be saved as .csv with `write.csv()` or `write_csv()`.

library(tidyverse)
# create a dataframe

df = data.frame(name = c('GOOG', 'AMC', 'GME'),
                Jan = c(1000, 2, 4),
                Feb = c(1010, 15, 30),
                March = c(1005, 25, 180))

df

# create a tibble
tib = tibble(name = c('GOOG', 'AMC', 'GME'),
             Jan = c(1000, 2, 4),
             Feb = c(1010, 15, 30),
             March = c(1005, 25, 180))

tib

#read in data file on computer
# change file path to path location on computer
read.csv('data/LDWFBayAnchovy2007.csv')

read_csv('data/LDWFBayAnchovy2007.csv')

# read in data file from github
# need to use raw file
read_csv(url('https://raw.githubusercontent.com/BSC6926-B52-Fall2023/workshopScripts/main/data/LDWFBayAnchovy2007.csv'))

# save dataframe or tibble as new csv
write.csv(df, 'data/df.csv')

write_csv(df, 'data/df.csv')

##  Indexing
#Once data is stored in an object, being able to retrieve those values is useful. Referred to as indexing, the syntax is specific to how the data is stored. With indexing specific values within your object can be modified. 

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

# dataframe and tibbles
mtcars
# first column
mtcars[1]
# first row
mtcars[1,]
# 2nd row of first column
mtcars[2,1]
# can call specific columns (called as a vector)
mtcars$mpg
mtcars$cyl
#same for tibble
d = mtcars %>% as_tibble
d[1]
d$mpg
d$cyl
# specific row in specific column
mtcars$cyl[1]
d$cyl[1]



# ## Exercises 
# ### Complete following exercises and turn in r script on canvas
# 1.    Make two vectors, object `a` containing the values 2, 3, 4, and 5 and object `b`containing the values 50, 100, 38, and 42.
# 
# 2.    Multiply object `a` by 3 and assign it to a new object, divide object `b` by 5 and assign it to a new object, then add the new two objects together. 
# 
# 3.    Create a new `data.frame`/`tibble` with the four objects created above
# 
# 4.    Save the `data.frame`/`tibble` created in exercise 3 as a .csv
# 
# 5.    Load in files a.csv and b.csv (found on [github](https://github.com/BSC6926-B52-Fall2023/workshopScripts/tree/main/data) and canvas) and assign each as an object.