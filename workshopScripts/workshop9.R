#' """ Workshop 9: Community data: trajectories
#'     @author: BSC 6926 B52
#'     date: 11/19/2024"""

# ## Community Trajectory Analysis
# 
# Community Trajectory Analysis (CTA) is a framework to analyze community dynamics described as trajectories. Community trajectory analysis takes trajectories as objects to be analyzed and compared geometrically.
# 
# We will be using the package `ecotraj`, and they have a great overview of the package with vignettes found [here](https://emf-creaf.github.io/ecotraj/index.html)
# 
# ### Trajectory data
# 
# To specify community dynamics, we need three data items:\ 
# - set of community states (i.e. coordinates in a space *Ω*), described using a distance matrix *d*\
# - A vector specifying the site (i.e. sampling unit) corresponding to each community state\
# - A vector specifying the survey (i.e. time point) corresponding to the sampling of each community state.\
# 
# CTA is based on the analysis of information in the distance matrix *Δ* = \[*d*\]. Therefore, it does not require explicit coordinates. This is an advantage because it allows the analysis to be conducted on arbitrary metric (or semi-metric) spaces. The choice of *d* is left to the user and will depend on the problem at hand.

library(tidyverse)
library(ecotraj)
library(vegan)
# load data and create community matrix
df = read_csv('data/LDWFseine_monthly2007.csv') 
df
#community matrix 
comm_matrix = df |> select(-basin, -month)

# site data
site = df$basin

# survey 
survey = df$month

# create bray curtis dissimilarity matrix
b_dist = vegdist(comm_matrix, method = "bray")

# ### Displaying trajectories
# To begin our analysis of the three trajectories, we display them in an ordination space, using function `trajectoryPCoA()`. Since *Ω*  has only two dimensions in this example, the Principal Coordinates Analysis (PCoA) on *d* displays the complete space:
#       

# plot trajectories
trajectoryPCoA(d = b_dist, 
               sites = site, 
               surveys = survey, 
               traj.colors = c("black","red", "blue", 'lightblue3', 'purple', 'pink'), 
               lwd = 2,
               survey.labels = T)

legend("bottomleft", col=c("black","red", "blue", 'lightblue3', 'purple', 'pink'), 
       legend=unique(df$basin), bty="n", lty=1, lwd = 2)


# `trajectoryPCoA()` uses PCoA to display the dissimilarity matrix, but other methods can be used. We can use `trajectoryPlot()` on visualizations (e.g., nMDS) as long as we have x and y coordinates 


# can be done with other ways
nmds = metaMDS(comm_matrix, distance = "bray", k = 2, try = 100)

trajectoryPlot(x = nmds$points, 
               sites = df$basin, 
               surveys = df$month, 
               traj.colors = c("black","red", "blue", 'lightblue3', 'purple', 'pink'), 
               lwd = 2,
               survey.labels = T)

legend("bottomleft", col=c("black","red", "blue", 'lightblue3', 'purple', 'pink'), 
       legend=unique(df$basin), bty="n", lty=1, lwd = 2)

# 
# We can also specify `selection =` in `trajectoryPCoA()` and `trajectoryPlot()` to highlight specific trajectories 

trajectoryPCoA(d = b_dist, 
               sites = df$basin, 
               surveys = df$month, 
               selection = 'Calcasieu',
               traj.colors = c("red"), 
               lwd = 2,
               survey.labels = T)

# We can also extract the raw information and plot ourselves in `ggplot` so it is easier to visualize

# convert nMDS to tibble
df_nmds = tibble(basin = site, 
                 month = survey, 
                 data.frame(nmds[["points"]])) |> 
      # add end points of each segment for arrow
      group_by(basin) |> 
      mutate(xend = lead(MDS1), yend = lead(MDS2))

library(viridis)
library(ggrepel)

# plot 
ggplot(df_nmds, aes(MDS1, MDS2))+
      geom_segment(aes(xend = xend, yend = yend, color = basin),
                   linewidth = 1,
                   arrow = arrow(length = unit(0.25, "cm")))+
      ggrepel::geom_text_repel(aes(label=month))+
      facet_wrap(~basin)+
      labs(x = 'NMDS1', y = 'NMDS2',
           color = 'Basin')+
      scale_color_viridis_d()+
      theme_bw()+
      theme(legend.position = 'none')


### Trajectory segment length and total length
# 
# `trajectoryLengths()` can be used to get the segment length of each time step and the overall distance.

trajectoryLengths(b_dist, site, survey)

tl = trajectoryLengths(b_dist, site, survey) |> 
      rownames_to_column(var = 'basin') |> 
      as_tibble()

# total trajectory length 
ggplot(tl, aes(basin, Trajectory, fill = basin))+
      geom_col()+
      labs(x = 'Basin', y = 'Trajectory total distance')+
      scale_fill_viridis_d()+
      theme_bw()+
      theme(legend.position = 'none')

# segment length 
tll = tl |> 
      pivot_longer(S1:S11, names_to = 'seg', values_to = 'len') |> 
      mutate(seg = factor(seg, levels = c('S1', 'S2','S3', 'S4',
                                          'S5', 'S6','S7', 'S8',
                                          'S9', 'S10','S11')))

tll |> group_by(basin) |> 
      summarize(mean = mean(len, na.rm = T),
                sd = sd(len, na.rm = T))

ggplot(tll, aes(basin, len, fill = basin))+
      geom_point(aes(color = basin), size = 1,
                 position = position_jitterdodge())+
      geom_boxplot(outliers = F, alpha = 0.6)+
      labs(x = 'Basin', y = 'Segment length')+
      scale_fill_viridis_d()+
      scale_color_viridis_d()+
      theme_bw()+
      theme(legend.position = 'none')


# ### Speed of change
# The speed of change is represented by the segment length divided by the time of change and is calculated with the formula 
# $$ S(s_i) = {L(s_i)}/{t_{i+1}-t_i}$$ 
#       where $S(s_i)$ is the speed of segment $i$, $L(s_i)$ is the length of segment $i$, $t_i$ start time for segment $i$, and $t_{i+1}$ is the time at end of segment $i$.

# plot 
ggplot(tll, aes(seg, len, group = basin, color = basin))+
      geom_point(size = 2.5)+
      geom_line(linewidth = 1)+
      labs(x = 'Segment', y = 'Speed of change')+
      scale_x_discrete(labels =c('Jan-Feb', 'Feb-Mar', 'Mar-Apr',
                                 'Apr-May', 'May-Jun', 'Jun-Jul',
                                 'Jul-Aug', 'Aug-Sep', 'Sep-Oct',
                                 'Oct-Nov', 'Nov-Dec'))+
      scale_color_viridis_d()+
      theme_bw()+
      theme(legend.position = 'none',
            axis.text.x = element_text(angle = 90))

### Distance from Baseline
# `trajectoryLengths()` can be used to get the relative distance to the baseline when `relativeToInitial = T`.

trajectoryLengths(b_dist, site, survey, relativeToInitial = T)

tl_init = trajectoryLengths(b_dist, site, survey,
                            relativeToInitial = T) |> 
      rownames_to_column(var = 'basin') |> 
      as_tibble() |> 
      pivot_longer(Lt1_t2:Lt1_t12, names_to = 'seg', 
                   values_to = 'len') |> 
      mutate(month = str_sub(seg,6) |> as.numeric())


ggplot(tl_init, aes(factor(month), len, group = basin, color = basin))+
      geom_point(size = 2.5)+
      geom_line(linewidth = 1)+
      labs(x = 'Segment', y = 'Distance from January')+
      scale_x_discrete(labels =c('Feb', 'Mar', 'Apr',
                                 'May', 'Jun', 'Jul',
                                 'Aug', 'Sep', 'Oct',
                                 'Nov', 'Dec'))+
      scale_color_viridis_d()+
      theme_bw()+
      theme(legend.position = 'none')


# ### Cumulative distance 
# You can use the output from the `trajectoryDistance()` to calculate the cumulative sum of the trajectory

tll = tll |> 
      group_by(basin) |> 
      mutate(dist_cuml = cumsum(len))


ggplot(tll, aes(seg, dist_cuml, group = basin, color = basin))+
      geom_point(size = 2.5)+
      geom_line(linewidth = 1)+
      labs(x = 'Month', y = 'Total segment length')+
      scale_x_discrete(labels =c('Feb', 'Mar', 'Apr',
                                 'May', 'Jun', 'Jul',
                                 'Aug', 'Sep', 'Oct',
                                 'Nov', 'Dec'))+
      scale_color_viridis_d()+
      theme_bw()+
      theme(legend.position = 'none')

# ## Exercises
# For these exercises use the [LDWF Calcasieu seine sampling dataset](https://github.com/SeascapeEcologyLab-workshops/BSC6926-B52_Fall2024/blob/main/data/Calcasieu.csv). 
# 
# 1. Using the Calcasieu dataset plot the community trajectory for each site.
# 
# 2. Calculate the total path length at each site.
# 
# 3. Plot the speed of change at each site.
# 
# 4. Plot the cumulative length at each time step for each site.