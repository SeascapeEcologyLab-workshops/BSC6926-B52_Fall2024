reeffish = read_csv("./data/MCR_LTER_Annual_Fish_Survey_20230615.csv")

reeffish_top = reeffish |> 
      group_by(Taxonomy, Coarse_Trophic) |> 
      summarize(n = sum(Count)) |> 
      group_by(Coarse_Trophic) |> 
      slice_max(n) |> 
      ungroup() |> 
      filter(!(Coarse_Trophic %in% c('na', NA, 'Unknown_coarse')))

df = reeffish |> 
      filter(Taxonomy %in% reeffish_top$Taxonomy) |> 
      group_by(Year, Taxonomy) |> 
      summarize(Nt = sum(Count)) |> 
      group_by(Taxonomy) |> 
      mutate(lambda = lead(Nt)/Nt,
             percap = (lead(Nt)-Nt)/Nt)
library(DescTools)

g = df |>  
      group_by(Taxonomy, Site) |> 
      summarize(mean = Gmean(lambda, na.rm = T),
                sd = Gsd(lambda, na.rm = T))


g = df |>  
      group_by(Taxonomy) |> 
      summarize(mean = Gmean(lambda, na.rm = T),
                sd = Gsd(lambda, na.rm = T)) |> 
      mutate(r = mean -1)

ggplot(df, aes(as.numeric(Year), Nt, color = as.factor(Site)))+
      geom_point()+
      geom_line()+
      facet_wrap(~Taxonomy, scales = 'free')

df = df |> 
      left_join(g)

ggplot(df, aes(as.numeric(Year), lambda, color = Taxonomy))+
      geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd,fill = Taxonomy), alpha = 0.2)+
      geom_point()+
      geom_hline(data = g, aes(yintercept = mean, color = Taxonomy), linewidth = 1)+
      
      
      #geom_line()+
      facet_wrap(~as.numeric(Site), scales = 'free')

ggplot(g, aes(Taxonomy, mean, color = Taxonomy))+
      geom_hline(aes(yintercept = 1), linewidth = 1)+
      geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd), size = 1, linewidth = 1.5)+
      facet_wrap(~as.numeric(Site), scales = 'free')

ggplot(df, aes(as.numeric(Year), Nt))+
      geom_point()+
      geom_line()+
      facet_wrap(~Taxonomy, scales = 'free')

ggplot(df, aes(Nt, percap, color = Taxonomy)) + 
      geom_smooth(method = 'lm') +
      geom_point(size = 2) +
      facet_wrap(~Taxonomy, scales = 'free')+
      labs(x = expression(italic(N[t])),
           y =  expression('per capita growth ('~italic(Delta~N[t]/N[t])~')'),
           title = 'Per Capita Growth')+
      theme_bw()

# ggplot(reeffish_summary |> filter(Taxonomy %in% reeffish_summary2$Taxonomy), aes(as.numeric(Year), n, color = Taxonomy))+
#       geom_point()+
#       geom_line()

A = matrix(c(0,0,100,0.2,0.3,0,0,0.4,0.1), nrow = 3, byrow = T)
A

library(popbio)
n = c(egg = 100, larvae = 0, adult = 0)

pop = pop.projection(A, n, 50)

pop
stage.vector.plot(pop$stage.vectors)
