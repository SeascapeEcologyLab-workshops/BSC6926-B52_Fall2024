reeffish = read_csv("./data/MCR_LTER_Annual_Fish_Survey_20230615.csv")

reeffish_summary = reeffish |> 
      group_by(Year, Taxonomy, Site) |> 
      summarize(n = sum(Count)) |> 
      ungroup()

reeffish_summary2 = reeffish |> 
      group_by(Taxonomy) |> 
      summarize(n = sum(Count)) |> 
      ungroup() |> 
      arrange(-n) |> 
      slice_head(n = 10)

ggplot(reeffish_summary |> filter(Taxonomy %in% reeffish_summary2$Taxonomy), aes(as.numeric(Year), n, color = factor(Site)))+
      geom_point()+
      geom_line()+
      facet_wrap(~Taxonomy)

# ggplot(reeffish_summary |> filter(Taxonomy %in% reeffish_summary2$Taxonomy), aes(as.numeric(Year), n, color = Taxonomy))+
#       geom_point()+
#       geom_line()