

# assumes tidyverse is loaded
interaction_line_plot <- function(y, x, by, data, ylim = NULL){
  
  Df <- group_by(data, {{x}}, {{by}}) %>% 
    summarise(mean = mean({{y}}),
              sem = sd({{y}})/sqrt(length({{y}})),
              .groups = 'drop')
  
  the_aes <- aes(x = {{ x }}, y = mean, group = {{ by }}, colour = {{ by }})

  p1 <- ggplot(Df, mapping = the_aes) + geom_point() + geom_line()
  
  p1 <- p1 + geom_linerange(aes(x = {{ x }}, ymin = mean - sem, ymax = mean + sem), size = .3)
  
  if (!is.null(ylim)){
    p1 <- p1 + scale_y_continuous(limits = ylim)
  }

  p1 + theme_classic() + scale_colour_brewer(palette = "Set1") 
  
}
