# a new theme
library(ggplot2)

theme_minimal_167 <- function(base_size = 12, 
                              base_family = "Palatino Linotype", 
                              caption.top = 2,
                              caption.bottom = 10,
                              ...
) {
  # take the starter theme
  theme_minimal(base_size = base_size, base_family = base_family) + 
  # then edit that them
    theme(
      text = element_text(size = base_size, 
                          family = base_family, ...
      ),
      plot.title = element_text(size = rel(1.2), face = "bold", ...),
      
      # add a buffer below caption for an image
      plot.caption = element_text(hjust = 0,
                                  margin = margin(t = caption.top, b = caption.bottom),
                                  ...
                                  ),
      
      axis.title = element_text(size = rel(1), face = "bold", ...),
      legend.title = element_text(size = rel(1), face = "bold", ...),
      ...
    )
}

theme_classic_167 <- function(base_size = 12, 
                              base_family = "Palatino Linotype", 
                              ...
) {
  # take the starter theme
  theme_classic(base_size = base_size, base_family = base_family) + 
    # then edit that them
    theme(
      
      text = element_text(size = base_size, 
                          family = base_family
                          ),
      plot.title = element_text(size = rel(1.2), face = "bold"),
      axis.title = element_text(size = rel(1), face = "bold"),
      #axis.title.x = element_text(size = rel(.80)),
      #axis.title.y = element_text(size = rel(.80)),
      #axis.text.x = element_text(size = rel(.80)),
      #axis.text.y = element_text(size = rel(.80)),
      legend.title = element_text(size = rel(1), face = "bold"),
      ...
    )
}

theme_167_classic <- function(base_size = 12, 
                              base_family = "Palatino Linotype", 
                              ...
                              ) {
  
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    
  ggplot2::theme(
      # font
      text = ggplot2::element_text(family = base_family), # changed in base
      
      # title
      plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      plot.caption = ggplot2::element_text(size = rel(.75), hjust = 0,
                                  margin = margin(1,1,1,1)), 
      
      # facets/panels
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      
      # axes 
      axis.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
      axis.text  = ggplot2::element_text(size = rel(0.70), face = "bold"),
      # for arrowed axes
      # axis.line  = ggplot2::element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      
      # legend 
      legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
      legend.text  = ggplot2::element_text(size = rel(0.70), face = "bold"),
      legend.key   = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key.size   = unit(1.5, "lines"),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      # 
      # 
      strip.background = ggplot2::element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0)),
      ...
  )
}

#####################################################################################

theme_167_minimal <- function(base_size = 14, base_family = "Palatino Linotype", ...) {
  
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    
  ggplot2::theme(
      # font
     
      
      # title
      plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      plot.caption = element_text(size = rel(.75), hjust = 0,
                                  margin = margin(1,1,1,1)), 
      
      # facets/panels
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      
      # axes 
      axis.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
      axis.text  = ggplot2::element_text(size = rel(0.70), face = "bold"),
      # for arrowed axes
      # axis.line  = ggplot2::element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      
      # legend 
      legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
      legend.text  = ggplot2::element_text(size = rel(0.70), face = "bold"),
      legend.key   = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key.size   = unit(1.5, "lines"),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      # 
      # 
      strip.background = ggplot2::element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0)),
      
      
      text = ggplot2::element_text(family = base_family), 
      ...
    )
}

