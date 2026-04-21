###############################
### Define Shared Variables ###
###############################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
#             Shuyao Wang (shuyao_wang@fas.harvard.edu)
# Last updated: 4/21/26

### Define Plotting Objects -----

# Set plotting color scheme
yellow <- "#FFDB58"
dark_yellow <- "#B8860B"
orange <- "#FF7F50"
dark_orange <- "#C04F15"
green <- "#008080"
dark_green <- "#0A4F2D"
dark_pink <- "#941650"
dark_blue <- "#005493"

# Set yellow plot variables
dark_yellow_point <- geom_point(alpha = .5, size = 2.5, color = dark_yellow)
yellow_lm <- geom_smooth(method = "lm", size = 1.5, alpha = .25, color =  yellow, fill =  yellow) 
dark_yellow_loess <- geom_smooth(method = "loess", size = 1.5, alpha = .25, color = dark_yellow, fill = dark_yellow) 

# Set orange plot variables
dark_orange_point <- geom_point(alpha = .5, size = 2.5, color = dark_orange) 
orange_lm <- geom_smooth(method = "lm", size = 1.5, alpha = .25, color = orange, fill = orange) 
dark_orange_loess <- geom_smooth(method = "loess", size = 1.5, alpha = .25, color = dark_orange, fill = dark_orange)

# Set green plot variables
dark_green_point <- geom_point(alpha = .5, size = 2.5, color = dark_green)
green_lm <- geom_smooth(method = "lm", size = 1.5, alpha = .25, color = green, fill = green)
dark_green_loess <- geom_smooth(method = "loess", size = 1.5, alpha = .25, color = dark_green, fill = dark_green)

# Set plotting theme
emoreg_theme <- theme(title = element_text(size = 24, face = "bold", family = "Avenir"),
                      plot.title = element_text(hjust = .5),
                      axis.title.x = element_text(size = 24, family = "Avenir"),
                      axis.title.y = element_text(size = 24, family = "Avenir"),
                      axis.text.x = element_text(size = 18, colour = "black", family = "Avenir"),
                      axis.text.y = element_text(size = 18, colour = "black", family = "Avenir"),
                      legend.text = element_text(size = 18, colour = "black", family = "Avenir"),
                      legend.position = "bottom",
                      legend.key = element_rect(fill = "transparent", color = NA),
                      strip.text.x = element_text(size = 18, colour = "black", family = "Avenir"),
                      strip.text.y = element_text(size = 18, colour = "black", family = "Avenir"),
                      panel.grid.major = element_blank(), # remove grid marks
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(), 
                      axis.line = element_line(colour = "black"))

# Set age plot variables
age_x_axis <- scale_x_continuous(breaks = seq(10, 20, by = 2))

# Set prospection plot variables
fos_x_axis <- scale_x_continuous(breaks = seq(15, 60, by = 9))
fos_y_axis <- scale_y_continuous(breaks = seq(15, 60, by = 9))
fos_dash <- geom_hline(yintercept = c(15, 60), linetype = 'dashed') 
fos_line <- geom_hline(yintercept = seq(24, 51, by = 9), color = 'grey90') 

# Set inhibition plot variables
stroop_x_axis <- scale_x_continuous(breaks = seq(-.6, .2, by = .2), labels = scales::label_number(accuracy = 0.1))
stroop_y_axis <- scale_y_continuous(breaks = seq(-.6, .2, by = .2), labels = scales::label_number(accuracy = 0.1))
stroop_dash <- geom_hline(yintercept = 0, linetype = 'dashed') 
stroop_line <- geom_hline(yintercept = c(-.6, -.4, -.2, .2), color = 'grey90') 

# Set working memory plot variables
nback_x_axis <- scale_x_continuous(breaks = seq(-7.5, 2.5, by = 2.5))
nback_y_axis <- scale_y_continuous(breaks = seq(-7.5, 2.5, by = 2.5))
nback_dash <- geom_hline(yintercept = 0, linetype = 'dashed') 
nback_line <- geom_hline(yintercept = c(-7.5, -5, -2.5, 2.5), color = 'grey90') 

# Set situation selection plot variables
ss_y_axis <- scale_y_continuous(breaks = seq(6, 30, by = 6)) 
ss_dash <- geom_hline(yintercept = c(6, 30), linetype = 'dashed') 
ss_line <- geom_hline(yintercept = seq(12, 24, by = 6), color = 'grey90') 

# Set suppression plot variables
es_y_axis <- scale_y_continuous(breaks = seq(4, 20, by = 4))
es_dash <- geom_hline(yintercept = c(4, 20), linetype = 'dashed') 
es_line <- geom_hline(yintercept = seq(8, 16, by = 4), color = 'grey90') 

# Set reappraisal plot variables
cr_y_axis <- scale_y_continuous(breaks = seq(6, 30, by = 6))
cr_dash <- geom_hline(yintercept = c(6, 30), linetype = 'dashed') 
cr_line <- geom_hline(yintercept = seq(12, 24, by = 6), color = 'grey90') 
