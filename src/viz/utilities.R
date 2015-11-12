ggToFile <- function(filename = 'plot.png', w = 6.5, h = 4,  res = 450, ...){
    ggsave(filename, width = w, height = h, units = 'in', dpi = res, ...)
}


formatNice<- function(x){
    format(x, big.mark = ',', scientific = FALSE)
}

#example use 
# last_plot() + scale_y_continuous(labels = formatNice)


formatPct <- function(x) 
    paste0(format(x * 100, big.mark = ',', scientific = FALSE, trim = TRUE), "%")

#example use
# last_plot() + scale_y_continuous(labels = formatPct)


