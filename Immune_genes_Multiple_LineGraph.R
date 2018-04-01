setwd("/Users/aabvud/Dev/Thesis/")
data<- read.table("Boxplot_Test.txt",header = TRUE, sep = "\t")
# library(ggplot2)
library(tidyverse)
#bp <- ggplot(data, aes(Letter_coding,TNF)) 
#bp <- bp + geom_boxplot(aes(fill=Letter_coding))+geom_jitter(width=0.2)
#bp + ylab("Immune response")
quartz()
coral <- data %>% filter(str_detect(Organism, "Coral"))
C_dash <- data %>% filter(str_detect(Organism, "C-"))
plt_data <- left_join(coral, C_dash, by = "Island", suffix = c("","_C") ) 

p1 <- plt_data %>% ggplot(aes( color = Island)) +  
      geom_segment(aes( x = Letter_coding, y = Lectin_C, xend = Letter_coding_C, yend = Lectin_C_C)) +
      geom_point(aes(x = Letter_coding, y=Lectin_C)) +
      geom_point(aes(x = Letter_coding_C, y = Lectin_C_C)) +
      ylab("Relative Abundance") + ggtitle("Lectin_C") + theme(plot.title = element_text(hjust = 0.5))

p2 <- plt_data %>% ggplot(aes( color = Island)) +  
      geom_segment(aes( x= Letter_coding, y = DEATH, xend = Letter_coding_C, yend = DEATH_C)) +
      geom_point(aes(x= Letter_coding, y=DEATH)) +
      geom_point(aes(x = Letter_coding_C, y = DEATH_C)) +
      ylab("Relative Abundance") + ggtitle("DEATH") +theme(plot.title = element_text(hjust = 0.5))

p3 <- plt_data %>% ggplot(aes( color = Island)) +  
      geom_segment(aes( x= Letter_coding, y = TNFR, xend = Letter_coding_C, yend = TNFR_C)) +
      geom_point(aes(x= Letter_coding, y=TNFR)) +
      geom_point(aes(x = Letter_coding_C, y = TNFR_C)) +
      ylab("Relative Abundance") + ggtitle("TNFR") +theme(plot.title = element_text(hjust = 0.5))

p4 <- plt_data %>% ggplot(aes( color = Island)) +  
      geom_segment(aes( x= Letter_coding, y = C_related, xend = Letter_coding_C, yend = C_related_C)) +
      geom_point(aes(x= Letter_coding, y=C_related)) +
      geom_point(aes(x = Letter_coding_C, y = C_related_C)) +
      ylab("Relative Abundance") + ggtitle("C_related") +theme(plot.title = element_text(hjust = 0.5))

p5 <- plt_data %>% ggplot(aes( color = Island)) +  
      geom_segment(aes( x= Letter_coding, y = LRR_1, xend = Letter_coding_C, yend = LRR_1_C)) +
      geom_point(aes(x= Letter_coding, y=LRR_1)) +
      geom_point(aes(x = Letter_coding_C, y = LRR_1_C)) +
      ylab("Relative Abundance") + ggtitle("LRR_1") +theme(plot.title = element_text(hjust = 0.5))

p6 <- plt_data %>% ggplot(aes( color = Island)) +  
      geom_segment(aes( x= Letter_coding, y = TNF, xend = Letter_coding_C, yend = TNF_C)) +
      geom_point(aes(x= Letter_coding, y=TNF)) +
      geom_point(aes(x = Letter_coding_C, y = TNF_C)) +
      ylab("Relative Abundance") + ggtitle("TNF") +theme(plot.title = element_text(hjust = 0.5))

p7 <- plt_data %>% ggplot(aes( color = Island)) +  
      geom_segment(aes( x= Letter_coding, y = NACHT, xend = Letter_coding_C, yend = NACHT_C)) +
      geom_point(aes(x= Letter_coding, y=NACHT)) +
      geom_point(aes(x = Letter_coding_C, y = NACHT_C)) +
      ylab("Relative Abundance") + ggtitle("NACHT") +theme(plot.title = element_text(hjust = 0.5))

p8 <- plt_data %>% ggplot(aes( color = Island)) +  
      geom_segment(aes( x= Letter_coding, y = DED, xend = Letter_coding_C, yend = DED_C)) +
      geom_point(aes(x= Letter_coding, y=DED)) +
      geom_point(aes(x = Letter_coding_C, y = DED_C)) +
      ylab("Relative Abundance") + ggtitle("DED") +theme(plot.title = element_text(hjust = 0.5))

p9 <- plt_data %>% ggplot(aes( color = Island)) +  
      geom_segment(aes( x= Letter_coding, y = CARD, xend = Letter_coding_C, yend = CARD_C)) +
      geom_point(aes(x= Letter_coding, y=CARD)) +
      geom_point(aes(x = Letter_coding_C, y = CARD_C)) +
      ylab("Relative Abundance") + ggtitle("CARD") +theme(plot.title = element_text(hjust = 0.5))

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9, cols=3)

