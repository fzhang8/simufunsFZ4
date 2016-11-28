
LD_plot <- function(D,FstSingulars,Demeanlevel,title = "test plot"){
	
	dtset <- data.frame(D,FstSingulars,Demeanlevel)
	
	p <- ggplot(dtset, aes(x=D, y=FstSingulars, group=Demeanlevel)) + 
				geom_line(aes(color = Demeanlevel),size = 1.5) + 
				geom_point(aes(shape = Demeanlevel),size = 3)+
				scale_shape_discrete(name = "Demean") + 
				scale_colour_hue(name = "Demean") + 
				scale_x_continuous(breaks=seq(min(D),max(D),(max(D) - min(D))/10)) +
				geom_vline(xintercept = 0, size = 1, color = "blue") +
				labs(title = title) +
				xlab("LD coefficient D") +
				ylab("Percentage of First Singular (%)") +
				theme_minimal() + 
				theme_bw() + 
				theme(plot.title = element_text(size = 20),
							axis.text=element_text(size=15),
							axis.title=element_text(size=20),
							legend.key.size =  unit(1,"cm"),
							legend.title=element_text(size=20),
							legend.text=element_text(size=20))
							
	plot(p)
}

