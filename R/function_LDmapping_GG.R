
AllLDmappingGG <- function(obsvX, obsvY, mind, maxd, mn = "latmean",dots = 10,
													inner_n = input$n,
													inner_Gcolmn = input$Gcolmn,
													inner_actvGcolmn = input$actvGcolmn,
													inner_latG1H1 = LtoutG()$G1H1,
													inner_latG2H2 = LtoutG()$G2H2,
													inner_latG = LtoutG()$GH,
													inner_Gprob = input$Gprob,
													inner_gprob = input$gprob){
		
		D <- c()
		Percentage <- c()
		Group <- c()
		
		for(i in c(FALSE,TRUE)){
				for(LD in c(0,seq(mind + 0.001, maxd - 0.001, (maxd - mind - 0.002)/dots))){
						obsvG <- generate_obs_G(n = inner_n,
														Gcolmn = inner_Gcolmn,
														actvGcolmn = inner_actvGcolmn,
														latG1H1 = inner_latG1H1,
														latG2H2 = inner_latG2H2,
														latG = inner_latG,
														D = LD,
														Gprob = inner_Gprob,
														gprob = inner_gprob,
														Gcenter = i,
														Gmean = mn)
														
						result <- svd_analysis(obsvY,obsvX,obsvG$G)
						
						fsp <- result$singulars[1] / sum(result$singulars) * 100
						
						D <- c(D, LD)
						Percentage <- c(Percentage, fsp)
						Group <- c(Group, ifelse(i,"YES","NO"))
				}
		}
		
		return(list(D = D, Percentage = Percentage, Group = Group, Meantype = mn))
		
}