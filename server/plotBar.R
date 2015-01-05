#plotBar.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#bar chart ------------------------------------------------------------------------------

#plot bar chart of query result
output$barPlot <- renderPlot({
	
	result.dt <- Query()
	
	#check for empty data
	if (nrow(result.dt) == 0 || is.null(result.dt$quantity))
		return()		  	
	
	#subset the data to the highest/lowest n spaces
	result.dt <- uniqueSubset(result.dt,nSpaces=getNSpaces(),sortDir=(getSortDir()=="desc"))
	
	#compute the differential if requested
	result.dt <- computeDifferential(result.dt,getOmitOutages()=="y")
	
	#absolute or relative differentials, set max/min scale bounds
	if(getDiffAbsRel()=="rel")
		result.dt[,diffQuant:=quantityRelDiff]	
	else
		result.dt[,diffQuant:=quantityAbsDiff]
	
	#compute mean quantity for current time window
	result.dt[,meanQuantity:=mean(quantity,na.rm=TRUE),by=list(name)]	
	result.dt[,meanDiffQuant:=mean(diffQuant,na.rm=TRUE),by=list(name)]
	
	result.dt <- unique(result.dt[, list(name,meanQuantity,meanDiffQuant) ])
	
	if (getDiffAbsRel() == "no")
	{	
		result.dt[,meanYQuantity:=meanQuantity]	
		#determine largest value for range capping
		minLimit <- 0
		maxLimit <- max(result.dt$meanYQuantity,na.rm=TRUE,0)
	}
	else
	{
		result.dt[,meanYQuantity:=meanDiffQuant]	
		minLimit <- min(result.dt$meanYQuantity,na.rm=TRUE)
		maxLimit <- max(result.dt$meanYQuantity,na.rm=TRUE) 
	}
	
	base_size <- 12
	
	p <- ggplot(result.dt)
	
	#specify bars and sort order order, dodge the bars by year 
	if(getSortDir()=="desc")
	{
		p <- p + aes(
			x = reorder(name,meanQuantity),
			y = meanYQuantity
		)
	}
	else
	{
		p <- p + aes(
			x = reorder(name,-meanQuantity),
			y = meanYQuantity
		)
	}
	
	p <- p + geom_hline(yintercept=c(0),colour="grey75",size=0.5) +
		
		geom_bar(alpha=0.5, width=0.5, stat='identity') +		
		
		geom_text(
			data = result.dt[result.dt$meanYQuantity != maxLimit],
			aes(
				label=signif(meanYQuantity,digits=3),
				y = meanYQuantity
			),
			size=4,
			hjust=-0.5,
			fontface="plain"
		) +
		
		geom_text(
			data = result.dt[result.dt$meanYQuantity == maxLimit || result.dt$meanYQuantity < 0],
			aes(
				label=signif(meanYQuantity,digits=3),
				y = meanYQuantity
			),
			size=4,
			hjust=1.5,
			color = "white",
			fontface="plain"
		) +
	
		#add plot style		
		theme_bw() + 			
		
		theme(
			legend.position = "top", 
			legend.key.size = unit(2,"lines"),
			plot.background = element_rect(fill="white", linetype="blank"),
			axis.text.x = element_text(size = base_size, angle = 0,colour = "grey25"), 
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank(),
			legend.title = element_text(size = base_size,colour = "grey25"),
			legend.text = element_text(size = base_size,colour = "grey25"),
			axis.title.x = element_blank(),
			axis.title.y = element_blank()
		) +
		
		scale_x_discrete() +
				
		#flip axes such that bars are horizontal
		coord_flip(ylim = c(minLimit*1.1,maxLimit*1.1))
	
	#draw the plot
	print(p)																
	
},height = plotHeight)