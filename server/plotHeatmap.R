#plotHeatmap.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#Heatmap ------------------------------------------------------------------------------

#plot heatmap plot of query result
output$heatmapPlot <- renderPlot({
	
	result.dt <- Query()
	
	#check for empty data
	if (nrow(result.dt) == 0 || is.null(result.dt$quantity))
		return()
	
	#subset the data to the highest/lowest n spaces
	result.dt <- uniqueSubset(result.dt,nSpaces=getNSpaces(),sortDir=getSortDir()=="desc")
	
	units <- getUnits()
	minLimit <- 0
	maxLimit <- max(result.dt$quantity,na.rm=TRUE)
	
	#gradient scale for tiles such that high values are red, low values are white, missing values are grey
	gradientScale <- scale_fill_gradient(name=paste(units),
																			 low = "white",  
																			 high = scales::muted("red"),
																			 space = "rgb", 
																			 na.value = "grey85", 
																			 guide = "legend",
																			 limits = c(minLimit,maxLimit))
	
	#gradient scale for label text
	colScale <- scale_colour_gradient(name=paste(units),
																		high = "white", 													
																		low = "grey33",
																		space = "rgb", 
																		na.value = "grey85",
																		guide = "none",
																		limits = c(minLimit,maxLimit))	
	
	if(getDiffAbsRel()!="no")
	{
		#compute the differential if requested
		result.dt <- computeDifferential(result.dt,getOmitOutages()=="y")
		
		#absolute or relative differentials, set max/min scale bounds
		if(getDiffAbsRel()=="rel")
		{
			result.dt[,diffQuant:=quantityRelDiff]
			maxLimit <- min(100,max(result.dt$diffQuant,na.rm=TRUE))
			minLimit <- max(-100,min(result.dt$diffQuant,na.rm=TRUE))	
			units <- paste(units," (%)")
		}
		else
		{
			result.dt[,diffQuant:=quantityAbsDiff]
			maxLimit <- max(result.dt$diffQuant,na.rm=TRUE) 
			minLimit <- min(result.dt$diffQuant,na.rm=TRUE)
		}
		
		#diverging colour scale for tiles such that increases values are red, decreases are blue, and unchanged is white, missing values are grey
		gradientScale <- scale_fill_gradient2(name=paste(units),
																					low = scales::muted("blue"), 
																					mid = "white", 
																					high = scales::muted("red"),
																					midpoint = 0, 
																					space = "rgb", 
																					na.value = "grey85", 
																					guide = "legend",
																					limits=c(minLimit, maxLimit))
		
		#diverging gradient scale for label text
		colScale <- scale_colour_gradient2(name=paste(units),
																			 low = "white", 
																			 high = "white", 													
																			 mid = "grey33",
																			 midpoint=0,
																			 space = "rgb", 
																			 na.value = "grey85",
																			 guide = "none",
																			 limits=c(minLimit, maxLimit))		
	}
	
	base_size <- 12	
	
	#combine interval and year factors but retain respective within-factor orderings
	result.dt$interval <- interaction(result.dt$interval,result.dt$Year)
		
	p <- ggplot(result.dt)
		
	#specify the dimensions of the plot: intervals (x) by names (y), sort order
	if(getSortDir()=="desc")
	{
		p <- p + aes(
			x = factor(interval),
			y = reorder(name,quantity)
		)
	}
	else
	{
		p <- p + aes(
			x = factor(interval),
			y = reorder(name,-quantity)
		)
	}
	
	if(getDiffAbsRel()=="no")
	{
		#add tiles for each interval+name combination
		p <- p + geom_tile(aes(fill=quantity))
		
		#add text labels to cells if #intervals <= 12
		if (length(levels(factor(result.dt$interval))) <= 12)
			p <- p + geom_text(
				aes(label=signif(quantity,digits=3),colour=quantity),
				size=4,
				fontface="plain")
	}
	
	else
	{		
		#add tiles for each interval+name combination
		p <- p + geom_tile(aes(fill=diffQuant))
		
		#add text labels to cells if #intervals <= 12
		if (length(levels(factor(result.dt$interval))) <= 12)
			p <- p + geom_text(
				aes(label=signif(diffQuant,digits=3),colour=diffQuant),
				size=4,
				fontface="plain")
	}
		
	#specify axes
	p <- p + scale_y_discrete(expand = c(0, 0)) +
		
		#add colour scales
		gradientScale + 
		colScale +
		
		#add plot style
		theme_bw() +	
		
		theme(
			legend.position = "left", 
			legend.key.size = unit(2,"lines"),
			plot.background = element_rect(fill="white", linetype="blank"),
			legend.text = element_text(size = base_size, angle = 0, hjust = 1,colour = "grey25"),
			legend.title = element_text(size = base_size,colour = "grey25"),
			axis.text.x = element_text(size = base_size, angle = 0,colour = "grey25"), 
			axis.text.y = element_text(size = base_size, hjust = 1,colour = "grey25")
		) +
				
		#don't show axes labels
		labs(x = "", y = "")
	
	#if there are more than 12 unique intervals (weeks), only show labels for quantiles
	if (length(levels(factor(result.dt$interval))) > 12)
	{
		intervals <- levels(factor(result.dt$interval))
		positions <- trunc(quantile(seq(intervals))) 
		p <- p + scale_x_discrete(expand = c(0, 0),breaks=c(intervals[positions]))
	}
	
	else
		p <- p + scale_x_discrete(expand = c(0, 0),breaks = c(levels(factor(result.dt$interval))))
	
	#draw the plot
	print(p)																
	
},height = plotHeight)