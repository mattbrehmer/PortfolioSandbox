#plotStackedTimeSeries.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#stacked time series plot ------------------------------------------------------------------------------

#plot differential heatmap plot of query result
output$stackedTimeSeriesPlot <- renderPlot({
	
	result.dt <- Query()
	
	#check for empty data
	if (nrow(result.dt) == 0 || is.null(result.dt$quantity))
		return()		
	
	#subset the data to the highest/lowest n spaces
	result.dt <- uniqueSubset(result.dt,nSpaces=getNSpaces(),sortDir=(getSortDir()=="desc"))
	
	result.dt$selected <- FALSE
	
	#if item is selected, highlight it
	for(i in 1:nrow(result.dt))
	{
		if (result.dt[i]$name == getHighlightSelect())
			result.dt[i]$selected <- TRUE
	}
	
	# 	#a stable colour scale for selected factor
	myColors <- c("grey50","red")
	names(myColors) <- levels(result.dt$selected)
	fillScale <- scale_fill_manual(name = "selected",values = myColors)
			
	#arrange data such that largest overall quantities are at top of stack
	result.dt[,meanQuantity:=mean(quantity,na.rm=TRUE), by=list(name)]
	
	if(getSortDir()=="desc")
	{
		result.dt$name <- reorder(result.dt$name,-result.dt$meanQuantity)
		result.dt <- arrange(result.dt,desc(meanQuantity))
	}
	else
	{
		result.dt$name <- reorder(result.dt$name, result.dt$meanQuantity)
		result.dt <- arrange(result.dt,meanQuantity)
	}
	
	#combine interval and year factors but retain respective within-factor orderings
	result.dt$interval <- interaction(result.dt$interval,result.dt$Year)
	
	base_size <- 9
	
	p <- ggplot(result.dt) +
		
		geom_area(
			aes(
				x = factor(interval),
				y = quantity,
				group = name,
				fill = selected
			),
			colour = "white",
			size = 1,
			position = "stack",
			alpha = 0.5
		) +
	
		#add plot style				
		theme_bw() + 
		
		theme(			
			legend.position = "none",
			legend.key.size = unit(2,"lines"),
			axis.title.x = element_blank(),
			axis.text.x = element_text(size = base_size, angle = 0,colour = "grey25"), 
			axis.text.y = element_text(size = base_size, hjust = 1,colour = "grey25"), 
			legend.title = element_blank(),
			legend.text = element_text(size = 12, hjust = 1,colour = "grey25"),
			axis.title.y = element_text(size = 12,angle = 0,colour = "grey25"),
			panel.margin = unit(1, "lines"),
			strip.background = element_rect(fill="white", linetype="blank"),
			strip.text = element_text(size = base_size,colour = "grey25")
		) +			
		
		#add manual fill scale
		fillScale +
				
		#add axes label
		ylab(paste(getUnits()))		
	
	#if there are more than 12 unique intervals (months), only show labels for quantiles
	if (length(levels(factor(result.dt$interval))) > 24)
	{
		intervals <- levels(factor(result.dt$interval))
		positions <- trunc(quantile(seq(intervals))) 
		p <- p + scale_x_discrete(expand = c(0, 0),breaks=c(intervals[positions]))
	}
	else
		p <- p + scale_x_discrete(expand = c(0, 0),breaks = c(levels(factor(result.dt$interval))))
	
	#draw the plot
	print(p)									
	
})	