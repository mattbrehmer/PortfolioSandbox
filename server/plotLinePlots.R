#plotLinePlots.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#line plots ------------------------------------------------------------------------------

#plot differential heatmap plot of query result
output$timeSeriesPlot <- renderPlot({
	
	result.dt <- Query()
	
	#check for empty data
	if (nrow(result.dt) == 0 || is.null(result.dt$quantity))
		return()			
# 	
# 	#a line type for each unique year (solid or dashed)
# 	myLinetype <- c(5,1)
# 	names(myLinetype) <- levels(result.dt$Year)
# 	linetypeScale <- scale_linetype_manual(name = "Year",values = myLinetype)
	
	#subset the data to the highest/lowest n spaces
	result.dt <- uniqueSubset(result.dt,nSpaces=getNSpaces(),sortDir=(getSortDir()=="desc"))
	
	result.dt$selected <- FALSE
	
	#if item is selected, highlight it
	for(i in 1:nrow(result.dt))
	{
		if (result.dt[i]$name == getHighlightSelect())
			result.dt[i]$selected <- TRUE
	}
	
	#a stable colour scale for selected factor
	myColors <- c("grey50","red")
	names(myColors) <- levels(result.dt$selected)
	colScale <- scale_color_manual(name = "selected",values = myColors)
	
	#reorder name by meanQuantity such that facets are correctly ordered
	result.dt <- result.dt[,meanQuantity:=mean(quantity,na.rm=TRUE), by=list(name)]	
	
	if(getSortDir()=="desc")
		result.dt$name <- reorder(result.dt$name,-result.dt$meanQuantity)
	else
		result.dt$name <- reorder(result.dt$name, result.dt$meanQuantity)
	
	result.dt <- result.dt[,maxQ:=max(quantity), by=list(name)]		
	result.dt <- result.dt[,minQ:=min(quantity), by=list(name)]		
	
	base_size <- 9
	
	#combine interval and year factors but retain respective within-factor orderings
	result.dt$interval <- interaction(result.dt$interval,result.dt$Year)
	
	p <- ggplot(result.dt) +
		
		# draw the line (quantity (y) across intervals (x))
		geom_line(
			aes(
				x = interval,
				y = quantity,
				group = name,
				label = name,
				colour = selected
			),
			linetype = "solid",
			size = 1) +
		
		# draw the line (quantity (y) across intervals (x))
		geom_line(
			data = result.dt[Year==2013,],
			aes(
				x = interval,
				y = baselineQuantity,
				group = name,
				label = name,
				colour = selected
			),
			linetype = "dashed",
			alpha = 0.5,
			size = 1) +
	
		#draw a red point for each name's max value
		geom_point(
			data = result.dt[quantity==maxQ & quantity!=0,],
			aes(
				x = interval,
				y = quantity
			),
			colour = "red",
			size = 5) +
		
		#draw a blue point for each name's min value
		geom_point(
			data = result.dt[quantity==minQ & quantity!=0,],
			aes(
				x = interval,
				y = quantity
			),
			colour = "blue",
			size = 5) +
		
		#add plot style				
		theme_bw() + 
		
		theme(
			legend.position = "none", 
			legend.key.size = unit(2,"lines"),
			axis.title.x = element_blank(),
			panel.margin = unit(3, "lines"),
			strip.background = element_rect(fill="white", linetype="blank"),
			axis.text.x = element_text(size = base_size, angle = 0, colour = "grey25"), 
			axis.text.y = element_text(size = base_size, hjust = 1,colour = "grey25"), 
			strip.text = element_text(size = 12, colour = "grey25"),				
			legend.title = element_text(size = 12, colour = "grey25"),
			legend.text = element_text(size = 12, hjust = 1,colour = "grey25"),
			axis.title.y = element_text(size = 12,angle = 0,colour = "grey25")
		) +			
		
		#add axes label
		ylab(getUnits()) +
		
		#specify scales
		ylim(0,max(result.dt$quantity) + 0.10*max(result.dt$quantity)) +
		
# 		#add linetype scale
# 		linetypeScale +
		
		#add manual fill scale
		colScale +
		
		#facet by name into 4xn grid
		facet_wrap(~ name, ncol = 4, scales = "free_x")
	
	#if there are more than 12 unique intervals (months), only show labels for quantiles
	if (length(levels(factor(result.dt$interval))) > 6)
	{
		intervals <- levels(factor(result.dt$interval))
		positions <- trunc(quantile(seq(intervals))) 
		p <- p + scale_x_discrete(expand = c(0, 0), breaks=c(intervals[positions]))
	}
	else
		p <- p + scale_x_discrete(expand = c(0, 0), breaks = c(levels(factor(result.dt$interval))))
	
	#draw the plot
	print(p)									
	
}, height = plotHeightTimeSeries)	