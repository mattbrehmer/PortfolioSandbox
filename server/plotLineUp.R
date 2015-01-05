#plotLineUp.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#LineUp plot ------------------------------------------------------------------------------

#plot lineUp plot of query result
output$lineUpPlot <- renderPlot({
	
	result.dt <- Query()						
	
	#check for empty data, and don't plot 4h intervals
	if (nrow(result.dt) == 0 || is.null(result.dt$quantity))
		return()		  
	
	#subset the data to the highest/lowest n spaces
	result.dt <- uniqueSubset(result.dt,nSpaces=getNSpaces(),sortDir=(getSortDir()=="desc"))
	
	#combine interval and year factors but retain respective within-factor orderings
	if(getIntervalSelect() != "Year")
		result.dt$interval <- interaction(result.dt$interval,result.dt$Year,drop=TRUE)
	
	#compute ranks and rank deltas for each interval, # of unique ranks for each item and interval
	ranked.dt <- assignRanks(result.dt,sortDir=(getSortDir()=="desc")) 
	
	#determine the number of unique intervals
	numIntervals <- length(unique(ranked.dt$interval))
	
	base_size <- 12
	
	p <- ggplot(
		ranked.dt		
	) +
		
		#specify the line indicating changes in rank over the intervals
		geom_line(
			aes(
				x = factor(interval), 
				y = Rank, 
				group = name,
				colour = name, 
				alpha = RankRange,
				label = name
			),
			size=0) + 
		
		#specify the bars indicating absolute quantities in each intervals, 
		geom_rect(
			aes(
				xmin = intervalIndex, 
				xmax = intervalIndex + sQuant, 
				ymin = Rank + 0.25, 
				ymax = Rank - 0.25,
				colour = name,
				alpha = RankRange,
				fill = name, 
				label = name)
		)
	
	if(numIntervals>1)	
		#specify the line indicating changes in rank over the intervals
		p <- p + geom_segment(
			data = ranked.dt[intervalIndex!=max(intervalIndex)],
			aes(
				x = intervalIndex + sQuant, 
				xend = intervalIndex + 1, 
				y = Rank, 
				yend = nextRank,
				group = name,
				colour = name,
				alpha = RankRange,
				label = name),
			size=1)	
		
		#add text labels to cells if #intervals <= 12
		if (length(levels(factor(result.dt$interval))) <= 12)
			p <- p + geom_text(
				aes(
					x = (intervalIndex + intervalIndex + sQuant) / 2,
					y = Rank - 0.40,
					label=signif(quantity,digits=3),
					colour = name
				),
				size=5,
				fontface="plain"
			) 
		
		#annotate the chart with two transparent rectangles on either side so as to ensure the visibility of the plots and labels
		p <- p + annotate("rect", xmin = -1*(numIntervals/5), xmax = 0, ymin = -Inf, ymax = Inf, alpha = 0.0) +			
		annotate("rect", xmin = numIntervals + 1, xmax = numIntervals + 1 + numIntervals/5, ymin = -Inf, ymax = Inf, alpha = 0.0) +
		
		#add plot style
		
		theme_bw() + 
		
		theme(
			legend.position = "none", 
			panel.border = element_rect(color="grey75"),
			plot.background = element_rect(fill="white", linetype="blank"),
			axis.ticks = element_blank(), 
			panel.margin = unit(1, "lines"),
			axis.text.y = element_blank(),
			panel.grid.major.y = element_blank(),
			panel.grid.minor.y = element_blank(),
			axis.text.x = element_text(size = base_size, angle = 0,colour = "grey25") 
		) + 
		
		#add qualitative color scale to lines, bars, labels
		scale_colour_hue(breaks = c(levels(factor(result.dt$name))), h = c(0, 360) + 15, c = 50,
										 l = 70, h.start = 0, direction = 1,
										 na.value = "grey50") +
		
		#add x axis ticks for each interval
		scale_x_discrete(breaks = c(levels(ranked.dt$interval))) +
		
		#add alpha scale to ensure that names that change rank are more salient than those that do not
		scale_alpha(range=c(0.075,0.75)) +
		
		#scale the y axis to the range of ranks
		ylim(min(ranked.dt$Rank) - 1, max(ranked.dt$Rank) + 1) +
		
		#don't show axes labels
		ylab(NULL) +			
		xlab(NULL) 
	
	#if only one interval is shown, display name labels to left of bar
	if (numIntervals == 1) {
		p <- p + geom_dl(
			aes(x = intervalIndex - 0.1,
					y = Rank, 
					colour = name, 
					label = name), 
			method = "first.points",
			cex = 5
		)
	}
	else { #if >1 interval is shown, display name labels to left and right of each line
		p <- p + geom_dl(
			aes(x = intervalIndex - 0.1,
					y = Rank, 
					colour = name, 
					label = name), 
			method = "first.qp",
			cex = 5
		) +
			
			geom_dl(
				aes(x = intervalIndex + 0.6,
						y = Rank, 
						colour = name, 
						label = name), 
				method = "last.qp",
				cex = 5
			)
	}
	
	#if there are more than 12 unique intervals (weeks), only show labels for quantiles
	if (length(levels(factor(ranked.dt$interval))) > 12)
	{
		intervals <- levels(factor(ranked.dt$interval))
		positions <- trunc(quantile(seq(intervals))) 
		p <- p + scale_x_discrete(breaks=c(intervals[positions]))
	}
	
	#draw the plot
	print(p)				
	
},height = plotHeightLineUp)