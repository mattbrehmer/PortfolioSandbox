#plotBox.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#box plot ------------------------------------------------------------------------------

#plot boxplot of non-aggregated data
output$boxPlot <- renderPlot({
	
	result.dt <- RawQuery()
	
	#check for empty data
	if (nrow(result.dt) == 0 || is.null(result.dt))
		return()		 
	
	#subset to top/bottom n
	subData.dt <- data.table(result.dt)
	setkey(subData.dt,name)
	subData.dt[,meanValue:=mean(value,na.rm=TRUE), by=list(name)]
	subData.dt <- subset(subData.dt, select = c(name,meanValue))	
	subData.dt <- unique(subData.dt[, list(name,meanValue) ])
	subData.dt <- arrange(subData.dt,desc(meanValue))	
	
	#take head or tail or table, based on user input and size specified
	if (getSortDir()=="desc")
		subData.dt <- head(subData.dt,n=getNSpaces())
	else
		subData.dt <- tail(subData.dt,n=getNSpaces())
	
	result.dt <- result.dt[result.dt$name %in% subData.dt$name,]
	
	#absolute or relative differentials, set max/min scale bounds
	if(getDiffAbsRel()=="no")	
		result.dt[,yVal:=value]
	
	else if(getDiffAbsRel()=="rel")
	{		
		#if value != 0 and baseline == 0, diff is +100%, if value == 0 and baseline != 0, diff is -100%
		result.dt[,yVal:= (value - baseline) / baseline]	
		result.dt <- result.dt[yVal < -100,yVal:=-100]
		result.dt <- result.dt[yVal > 100,yVal:=100]
		
		if(getOmitOutages()=="y") #prevent these values from skewing the scale
		{
			result.dt <- result.dt[value==0 & baseline==0,yVal:=NA]
			result.dt <- result.dt[value!=0 & baseline==0,yVal:=NA]
			result.dt <- result.dt[value==0 & baseline!=0,yVal:=NA]
		}
	}
	else
	{
		result.dt[,yVal:=value - baseline]	
		
		if(getOmitOutages()=="y") #prevent these values from skewing the scale
		{
			result.dt <- result.dt[value!=0 & baseline==0,yVal:=NA]
			result.dt <- result.dt[value==0 & baseline!=0,yVal:=NA]
		}
	}
	
	#subset to real quantities, omit divide / zero errors
	result.dt <- result.dt[yVal>-Inf]
	result.dt <- result.dt[yVal<Inf]
	
	result.dt[,maxWhiskerValue:=as.double(max(boxplot.stats(yVal)$stats[5],0)), by=list(name)]	
	result.dt[,minWhiskerValue:=as.double(min(boxplot.stats(yVal)$stats[1],0)), by=list(name)]	
	
	#if outliers omitted, limit scale to highest whisker * 1.05
	if (getOmitOutliers()=="y")
	{
		yCappedMax <- max(result.dt$maxWhiskerValue, na.rm=TRUE)*1.05
		yCappedMin <- min(result.dt$minWhiskerValue, na.rm=TRUE)*1.05
	}
	
	base_size <- 12
	
	#determine largest value for range capping
	maxVal <- max(result.dt$yVal,na.rm=TRUE,0)*1.05
	minVal <- min(result.dt$yVal,na.rm=TRUE,0)
	
	p <- ggplot(result.dt)
	
	#specify bars and sort order order, dodge the bars by year 
	if(getSortDir()=="desc")
	{
		p <- p + aes(
			x = reorder(name,value),
			y = yVal
		)
	}
	
	else
	{
		p <- p + aes(
			x = reorder(name,-value),
			y = yVal
		)		
	}
	
	p <- p + geom_hline(yintercept=c(0),colour="grey75",size=0.5) +
		
		geom_boxplot(
		alpha=0.5, 
		width=0.5, 
		fill="grey50",
		colour="grey50",
		outlier.colour="grey50",
		outlier.size=1.5) +
	
		#add plot style
		theme_bw() + 
		
		theme(
			legend.position = "top", 
			legend.key.size = unit(2,"lines"),
			axis.text.x = element_text(size = base_size, angle = 0,colour = "grey25"), 
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank(),
			plot.background = element_rect(fill="white", linetype="blank"),
			legend.title = element_text(size = base_size,colour = "grey25"),
			legend.text = element_text(size = base_size, colour = "grey25"),
			axis.title.y = element_blank(),
			axis.title.x = element_blank()
		) +
		
		scale_x_discrete() 
		
	#flip axes such that boxplots are horizontal
	if (getOmitOutliers()=="y")
		p <- p + coord_flip(ylim = c(yCappedMin,yCappedMax))
	else
		p <- p + coord_flip(ylim = c(minVal,maxVal))
	
	#draw the plot
	print(p)																
	
},height = plotHeight)