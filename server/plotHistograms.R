#plotHistograms.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#histograms ------------------------------------------------------------------------------

#plot histogram of non-aggregated data
output$histogram <- renderPlot({
	
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
	if (getSortDir() == "desc")
		subData.dt <- head(subData.dt,n=getNSpaces())
	else
		subData.dt <- tail(subData.dt,n=getNSpaces())
	
	result.dt <- result.dt[result.dt$name %in% subData.dt$name,]
	
	#absolute or relative differentials, set max/min scale bounds
	if(getDiffAbsRel()=="no")	
		result.dt[,xVal:=value]
	
	else if(getDiffAbsRel()=="rel")
	{
		#if value != 0 and baseline == 0, diff is +100%, if value == 0 and baseline != 0, diff is -100%
		result.dt[,xVal:= (value - baseline) / baseline]	
		result.dt <- result.dt[xVal < -100,xVal:=-100]
		result.dt <- result.dt[xVal > 100,xVal:=100]
		
		if(getOmitOutages()=="y") #prevent these values from skewing the scale
		{
			result.dt <- result.dt[value==0 & baseline==0,xVal:=NA]
			result.dt <- result.dt[value!=0 & baseline==0,xVal:=NA]
			result.dt <- result.dt[value==0 & baseline!=0,xVal:=NA]
		}
	}
	else
	{
		result.dt[,xVal:=value - baseline]	
		
		if(getOmitOutages()=="y") #prevent these values from skewing the scale
		{
			result.dt <- result.dt[value!=0 & baseline==0,xVal:=NA]
			result.dt <- result.dt[value==0 & baseline!=0,xVal:=NA]
		}
	}
	
	#subset to real quantities, omit divide / zero errors
	result.dt <- result.dt[xVal>-Inf]
	result.dt <- result.dt[xVal<Inf]
	
	result.dt[,maxWhiskerValue:=as.double(max(boxplot.stats(xVal)$stats[5],0)), by=list(name)]	
	result.dt[,minWhiskerValue:=as.double(min(boxplot.stats(xVal)$stats[1],0)), by=list(name)]	
	
	#if outliers omitted, limit scale to highest whisker * 1.05
	if (getOmitOutliers()=="y")
	{
		xCappedMax <- max(result.dt$maxWhiskerValue, na.rm=TRUE)*1.05
		xCappedMin <- min(result.dt$minWhiskerValue, na.rm=TRUE)*1.05
	}
	
	#reorder name by meanValue such that facets are correctly ordered
	result.dt <- result.dt[,meanValue:=mean(value,na.rm=TRUE), by=list(name)]	
	
	if(getSortDir()=="desc")
		result.dt$name <- reorder(result.dt$name,-result.dt$meanValue)
	else
		result.dt$name <- reorder(result.dt$name, result.dt$meanValue)
		
	base_size <- 12
	
	#determine largest value for range capping
	maxVal <- max(result.dt$xVal,na.rm=TRUE,0)*1.05
	minVal <- min(result.dt$xVal,na.rm=TRUE,0)
	
	p <- ggplot(result.dt) +
		
		#specify histograms / density plots
		aes(
			x = xVal,
			y = ..density..
		) +
		
		geom_hline(yintercept=c(0),colour="grey75",size=0.5) +
		
		geom_vline(xintercept=c(0),colour="grey75",size=0.5) +
		
		geom_density(alpha=0.5, linetype=0,fill="grey50") +
				
		#add plot style
		theme_bw() + 
		
		theme(
			legend.position = "top", 
			legend.key.size = unit(2,"lines"),
			panel.margin = unit(0, "lines"),
			strip.background = element_rect(fill="white", linetype="blank"),
			axis.text.x = element_text(size = base_size, angle = 0,colour = "grey25"), 
			axis.text.y = element_blank(), 
			axis.ticks.y = element_blank(), 
			strip.text.y = element_blank(),
			panel.border = element_blank(),
			legend.title = element_text(size = base_size, colour = "grey25"),
			legend.text = element_text(size = base_size,colour = "grey25"),
			axis.title.x = element_blank(),
			axis.title.y = element_blank()
		)
	
	#flip axes such that boxplots are horizontal
	if (getOmitOutliers()=="y")
		p <- p + coord_cartesian(xlim = c(xCappedMin,xCappedMax))
	else
		p <- p + coord_cartesian(xlim = c(minVal,maxVal))
	
	#facet by space
	p <- p + facet_grid(name ~ .)			
	
	#draw the plot
	print(p)																
	
},height = plotHeight)