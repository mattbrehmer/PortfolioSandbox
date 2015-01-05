#plotCalendars.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#Calendar ------------------------------------------------------------------------------

#plot calendar plot of query result
output$calendarPlot <- renderPlot({
			
	result.dt <- Data()
	
	#check for empty data, and don't plot 4h intervals
	if (nrow(result.dt) == 0 || is.null(result.dt$quantity))
		return()		  
	
	#combine month and year factors but retain respective within-factor orderings
	result.dt$month <- interaction(result.dt$month,result.dt$Year)
	
	result.dt<-data.table(ddply(result.dt,.(month),transform,monthweek=1+as.numeric(WeekOfYear)-min(as.numeric(WeekOfYear))))
	
	result.dt <- unique(result.dt[, list(
		space_id,
		monthweek,
		month,
		weekday,
		DayOfMonth,
		quantity,
		baselineQuantity,
		name,
		space_use,
		use_type,
		space_type,
		city,
		area,
		occupant_count,
		weekly_operating_hours,
		tag) ])	
	
	#aggregate space data based on space_use, use_type, space_type, city, or tags		
	if (getAggregateSelect() != "None"){
		
		switch(getAggregateSelect(),	
					 
					 "Space Use" = result.dt <- result.dt
					 [,
					  list(
					  	quantity=sum(quantity,na.rm=TRUE),
					  	baselineQuantity=sum(baselineQuantity,na.rm=TRUE),
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(space_use,monthweek,month,weekday,DayOfMonth)
					  ],
					 
					 "Use Type" = result.dt <- result.dt
					 [,
					  list(
					  	quantity=sum(quantity,na.rm=TRUE),
					  	baselineQuantity=sum(baselineQuantity,na.rm=TRUE),
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(use_type,monthweek,month,weekday,DayOfMonth)
					  ],
					 
					 "Space Type" = result.dt <- result.dt
					 [,
					  list(
					  	quantity=sum(quantity,na.rm=TRUE),
					  	baselineQuantity=sum(baselineQuantity,na.rm=TRUE),
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(space_type,monthweek,month,weekday,DayOfMonth)
					  ],
					 
					 "City" = result.dt <- result.dt
					 [,
					  list(
					  	quantity=sum(quantity,na.rm=TRUE),
					  	baselineQuantity=sum(baselineQuantity,na.rm=TRUE),
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(city,monthweek,month,weekday,DayOfMonth)
					  ],
					 
					 "Tags" = result.dt <- result.dt
					 [,
					  list(
					  	quantity=sum(quantity,na.rm=TRUE),
					  	baselineQuantity=sum(baselineQuantity,na.rm=TRUE),
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(tag,monthweek,month,weekday,DayOfMonth)
					  ],
					 
					 "Space Use"
		)
		setnames(result.dt,colnames(result.dt)[1],"name") #set space_use/use_type/space_type/tag/city to name
	}
	
	#normalize based on space metadata
	if (getNormalizeSelect() != "None"){
		switch(getNormalizeSelect(),	
					 
					 "Area" = result.dt$quantity <- 
					 	result.dt$quantity/result.dt$area,
					 
					 "Occupant Count" = result.dt$quantity <- 
					 	result.dt$quantity/result.dt$occupant_count,
					 
					 "Operating Hours" = result.dt$quantity <- 
					 	result.dt$quantity/result.dt$weekly_operating_hours,
					 
					 "Area"
		)
		
		switch(getNormalizeSelect(),	
					 
					 "Area" = result.dt$baselineQuantity <- 
					 	result.dt$baselineQuantity/result.dt$area,
					 
					 "Occupant Count" = result.dt$baselineQuantity <- 
					 	result.dt$baselineQuantity/data.dt$occupant_count,
					 
					 "Operating Hours" = result.dt$baselineQuantity <- 
					 	result.dt$baselineQuantity/result.dt$weekly_operating_hours,
					 
					 "Area"
		)
	}
	
	#remove NA values
	result.dt <- result.dt[!is.na(name)]
	
	#subset to real quantities, omit divide / zero errors
	result.dt <- result.dt[quantity>-Inf]
	result.dt <- result.dt[quantity<Inf]
	result.dt <- result.dt[baselineQuantity>-Inf]
	result.dt <- result.dt[baselineQuantity<Inf]
	
	#subset to top/bottom n
	subData.dt <- data.table(result.dt)
	setkey(subData.dt,name)
	subData.dt <- subData.dt[,meanQuantity:=mean(quantity,na.rm=TRUE), by=list(name)]	
	subData.dt <- subset(subData.dt, select = c(name,meanQuantity))	
	subData.dt <- unique(subData.dt[, list(name,meanQuantity) ])
	subData.dt <- arrange(subData.dt,desc(meanQuantity))	
		
	#take head or tail or table, based on user input and size specified
	if (getSortDir() == "desc")
		subData.dt <- head(subData.dt,n=getNSpaces())
	else
		subData.dt <- tail(subData.dt,n=getNSpaces())
	
	result.dt <- result.dt[result.dt$name %in% subData.dt$name,]
	
	#reorder name by meanQuantity such that facets are correctly ordered
	result.dt <- result.dt[,meanQuantity:=mean(quantity,na.rm=TRUE), by=list(name)]	
	
	if(getSortDir()=="desc")
		result.dt$name <- reorder(result.dt$name,-result.dt$meanQuantity)
	else
		result.dt$name <- reorder(result.dt$name, result.dt$meanQuantity)
	
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
	
	p <- ggplot(result.dt) +
		aes(monthweek, weekday)
	
	if (getDiffAbsRel()=="no")
		p <- p + geom_tile(aes(fill = quantity)) + 
		geom_text(aes(label=DayOfMonth,colour=quantity),size=2,fontface="plain")
	
	else
		p <- p + geom_tile(aes(fill = diffQuant)) + 
		geom_text(aes(label=DayOfMonth,colour=diffQuant),size=2,fontface="plain")
	
		#facet by month
		p <- p + facet_grid(name ~ month) +
		
		#add colour scales
		gradientScale + 
		colScale +
		
		#add plot style
		theme_bw() +	
		
		theme(
			legend.position = "left", 
			legend.key.size = unit(2,"lines"),
			legend.text = element_text(size = 12, angle = 0, hjust = 1,colour = "grey25"),
			legend.title = element_text(size = 12, colour = "grey25"),
			axis.ticks = element_blank(), 
			panel.margin = unit(0.5, "lines"),
			strip.background = element_rect(fill="white", linetype="blank"),
			axis.text.x = element_blank(), 
			panel.grid = element_blank(),
			axis.text.y = element_text(size = 10, hjust = 1,colour = "grey25"), 		
			strip.text.x = element_text(size = 12,colour = "grey25"),
			strip.text.y = element_text(size = 12,colour = "grey25",angle = 0, hjust = 0)
		) +
				
		#don't show axes labels			
		labs(x = "", y = "")
	
	print(p)															
	
},height = plotHeight)