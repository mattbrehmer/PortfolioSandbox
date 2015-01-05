#reactiveFunctions.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#Metadata() ------------------------------------------------------------------------------

Metadata <- reactive({
	
	submeta.dt <- meta.dt
	
	#add column for tags, even if portfolio has no tags
	submeta.dt$tags <- ""
	
	#generate comma-separated string of tags for display in map pop-up
	for(i in 1:nrow(submeta.dt))
		submeta.dt[i]$tags <- paste(as.character(spacetags.dt[space_id==submeta.dt[i]$space_id]$tag),collapse=", ")
	
	#join metadata with tags
	spacetags.dt <- spacetags.dt[option_match(spacetags.dt$tag, getTagFilter()),]
	
	if(nrow(spacetags.dt) > 0)
		submeta.dt <- submeta.dt[spacetags.dt,allow.cartesian=TRUE]
	else
		submeta.dt$tag <- ""
	
	#filter spaces by metadata
	submeta.dt <- submeta.dt[
		option_match(submeta.dt$name,getNameFilter()) &
			option_match(submeta.dt$space_type,getSpaceTypeFilter()) &
			option_match(submeta.dt$use_type,getUseTypeFilter()) &
			option_match(submeta.dt$space_use,getSpaceUseFilter()) &
			option_match(submeta.dt$city,getCityFilter())
		,]
	
	#filter by area
	if(getAreaCB())
		submeta.dt <- submeta.dt[in_range(submeta.dt$area, getAreaFilter()),]
	
	#filter by occupant count
	if(getOccupantCountCB())
		submeta.dt <- submeta.dt[in_range(submeta.dt$occupant_count, getOccupantCountFilter()),]
	
	#filter by weely_operating_hours
	if(getWeeklyOperatingHoursCB())
		submeta.dt <-  submeta.dt[in_range(submeta.dt$weekly_operating_hours, getWeeklyOperatingHoursFilter()),]
	
	#filter by building age
	if(getBuildingAgeCB())
		submeta.dt <-  submeta.dt[in_range(submeta.dt$building_age, getBuildingAgeFilter()),]
		
	#subset metadata to these attributes
	submeta.dt <- unique(submeta.dt[,list(
		space_id,
		name,
		latitude,
		longitude,
		space_type,
		space_use,
		use_type,
		city,			
		tags,
		area,
		occupant_count,
		building_age,
		weekly_operating_hours,
		tag
	)])
	
	return(submeta.dt)		
})

#Data() ------------------------------------------------------------------------------

Data <- reactive({
	
	data.dt <- performance.dt
	
	#filter by date
	data.dt <- performance.dt[
		in_range(as.Date(performance.dt$date), getDateFilter()) &
			in_range(performance.dt$WeekOfYear, getWeekFilter()) &
			option_match(performance.dt$Year, getYearFilter()) &
			option_match(performance.dt$month, getMonthFilter()) &	
			option_match(performance.dt$season, getSeasonFilter())
		,]
	
	#filter weekdays/weekends
	if(getDayOfWeekSelect()=="weekday")
		data.dt <- data.dt[option_match(data.dt$weekday, c("mon","tue","wed","thu","fri")),]
	
	if(getDayOfWeekSelect()=="weekend")
		data.dt <- data.dt[option_match(data.dt$weekday, c("sat","sun")),]
	
	#filter daytime/nighttime
	if(getTimeOfDaySelect()=="daytime")
		data.dt <- data.dt[option_match(data.dt$TimeOfDay, c(8:19)),]
		
	if(getTimeOfDaySelect()=="nighttime")
		data.dt <- data.dt[option_match(data.dt$TimeOfDay, c(0:7,20:23)),]
	
	#normalize based on weather, substitute constant 0.001 when HDD/CDD = 0, to rule out div/0 errors
	if (getWeatherNormalizeSelect() != "None")
	{
		switch(getWeatherNormalizeSelect(),	
					 
					 "HDDs" = data.dt$value <- 
					 	data.dt$value/max(0.001,data.dt$HDD),
					 
					 "CDDs" = data.dt$value <- 
					 	data.dt$value/max(0.001,data.dt$CDD),
					 
					 "HDDs"					 
		)
		switch(getWeatherNormalizeSelect(),	
					 
					 "HDDs" = data.dt$baseline <- 
					 	data.dt$baseline/max(0.001,data.dt$baselineHDD),
					 
					 "CDDs" = data.dt$baseline <- 
					 	data.dt$baseline/max(0.001,data.dt$baselineCDD),
					 
					 "HDDs"					 
		)
	}
	
	#subset to faceting interval
	switch(getIntervalSelect(),		
				 
				 "Year" = data.dt[,interval:=Year],					 
				 "month" =  data.dt[,interval:=month],					 
				 "season" =  data.dt[,interval:=season],					 
				 "WeekOfYear" = data.dt[,interval:=WeekOfYear],					 
				 "DayOfYear" = data.dt[,interval:=DayOfYear],					 
				 "DayOfMonth" = data.dt[,interval:=DayOfMonth],					 
				 "weekday" =  data.dt[,interval:=weekday],					 
				 "TimeOfDay" =  data.dt[,interval:=TimeOfDay],					 
				 "DateTimeStmp" =  data.dt[,interval:=DateTimeStmp],					 
				 "Year"					 
	)	
	
	data.dt <- data.dt[resource==paste(getResourceSelect())]
	
	setkey(data.dt,space_id)
	
	#compute quantity of interest		
	switch(getQuantitySelect(),		
				 
				 "Average Demand" = data.dt
				 [,
				  quantity:=mean(value,na.rm=TRUE),
				  by=list(space_id,resource,Year,interval)
				  ],			
				 
				 "Min Demand" = data.dt
				 [,
				  quantity:=min(value,na.rm=TRUE),
				  by=list(space_id,resource,Year,interval)
				  ],
				 
				 "Max Demand" =	data.dt
				 [,
				  quantity:=max(value,na.rm=TRUE),
				  by=list(space_id,resource,Year,interval)
				  ],
				
				 
				 "Total Consumption" = data.dt
				 [,
				  quantity:=sum(value*4L,na.rm=TRUE),
				  by=list(space_id,resource,Year,interval)
				  ],
				 "Average Demand"					 
	)
	
	#compute baseline of interest		
	switch(getQuantitySelect(),		
				 
				 "Average Demand" = data.dt
				 [,
				  baselineQuantity:=mean(baseline,na.rm=TRUE),
				  by=list(space_id,resource,Year,interval)
				  ],			
				 
				 "Min Demand" = data.dt
				 [,
				  baselineQuantity:=min(baseline,na.rm=TRUE),
				  by=list(space_id,resource,Year,interval)
				  ],
				 
				 "Max Demand" =	data.dt
				 [,
				  baselineQuantity:=max(baseline,na.rm=TRUE),
				  by=list(space_id,resource,Year,interval)
				  ],				 
				 
				 "Total Consumption" = data.dt
				 [,
				  baselineQuantity:=sum(baseline*4L,na.rm=TRUE),
				  by=list(space_id,resource,Year,interval)
				  ],
				 "Average Demand"					 
	)
	
	#join with metadata
	submeta.dt <- Metadata()
	data.dt <- data.dt[submeta.dt,allow.cartesian=TRUE]
	setkey(data.dt,space_id)
	
	return(data.dt)
	
})

#Query() ------------------------------------------------------------------------------

#query result aggregates and normalizes, used by all charts and tables except box plots
Query <- reactive({
	
	#subset the data with fitlers, merge with metadata and tags
	data.dt <- Data()
	
	if (nrow(data.dt) == 0 || is.null(data.dt$quantity))
		return()		
	
	#subset to unique data points (one per space per interval)
	data.dt <- unique(
	  data.dt, by = c(
	    "space_id",
	    "name",
	    "area",
	    "space_use",
	    "space_type",
	    "use_type",
	    "latitude",
	    "longitude",
	    "city",
	    "tag",
	    "occupant_count",
	    "weekly_operating_hours",
	    "resource",
	    "interval",
	    "Year",
	    "quantity",
	    "baselineQuantity"
	  ) 	  
	)	
	
	#aggregate space data based on space_use, use_type, space_type, city, or tags		
	if (getAggregateSelect() != "None"){
		
		switch(getAggregateSelect(),	
					 
					 "Space Use" = data.dt <- data.dt
					 [,
					  list(
					  	quantity=sum(quantity,na.rm=TRUE),
					  	baselineQuantity=sum(baselineQuantity,na.rm=TRUE),
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(space_use,resource,Year,interval)
					  ],
					 
					 "Use Type" = data.dt <- data.dt
					 [,
					  list(
					  	quantity=sum(quantity,na.rm=TRUE),
					  	baselineQuantity=sum(baselineQuantity,na.rm=TRUE),
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(use_type,resource,Year,interval)
					  ],
					 
					 "Space Type" = data.dt <- data.dt
					 [,
					  list(
					  	quantity=sum(quantity,na.rm=TRUE),
					  	baselineQuantity=sum(baselineQuantity,na.rm=TRUE),
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(space_type,resource,Year,interval)
					  ],
					 
					 "City" = data.dt <- data.dt
					 [,
					  list(
					  	quantity=sum(quantity,na.rm=TRUE),
					  	baselineQuantity=sum(baselineQuantity,na.rm=TRUE),
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(city,resource,Year,interval)
					  ],
					 
					 "Tags" = data.dt <- data.dt
					 [,
					  list(
					  	quantity=sum(quantity,na.rm=TRUE),
					  	baselineQuantity=sum(baselineQuantity,na.rm=TRUE),
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(tag,resource,Year,interval)
					  ],
					 
					 "Space Use"
		)
		setnames(data.dt,colnames(data.dt)[1],"name") #set space_use/use_type/space_type/tag/city to name
	}
	
	#normalize based on space metadata
	if (getNormalizeSelect() != "None"){
		switch(getNormalizeSelect(),	
					 
					 "Area" = data.dt$quantity <- 
					 	data.dt$quantity/data.dt$area,
					 
					 "Occupant Count" = data.dt$quantity <- 
					 	data.dt$quantity/data.dt$occupant_count,
					 
					 "Operating Hours" = data.dt$quantity <- 
					 	data.dt$quantity/data.dt$weekly_operating_hours,
					 
					 "Area"					 
		)
		
		switch(getNormalizeSelect(),	
					 
					 "Area" = data.dt$baselineQuantity <- 
					 	data.dt$baselineQuantity/data.dt$area,
					 
					 "Occupant Count" = data.dt$baselineQuantity <- 
					 	data.dt$baselineQuantity/data.dt$occupant_count,
					 
					 "Operating Hours" = data.dt$baselineQuantity <- 
					 	data.dt$baselineQuantity/data.dt$weekly_operating_hours,
					 
					 "Area"
		)
	}
	
	#remove NA values
	data.dt <- data.dt[!is.na(name)]
	data.dt <- data.dt[!is.na(Year)]
	data.dt <- data.dt[!is.na(resource)]
	
	#subset to real quantities, omit divide / zero errors
	data.dt <- data.dt[quantity>-Inf]
	data.dt <- data.dt[quantity<Inf]
	data.dt <- data.dt[baselineQuantity>-Inf]
	data.dt <- data.dt[baselineQuantity<Inf]
	
	return(data.dt)
	
})

#RawQuery() ------------------------------------------------------------------------------

#returns raw disaggregated data (query used for drawing boxplots and histograms)
RawQuery <- reactive({
	
	data.dt <- Data()
	
	if (nrow(data.dt) == 0 || is.null(data.dt$quantity))
		return()		
	
	if(getQuantitySelect() == "Total Consumption")
		data.dt$value <- data.dt$value*4L
	
	#compute aggregate space performance
	if (getAggregateSelect() != "None"){
		
		switch(getAggregateSelect(),	
					 
					 "Space Use" = aggData.dt <- data.dt
					 [,
					  list(
					  	value=sum(value,na.rm=TRUE),
					  	baseline=sum(baseline,na.rm=TRUE)
					  ), 
					  by=list(space_use,resource,Time,Year,interval)
					  ],
					 
					 "Use Type" = aggData.dt <- data.dt
					 [,
					  list(
					  	value=sum(value,na.rm=TRUE),
					  	baseline=sum(baseline,na.rm=TRUE)
					  ), 
					  by=list(use_type,resource,Time,Year,interval)
					  ],
					 
					 "Space Type" = aggData.dt <- data.dt
					 [,
					  list(
					  	value=sum(value,na.rm=TRUE),
					  	baseline=sum(baseline,na.rm=TRUE)
					  ), 
					  by=list(space_type,resource,Time,Year,interval)
					  ],
					 
					 "City" = aggData.dt <- data.dt
					 [,
					  list(
					  	value=sum(value,na.rm=TRUE),
					  	baseline=sum(baseline,na.rm=TRUE)
					  ), 
					  by=list(city,Time,resource,Year,interval)
					  ],
					 
					 "Tags" = aggData.dt <- data.dt
					 [,
					  list(
					  	value=sum(value,na.rm=TRUE),
					  	baseline=sum(baseline,na.rm=TRUE)
					  ), 
					  by=list(tag,Time,resource,Year,interval)
					  ],
					 
					 "Space Use"
		)
		
		#subset to unique metadata for computing aggregate space metadata
		data.dt <- unique(
			data.dt[, 
							list(
								name,
								area,
								space_use,
								space_type,
								use_type,
								city,
								tag,
								latitude,
								longitude,
								occupant_count,
								weekly_operating_hours
							) 
							]
		)
		
		#compute aggregate space metadata: total area, occupant count, weekly operating hours	
		switch(getAggregateSelect(),	
					 
					 "Space Use" = data.dt <- data.dt
					 [,
					  list(
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(space_use)
					  ],
					 
					 "Use Type" = data.dt <- data.dt
					 [,
					  list(
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(use_type)
					  ],
					 
					 "Space Type" = data.dt <- data.dt
					 [,
					  list(
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(space_type)
					  ],
					 
					 "City" = data.dt <- data.dt
					 [,
					  list(
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(city)
					  ],
					 
					 "Tags" = data.dt <- data.dt
					 [,
					  list(
					  	area=sum(area,na.rm=TRUE),
					  	occupant_count=sum(occupant_count,na.rm=TRUE),
					  	weekly_operating_hours=mean(weekly_operating_hours,na.rm=TRUE)
					  ), 
					  by=list(tag)
					  ],
					 
					 "Space Use"
		)
		
		#join aggregated data with aggregate space data	
		setnames(aggData.dt,colnames(aggData.dt)[1],"name")
		setkey(aggData.dt,name)
		setnames(data.dt,colnames(data.dt)[1],"name")
		setkey(data.dt,name)
		
		data.dt <- data.dt[aggData.dt]			
	}
	
	#normalize daily data
	if (getNormalizeSelect() != "None"){
		
		switch(getNormalizeSelect(),	
					 
					 "Area" = data.dt$value <- 
					 	data.dt$value/data.dt$area,
					 
					 "Occupant Count" = data.dt$value <- 
					 	data.dt$value/data.dt$occupant_count,
					 
					 "Operating Hours" = data.dt$value <- 
					 	data.dt$value/data.dt$weekly_operating_hours,
					 
					 "Area"
		)
		
		switch(getNormalizeSelect(),	
					 
					 "Area" = data.dt$baseline <- 
					 	data.dt$baseline/data.dt$area,
					 
					 "Occupant Count" = data.dt$baseline <- 
					 	data.dt$baseline/data.dt$occupant_count,
					 
					 "Operating Hours" = data.dt$baseline <- 
					 	data.dt$baseline/data.dt$weekly_operating_hours,
					 
					 "Area"
		)
		
		#remove NA values
		data.dt <- data.dt[!is.na(name)]
		data.dt <- data.dt[!is.na(Year)]
		data.dt <- data.dt[!is.na(resource)]
		data.dt <- data.dt[!is.na(interval)]
		
		data.dt <- data.dt[value>-Inf]
		data.dt <- data.dt[value<Inf]
		data.dt <- data.dt[baseline>-Inf]
		data.dt <- data.dt[baseline<Inf]
	}
	
	return(data.dt)		
})

#query result normalizeS but doesn't aggregate, used by the map
MapQuery <- reactive({
	
	#subset the data with fitlers, merge with metadata and tags
	data.dt <- Data()
	
	if (nrow(data.dt) == 0 || is.null(data.dt$quantity))
		return()
	
	#subset to unique data points (one per space per interval)
	data.dt <- unique(
		data.dt[, 
						list(
							space_id,
							name,
							area,
							space_use,
							space_type,
							use_type,
							latitude,
							longitude,
							city,
							tags, #the string list of tags, not each tag individually
							occupant_count,
							weekly_operating_hours,
							building_age,
							Year,
							resource,
							interval,
							quantity
						) 
						]
	)
	
	#compute quantity of interest		
	switch(getQuantitySelect(),		
				 
				 "Average Demand" = data.dt
				 [,
				  overallQuantity:=mean(quantity,na.rm=TRUE), 
				  by=list(space_id)
				  ],
				 
				 "Min Demand" = data.dt
				 [,
				  overallQuantity:=min(quantity,na.rm=TRUE), 
				  by=list(space_id)
				  ],
				 
				 "Max Demand" =	data.dt
				 [,
				  overallQuantity:=max(quantity,na.rm=TRUE), 
				  by=list(space_id)
				  ],
				 
				 "Total Consumption" = data.dt
				 [,
				  overallQuantity:=sum(quantity,na.rm=TRUE), 
				  by=list(space_id)
				  ],
				 "Average Demand"					 
	)
	
	#normalize based on space metadata
	if (getNormalizeSelect() != "None"){
		switch(getNormalizeSelect(),	
					 
					 "Area" = data.dt$overallQuantity <- 
					 	data.dt$overallQuantity/data.dt$area,
					 
					 "Occupant Count" = data.dt$overallQuantity <- 
					 	data.dt$overallQuantity/data.dt$occupant_count,
					 
					 "Operating Hours" = data.dt$overallQuantity <- 
					 	data.dt$overallQuantity/data.dt$weekly_operating_hours,
					 
					 "Area"
		)
	}
	
	#remove NA values
	data.dt <- data.dt[!is.na(name)]
	data.dt <- data.dt[!is.na(Year)]
	data.dt <- data.dt[!is.na(resource)]
	
	#subset to real quantities, omit divide / zero errors
	if (!is.null(data.dt$overallQuantity))
	{
		data.dt <- data.dt[overallQuantity>-Inf]
		data.dt <- data.dt[overallQuantity<Inf]
		data.dt$fillColour <- '#FFFFFF'
	}
	
	#throw out duplicate rows
	data.dt <- unique(subset(data.dt, select=-c(quantity,interval,Year,resource)))
	
	#discretize gradient scale into 6 bins based on evenly spaced breaks
	maxQuant <- max(data.dt$overallQuantity,na.rm=TRUE,0)
	
	if (maxQuant > 0)
	{	
		for (i in 1:nrow(data.dt))
		{	
			if(data.dt[i]$overallQuantity > 0.8 * maxQuant) 
				data.dt[i]$fillColour <- '#832424' #80-100% of max
			
			else if(data.dt[i]$overallQuantity > 0.6 * maxQuant) 
				data.dt[i]$fillColour <- '#A0504A' #60-80% of max
			
			else if(data.dt[i]$overallQuantity > 0.4 * maxQuant) 
				data.dt[i]$fillColour <- '#BB7A73' #40-60% of max
			
			else if(data.dt[i]$overallQuantity > 0.2 * maxQuant) 
				data.dt[i]$fillColour <- '#D3A59F' #20-40% of max
			
			else if(data.dt[i]$overallQuantity > 0) 
				data.dt[i]$fillColour <- '#EAD1CE' #0-20% of max
			
			else
				data.dt[i]$fillColour <- '#FFFFFF'
		}	
	}

		#discretize gradient scale into 7 bins based on boxplot stats
# 	quantDist <- boxplot.stats(data.dt$overallQuantity)$stats
# 	
# 	for (i in 1:nrow(data.dt))
# 	{	
# 		if(data.dt[i]$overallQuantity > quantDist[5]) 
# 			data.dt[i]$fillColour <- '#832424' #outlier greater than max
# 		
# 		else if(data.dt[i]$overallQuantity > quantDist[4]) 
# 			data.dt[i]$fillColour <- '#9B4943' #greater than 75th percentile
# 		
# 		else if(data.dt[i]$overallQuantity > quantDist[3]) 
# 			data.dt[i]$fillColour <- '#B26C65' #greater than median
# 		
# 		else if(data.dt[i]$overallQuantity > quantDist[2]) 
# 			data.dt[i]$fillColour <- '#C79089' #greater than 25th percentile
# 		
# 		else if(data.dt[i]$overallQuantity > quantDist[1]) 
# 			data.dt[i]$fillColour <- '#DBB4AF' #greater than min
# 		
# 		else if(data.dt[i]$overallQuantity < quantDist[1]) 
# 			data.dt[i]$fillColour <- '#EDD9D6' #outlier less than min
# 		
# 		else
# 			data.dt[i]$fillColour <- '#FFFFFF'
# 	}	
	
	return(data.dt)
	
})