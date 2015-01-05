#reactiveValues.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#reactiveValues such as selectedSpace, which allow propagated selection of spaces from map to name_filter
values <- reactiveValues()	
values$selectedInterval <- "season"

#selectOptions
getAggregateSelect <- reactive({
	if(is.null(input$aggregate_select))
		return("None")
	else
		return(input$aggregate_select)
})

getQuantitySelect <- reactive({
	if(is.null(input$quantity_select))
		return("Average Demand")
	else
		return(input$quantity_select)
})

getNormalizeSelect <- reactive({
	if(is.null(input$normalize_select))
		return("None")
	else
		return(input$normalize_select)
})

getWeatherNormalizeSelect <- reactive({
	if(is.null(input$weather_normalize_select))
		return("None")
	else
		return(input$weather_normalize_select)
})

getHighlightSelect <- reactive({
	if(is.null(input$highlight_select))
		return("None")
	else
		return(input$highlight_select)
})

getResourceSelect <- reactive({
	if(is.null(input$resource_select))
		return("Electricity")
	else
		return(input$resource_select)
})

getIntervalSelect <- reactive({
	
	dateRange <- as.numeric(max(getDateFilter())-min(getDateFilter()))
	
	if (input$vizTabs == "Heatmaps")
	{
		if (dateRange > 365 && getMonthFilter() == "all")
			values$selectedInterval <- "season"
		else
			values$selectedInterval <- "month"
		
		if (!is.null(input$heatmap_radio) && input$heatmap_radio == "cal")
			values$selectedInterval <- "DayOfYear"
	}
	else if (input$vizTabs == "LineUp")
	{
		if (dateRange > 365 && getSeasonFilter() == "all" && getMonthFilter() == "all")
			values$selectedInterval <- "Year"
		else if (dateRange > 180 && getMonthFilter() == "all")
			values$selectedInterval <- "season"
		else
			values$selectedInterval <- "month"
	}
	else if (input$vizTabs == "Line Plots")
	{
		if (dateRange > 180 && getMonthFilter() == "all")
			values$selectedInterval <- "month"
		else if (dateRange > 90)
			values$selectedInterval <- "WeekOfYear"
		else if (dateRange > 7)
			values$selectedInterval <- "DayOfYear"
		else
			values$selectedInterval <- "DateTimeStmp"
	}
	return(values$selectedInterval)
})

getNSpaces <- reactive({
	if(is.null(input$nSpaces))
		return(5)
	else
		return(input$nSpaces)
})

getSortDir <- reactive({
	if(is.null(input$sortDir))
		return("desc")
	else
		return(input$sortDir)
})

getOmitOutliers <- reactive({
	if(is.null(input$omitOutliers))
		return("n")
	else
		return(input$omitOutliers)
})

getDiffAbsRel <- reactive({
	if(is.null(input$diffAbsRel))
		return("abs")
	else
		return(input$diffAbsRel)
})

getOmitOutages <- reactive({
	if(is.null(input$omitOutages))
		return("n")
	else
		return(input$omitOutages)
})

#filter options
getDateFilter <- reactive({
	if(is.null(input$date_filter))
		return(as.Date("2013-01-01"):as.Date("2013-12-31"))
	else
		return(input$date_filter)
})

getYearFilter <- reactive({
	if(is.null(input$year_filter))
		return("all")
	else
		return(input$year_filter)
})

getWeekFilter <- reactive({
	return(input$week_filter)
})

getMonthFilter <- reactive({
	if(is.null(input$month_filter))
		return("all")
	else
		return(input$month_filter)
})

getSeasonFilter <- reactive({
	if(is.null(input$season_filter))
		return("all")
	else
		return(input$season_filter)
})
	
getNameFilter <- reactive({
	if(is.null(input$name_filter))
		return("all")
	else
		return(input$name_filter)
})

getSpaceTypeFilter <- reactive({
	if(is.null(input$space_type_filter))
		return("all")
	else
		return(input$space_type_filter)
})	

getUseTypeFilter <- reactive({
	if(is.null(input$use_type_filter))
		return("all")
	else
		return(input$use_type_filter)
})	

getSpaceUseFilter <- reactive({
	if(is.null(input$space_use_filter))
		return("all")
	else
		return(input$space_use_filter)
})

getCityFilter <- reactive({
	if(is.null(input$city_filter))
		return("all")
	else
		return(input$city_filter)
})

getTagFilter <- reactive({
	if(is.null(input$tag_filter))
		return("all")
	else
		return(input$tag_filter)
})

getAreaFilter <- reactive({
	return(input$area_filter)
})	

getOccupantCountFilter <- reactive({
	return(input$occupant_count_filter)
})

getWeeklyOperatingHoursFilter <- reactive({
	return(input$weekly_operating_hours_filter)
})

getBuildingAgeFilter <- reactive({
	return(input$building_age_filter)
})

getAreaCB <- reactive({
	return(input$area_cb)
})	

getOccupantCountCB <- reactive({
	return(input$occupant_count_cb)
})

getWeeklyOperatingHoursCB <- reactive({
	return(input$weekly_operating_hours_cb)
})

getBuildingAgeCB <- reactive({
	return(input$building_age_cb)
})

getDayOfWeekSelect <- reactive({
	return(input$dayOfWeekSelect)
})

getTimeOfDaySelect <- reactive({
	return(input$timeOfDaySelect)
})

#getUnits
getUnits <- reactive({
	
	unit <- "kW"
	
	if(getResourceSelect() == "Steam")
		unit <- "lb/h"
	
	switch(getWeatherNormalizeSelect(),
				 
				 "None" = unit <- paste(unit,"",sep=""),
				 
				 "HDDs" = unit <- paste(unit,"/HDD",sep=""),
				 
				 "CDDs" = unit <- paste(unit,"/CDD",sep=""),
				 
				 "None"
	)
	
	if(getQuantitySelect() == "Total Consumption")
		unit <- paste(unit,"*h",sep="")
	
	switch(getNormalizeSelect(),
				 
				 "None" = unit <- paste(unit,"",sep=""),
				 
				 "Area" = unit <- paste(unit,"/sq.m",sep=""),
				 
				 "Occupant Count" = unit <- paste(unit,"/occupant",sep=""),
				 
				 "Operating Hours" = unit <- paste(unit,"/op. hour",sep=""),
				 
				 "None"
	)
	
	return(unit)
})