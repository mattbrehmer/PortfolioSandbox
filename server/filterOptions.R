#filters.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#filters ------------------------------------------------------------------------------

#specify the date range (dateRangeInput)
output$date_filter <- renderUI({
	
	dateRangeInput(
		inputId = "date_filter", 
		label = "Date Range:",
		start  = "2013-01-01",
		end = "2013-12-31",
		min = "2012-01-01",
		max = "2013-12-31",
		format = "mm/dd/yy",
		separator = " - ")
})

# #specify the year (selectInput)
# output$year_filter <- renderUI({
# 	
# 	selectInput(
# 		inputId = "year_filter", 
# 		label = "Year:",
# 		choices=c("all",levels(performance.dt$Year)),
# 		selected = "all",
# 		multiple = TRUE)
# })

# #specify the week range (sliderInput)
# output$week_filter <- renderUI({
# 	
# 	sliderInput(
# 		inputId = "week_filter",
# 		label = paste("Week:"),
# 		min = 0, 
# 		max = 52, 
# 		value = c(0, 52),
# 		step = 1, 
# 		format="#")
# })

#specify the month (selectInput)
output$month_filter <- renderUI({
	
	selectInput(
		inputId = "month_filter",
		label = "Month:",
		choices=c("all",levels(performance.dt$month)),
		selected = "all",
		multiple = TRUE)
})

# #specify the day of month (selectInput)
# output$dayofmonth_filter <- renderUI({
# 	
# 	selectInput(
# 		inputId = "dayofmonth_filter",
# 		label = "Day of Month:",
# 		choices=c("all",levels(performance.dt$DayOfMonth)),
# 		selected = "all",
# 		multiple = TRUE)
# })
# 
# #specify the weekday (selectInput)
# output$weekday_filter <- renderUI({
# 	
# 	selectInput(
# 		inputId = "weekday_filter",
# 		label = "Day of Week:",
# 		choices=c("all",levels(performance.dt$weekday)),
# 		selected = "all",
# 		multiple = TRUE)
# })
# 
# #specify the timeofday (selectInput)
# output$timeofday_filter <- renderUI({
# 	
# 	selectInput(
# 		inputId = "timeofday_filter",
# 		label = "Time of Day:",
# 		choices=c("all",levels(performance.dt$TimeOfDay)),
# 		selected = "all",
# 		multiple = TRUE)
# })	

#specify the season (selectInput)
output$season_filter <- renderUI({
	
	selectInput(
		inputId = "season_filter", 
		label = "Season:",
		choices=c(
			"all",
			"winter",
			"spring",
			"summer",
			"fall"
		),
		selected = "all",
		multiple = TRUE)
})

# 	#specify the name (textInput) made redundant by coordinated map+name_filter selectInput
# 	output$name_text_input_filter <- renderUI({
# 		
# 		if(is.null(values$selectedSpace))
# 			name = "all"
# 		else
# 			name <- paste(values$selectedSpace$name)
# 		
# 		textInput(
# 			inputId = "name_text_input_filter", 
# 			label = "Search by space name:",
# 			value = name)
# 	})

#specify the name (selectInput)
output$name_filter <- renderUI({
	
	if(is.null(values$selectedSpace)) #nothing is currently selected in the map
		name = "all"
	else
		name <- paste(values$selectedSpace$name) #propagate reactive value from map selection
	
	selectInput(
		inputId = "name_filter", 
		label = "Space Name:",
		choices=c("all",levels(meta.dt$name)),
		selected = name,
		multiple = TRUE)
})

#specify the city (selectInput)
output$city_filter <- renderUI({
	
	selectInput(
		inputId = "city_filter", 
		label = "City:",
		choices=c("all",levels(meta.dt$city)),
		selected = "all",
		multiple = TRUE)
})

#specify the space type (selectInput)
output$tag_filter <- renderUI({
	
	selectInput(
		inputId = "tag_filter", 
		label = "Tag:",
		choices=c("all",levels(taglist.dt$tag)),
		selected = "all",
		multiple = TRUE)
})

#specify the space type (selectInput)
output$space_type_filter <- renderUI({
	
	selectInput(
		inputId = "space_type_filter", 
		label = "Space Type:",
		choices=c("all",levels(meta.dt$space_type)),
		selected = "all",
		multiple = TRUE)
})

#specify the use type (selectInput)
output$use_type_filter <- renderUI({
	
	selectInput(
		inputId = "use_type_filter", 
		label = "Use Type:",
		choices=c("all",levels(meta.dt$use_type)),
		selected = "all",
		multiple = TRUE)
})

#specify the space_use (selectInput)
output$space_use_filter <- renderUI({
	
	selectInput(
		inputId = "space_use_filter", 
		label = "Space Use:",
		choices=c("all",levels(meta.dt$space_use)),
		selected = "all",
		multiple = TRUE)
})

#specify the area range (sliderInput)
output$area_filter <- renderUI({
	
	if (identical(max(meta.dt$area,na.rm=TRUE),-Inf))
		maxVal <- 1000
	else
		maxVal <-	max(meta.dt$area,na.rm=TRUE)
	
	sliderInput(
		inputId = "area_filter",
		label = "",
		min = 0, 
		max = maxVal, 
		value = c(0,maxVal),
		step = 1000, 
		format="####")
})

#specify the occupant count range (sliderInput)
output$occupant_count_filter <- renderUI({
	
	if (identical(max(meta.dt$occupant_count,na.rm=TRUE),-Inf))
		maxVal <- 1000
	else
		maxVal <-	max(meta.dt$occupant_count,na.rm=TRUE)
	
	sliderInput(
		inputId = "occupant_count_filter",
		label = "",
		min = 0,
		max = maxVal, 
		value = c(0,maxVal),
		step = 100, 
		format="###")
})

#specify the weekly operating hours range (sliderInput)
output$weekly_operating_hours_filter <- renderUI({
	
	sliderInput(
		inputId = "weekly_operating_hours_filter",
		label = "",
		min = 0, 
		max = 168, 
		value = c(0,168),
		step = 10, 
		format="##")
})

#specify the occupant count range (sliderInput)
output$building_age_filter <- renderUI({
	
	sliderInput(
		inputId = "building_age_filter",
		label = "",
		min = 0, 
		max = 200, 
		value = c(0,200),
		step = 1, 
		format="##")
})