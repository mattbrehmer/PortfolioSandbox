#plotPortfolioMap.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#leaflet map ------------------------------------------------------------------------------

#create the map object
map <- createLeafletMap(session, 'map')

#display spaces as circles on the map, refresh with upon filtering
observe({
	
	map$clearShapes()
	
	result.dt <- data.frame(MapQuery(), stringsAsFactors = FALSE)
	
	if (nrow(result.dt) == 0 || is.null(result.dt$overallQuantity))
		return()		
	
	#solution attribution: https://groups.google.com/forum/#!topic/shiny-discuss/1pguNdowN6E
	
	# get data into a nested list with the dlply function. 
	paramList <- dlply(result.dt, 2, function(x) {
		list(x[['latitude']], 
				 x[['longitude']], 
				 max(1000000 / 2^input$map_zoom,50), 
				 x[['space_id']], 
				 list(weight = 1, # stroke weight
				 		 fill = T, # fill object
				 		 color = '#F00', # stroke color, red
				 		 opacity = 1, # stroke opacity
				 		 fillColor = x[['fillColour']], # fill color
				 		 fillOpacity = 1, # fill opacity
				 		 clickable = T)
		)})
	
	# call lapply with custom wrapper function to map$addCircle. 
	# the names of the list have to be stripped before being sent to map$addCircle
	lapply(paramList, function(x) do.call(map$addCircle, unname(x)))
	
})

#unselect any selected space by clicking elsewhere on the map
bindEvent(input$map_click, function() 
{
	values$selectedSpace <- NULL
})

#show popup with space details on mouseover
bindEvent(input$map_shape_mouseover, function() {
	
	event <- input$map_shape_mouseover
	
	result.dt <- MapQuery()
	
	if (nrow(result.dt) == 0 || is.null(result.dt$overallQuantity))
		return()
	
	space <- result.dt[result.dt$space_id == event$id,]
		
	content <- as.character(tagList(
		tags$strong(paste(space$name)),
# 		tags$br(),
# 		sprintf("Space type: %s", paste(space$space_type)),
# 		tags$br(),
# 		sprintf("Use type: %s", paste(space$use_type)),
		tags$br(),
		sprintf("Space use: %s", paste(space$space_use)),
		tags$br(),
		sprintf("City: %s (%s,%s)", paste(space$city), paste(round(space$latitude,digits=6)), paste(round(space$longitude,digits=6))),
		tags$br(),
		sprintf("Area: %s sq.m", paste(round(space$area))),
# 		tags$br(),
# 		sprintf("Building age: %s years", paste(space$building_age)),
		tags$br(),
		tags$br(),
		sprintf("Tags: %s", paste(space$tags)),
		tags$br(),
		tags$br(),
		sprintf("%s %s: %s %s", 
						paste(getResourceSelect()), 
						paste(getQuantitySelect()), 
						paste(signif(space$overallQuantity,digits=3)),
						paste(getUnits()))
	))
	
	map$showPopup(event$lat, event$lng, content, event$id)
})

#remove popup on mouseout
bindEvent(input$map_shape_mouseout, function() {
	
	event <- input$map_shape_mouseout
	map$clearPopups()
})

#if a space (represented as a shape) is clicked on the map, select that space
bindEvent(input$map_shape_click, function() {
	
	event <- input$map_shape_click
	
	submeta.dt <- Metadata()
	space <- submeta.dt[submeta.dt$space_id == event$id,]
	values$selectedSpace <- space
})