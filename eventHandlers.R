#eventHandlers.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#bindEvent ------------------------------------------------------------------------------------------
#' bindEvent
#' @author Matt Brehmer
#' @export
#' @description functions that defines interactive behaviour for the leaflet map. 
#' copied from shiny-leaflet example: https://github.com/jcheng5/leaflet-shiny/blob/master/inst/example/server.R
#' @param eventExpr
#' @param callback
#' @param env
#' @param quoted
bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) 
{
	eventFunc <- exprToFunction(eventExpr, env, quoted)
	
	initialized <- FALSE
	invisible(observe({
		eventVal <- eventFunc()
		if (!initialized)
			initialized <<- TRUE
		else
			isolate(callback())
	}))
}