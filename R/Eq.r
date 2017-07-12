#library(readr)
#library(magrittr)
#library(dplyr)
#library(lubridate)
#library(stringr)
#library(grid)
#library(ggplot2)
#library(leaflet)
#library(devtools)
#library(roxygen2)





#' Read earthquake data text file 
#'
#' This function (eq_read_data) reads the text file with earthquake data and creates a data frame.
#' The function assumes that the text file is stored in the working directory.
#' The file is read using the read_delim function from the readr package (\code{\link[readr]{read_delim}}).
#'
#' @param filename A character string giving the path, name, and extension of a file to read in. Only the name and extension needs to be provided if the file is in the working directory. 
#' 
#' @return This function returns a data frame table containing data for earthquakes.
#'
#' @examples
#' \dontrun{
#' eq_data <- eq_read_data("signif.txt")
#' eq_data <- eq_read_data(filename= "signif.txt")
#' }
#'
#' @export
#' @importFrom readr read_delim
eq_read_data <- function(filename) {

readr::read_delim(filename, "\t")

}




#' Tidy and subset earthquake data
#'
#' This function (eq_clean_data) removes unnecessary columns using the select_ function from the dplyr package (\code{\link[dplyr]{select_}}).
#' The columns that are kept in the data are: YEAR, MONTH, DAY, LATITUDE, LONGITUDE, LOCATION_NAME, TOTAL_DEATHS, COUNTRY, EQ_PRIMARY.
#' Earthquakes with negative year values are excluded from the data using the filter_ function from the dplyr package (\code{\link[dplyr]{filter_}}).
#' The mutate_ function from the dplyr package (\code{\link[dplyr]{mutate_}}) is used to create a date column and ensure analysis columns are numeric.
#' 
#' @param data Name of input data frame on which to apply function.
#'
#' @return This function returns a data frame table containing a cleansed subset of the earthquake data.
#'
#' @examples
#' \dontrun{
#' eq_clean <- eq_clean_data(eq_data)
#' eq_clean <- eq_clean_data(data= eq_data)
#' }
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select_ mutate_ filter_
eq_clean_data <- function(data) {

data %>%
dplyr::select_(.dots = c('YEAR', 'MONTH', 'DAY', 'LATITUDE', 'LONGITUDE', 'LOCATION_NAME', 'TOTAL_DEATHS', 'COUNTRY', 'EQ_PRIMARY')) %>%
dplyr::filter_(~ YEAR > 0) %>%
dplyr::mutate_(MONTH = ~ ifelse(is.na(MONTH), 01, MONTH), DAY = ~ ifelse(is.na(DAY), 01, DAY), DATE = ~ as.Date(paste(DAY, MONTH, YEAR, sep= "/"), "%d/%m/%Y"),
		  LATITUDE = ~ as.numeric(LATITUDE), LONGITUDE = ~ as.numeric(LONGITUDE), TOTAL_DEATHS = ~ as.numeric(TOTAL_DEATHS),
		  EQ_PRIMARY = ~ as.numeric(EQ_PRIMARY))

}




#' Tidy location names field in earthquake data
#'
#' This function (eq_location_clean) removes country names at the beginning of the LOCATION_NAME field.
#' It is assumed that country names are followed by a colon when a subsequent city name is provided.
#' Where only the country name is given in the LOCATION_NAME field, the country name is removed since this information is in the country field.
#' The removal of country names is performed using the mutate_ function from the dplyr package (\code{\link[dplyr]{mutate_}}).
#' The LOCATION_NAME field is converted to title case using the str_to_title function from the stringr package (\code{\link[stringr]{str_to_title}}).
#' 
#' @param data Name of input data frame on which to apply function.
#'
#' @return This function returns a data frame table where the LOCATION_NAME field in the earthquake data is cleansed.
#'
#' @examples
#' \dontrun{
#' eq_clean_loc <- eq_location_clean(eq_clean)
#' eq_clean_loc <- eq_location_clean(data= eq_clean)
#' }
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_
#' @importFrom stringr str_to_title
eq_location_clean <- function(data) {

# From the beginning of the string (first ^) replace 0 or more characters (*) up to first colon if it exists ([^:]) with nothing ("")
# Then from the beginning of the string (first ^) replace 0 or more characters (*) up to first upper or lower case letter ([^a-z|^A-Z]) with nothing ("")
data %>%
dplyr::mutate_(LOCATION_NAME = ~ sub("^[^:]*", "", LOCATION_NAME)) %>%
dplyr::mutate_(LOCATION_NAME = ~ sub("^[^a-z|^A-Z]*", "", LOCATION_NAME)) %>%
dplyr::mutate_(LOCATION_NAME = ~ sub("[^;\n]+[:]", "", LOCATION_NAME)) %>%
dplyr::mutate_(LOCATION_NAME = ~ sub("^[^a-z|^A-Z]*", "", LOCATION_NAME)) %>%
dplyr::mutate_(LOCATION_NAME = ~ stringr::str_to_title(LOCATION_NAME))

}




#' Function for plotting timeline of earthquake data
#'
#' This function (geom_timeline) subsets the data to values between xmin and xmax using the filter_ function from the dplyr package (\code{\link[dplyr]{filter_}}).
#' Uses the layer function from the ggplot2 package (\code{\link[ggplot2]{layer}}) to call the geomtimeline geom and use it on the data. 
#'  
#' @param mapping Set of aesthetic mappings created by aes or aes_; 
#' 	If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot;
#' 	You must supply mapping if there is no plot mapping; 
#' 	Required aes: Date field - x, lowest date to include - xmin, highest date to include - xmax; 
#' 	Default aes: Group variable where separate timelines should be created for each unique value - y; 
#' 		variable where values distinguished by point outline colour - colour; 
#' 		variable where values distinguished by point fill colour - fill, variable where values distinguished by point size - size;
#' 		transparency of points - alpha, shape of points - shape, width of border to points - stroke.
#' @param data The data to be displayed in this layer. There are three options: 
#' 	If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#' 	A data.frame, or other object, will override the plot data; All objects will be fortified to produce a data frame; 
#' 	See fortify for which variables will be created; A function will be called with a single argument, the plot data; 
#' 	The return value must be a data.frame, and will be used as the layer data.
#' @param ... Other arguments passed on to layer; These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3; 
#' 	They may also be parameters to the paired geom/stat.
#' @param stat Statistical transformation to use on the data. 
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function. 
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed. 
#' @param show.legend Logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped; 
#' 	FALSE never includes, and TRUE always includes. 
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them; 
#' 	This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders. 
#'
#' @return A timeline showing eathquake sizes and intensities between specified dates.
#' 
#' @examples
#' \dontrun{
#' eq_clean_loc_tl <- ggplot(data = eq_clean_loc) +
#' geom_timeline(aes(x = DATE,
#' 		     xmin = as.Date("2000-01-01"),
#'		     xmax = as.Date("2015-12-31"),
#'		     size = EQ_PRIMARY,
#'            	     fill = TOTAL_DEATHS          		
#' )) +
#' labs(size = "Richter scale value", fill = "# deaths") +
#' theme(legend.position="bottom") 
#'
#' eq_clean_loc_tl <- ggplot(data = eq_clean_loc) +
#' geom_timeline(aes(x = DATE,
#'		     xmin = as.Date("2000-01-01"),
#'		     xmax = as.Date("2015-12-31"),
#'		     y = COUNTRY,
#'		     size = EQ_PRIMARY,
#'           	     fill = TOTAL_DEATHS          		
#' )) +
#' labs(size = "Richter scale value", fill = "# deaths") +
#' theme(legend.position="bottom") 
#'
#' eq_clean_loc_tl <- ggplot(data = subset(eq_clean_loc, COUNTRY %in% c("USA" , "CHINA"))) +
#' geom_timeline(aes(x = DATE,
#'		     xmin = as.Date("2000-01-01"),
#'		     xmax = as.Date("2015-12-31"),
#'		     y = COUNTRY,
#'		     size = EQ_PRIMARY,
#'           	     fill = TOTAL_DEATHS          		
#' )) +
#' labs(size = "Richter scale value", fill = "# deaths") +
#' theme(legend.position="bottom")
#' }
#'
#' @export
#' @importFrom ggplot2 layer
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {

     ggplot2::layer(
          geom = geomtimeline, mapping = mapping,
          data = data, stat = stat, position = position,
          show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
     )
}




#' Geom for plotting timeline of earthquake data
#'
#' This geom (geomtimeline) subsets the data to values between xmin and xmax using the filter_ function from the dplyr package (\code{\link[dplyr]{filter_}}).
#' The line elements of the graph are created using the polylineGrob function from the grid package (\code{\link[grid]{polylineGrob}}).
#' The point elements of the graph are created using the pointsGrob function from the grid package (\code{\link[grid]{pointsGrob}}).
#' The line and point elements are plotted together using the gList function from the grid package (\code{\link[grid]{gList}}).
#' Returns a timeline showing eathquake sizes and intensities between specified dates.
#' Required_aes A character vector of aesthetics needed to render the geom; Date field - x, lowest date to include - xmin, highest date to include - xmax.
#' Default_aes A list generated by aes() of default values for aesthetics; Group variable where separate timelines should be created for each unique value - y. 
#' Variable where values distinguished by point outline colour - colour. 
#' Variable where values distinguished by point fill colour - fill, variable where values distinguished by point size - size.
#' Transparency of points - alpha, shape of points - shape, width of border to points - stroke.
#' Setup_data Called once for each layer, after setp_params(); Returns modified data between specified dates.
#' Draw_key Renders a single legend key.
#' Draw_group Function to transform the data to appropriate graphing coordinate system.
#' Not a user facing element of package.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_
#' @importFrom grid pointsGrob polylineGrob gpar gList
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
geomtimeline <- ggplot2::ggproto("geomtimeline", ggplot2::Geom, 
                         	 required_aes = c("x", "xmin", "xmax"),
          			 default_aes = ggplot2::aes(y = 0.1, colour = "grey", fill = "grey", size = 0.1, alpha = 0.4, shape = 21, stroke = 1),
				 setup_data = function(data, params) {
                    
						data <- data %>%
                         			dplyr::filter_(~ x >= xmin & x <= xmax)
                    
          			 },
                         draw_key = ggplot2::draw_key_point, 
                         draw_group = function(data, panel_scales, coord) {

               coords <- coord$transform(data, panel_scales)

	       # Shape 21 is filled circle
	       # Alpha is transparency
	       # Use stroke to modify width of border to  points

               points <- grid::pointsGrob(x = coords$x, 
							y = coords$y,
                    					pch = coords$shape, 
							gp = grid::gpar(col = coords$colour, fill = coords$fill, alpha = coords$alpha, stroke = coords$stroke),
							size = unit(coords$size / 3, "char")
               )

               y_lines <- unique(coords$y)

               lines <- grid::polylineGrob(	x = unit(rep(c(0, 1), each = length(y_lines)), "npc"),
                    				 	y = unit(c(y_lines, y_lines), "npc"),
                    				 	id = rep(seq_along(y_lines), 2),
                    				 	gp = grid::gpar(col = "grey")
               )

               grid::gList(lines, points)
                                   
}                               
)





#' Function for plotting timeline labels for earthquake data timelines
#'
#' This function (geom_timeline_label) subsets the data to values between xmin and xmax using the filter_ function from the dplyr package (\code{\link[dplyr]{filter_}}).
#' Uses the layer function from the ggplot2 package to call the geomtimeline geom and use it on the data. 
#'  
#' @param mapping Set of aesthetic mappings created by aes or aes_; 
#' 	If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot;
#' 	You must supply mapping if there is no plot mapping; 
#' 	Required aes: Date field - x, lowest date to include - xmin, highest date to include - xmax, variable holding labels - label; 
#' 	Default aes: Group variable where separate timelines should be created for each unique value - y; 
#' 		variable where values distinguished by point size - size.
#' @param data The data to be displayed in this layer. There are three options: 
#' 	If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#' 	A data.frame, or other object, will override the plot data; All objects will be fortified to produce a data frame; 
#' 	See fortify for which variables will be created; A function will be called with a single argument, the plot data; 
#' 	The return value must be a data.frame, and will be used as the layer data.
#' @param ... Other arguments passed on to layer; These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3; 
#' 	They may also be parameters to the paired geom/stat.
#' @param stat Statistical transformation to use on the data. 
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function. 
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed. 
#' @param show.legend Logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped; 
#' 	FALSE never includes, and TRUE always includes. 
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them; 
#' 	This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders. 
#' @param n_max If specified it gives the maximum number of labels to show for each timeline displayed.
#'
#' @return A timeline showing eathquake sizes and intensities between specified dates.
#'
#' @examples
#' \dontrun{
#' eq_clean_loc_tl <- ggplot(data = eq_clean_loc) +
#' geom_timeline(aes(x = DATE,
#'		     xmin = as.Date("2000-01-01"),
#'		     xmax = as.Date("2015-12-31"),
#'		     size = EQ_PRIMARY,
#'           	     fill = TOTAL_DEATHS          		
#' )) + 
#' geom_timeline_label(aes(x = DATE,
#'			   xmin = as.Date("2000-01-01"),
#'			   xmax = as.Date("2015-12-31"), 
#'			   label = LOCATION_NAME, 
#'			   size = EQ_PRIMARY), 
#'			   n_max = 5) +
#' labs(size = "Richter scale value", fill = "# deaths") +
#' theme(legend.position="bottom")
#' 
#' eq_clean_loc_tl <- ggplot(data = eq_clean_loc) +
#' geom_timeline(aes(x = DATE,
#'		     xmin = as.Date("2000-01-01"),
#'		     xmax = as.Date("2015-12-31"),
#'		     y = COUNTRY,
#'		     size = EQ_PRIMARY,
#'         	     fill = TOTAL_DEATHS          		
#' )) + 
#' geom_timeline_label(aes(x = DATE,
#'			   xmin = as.Date("2000-01-01"),
#'			   xmax = as.Date("2015-12-31"),
#'			   y = COUNTRY, 
#'			   label = LOCATION_NAME, 
#'			   size = EQ_PRIMARY)) +
#' labs(size = "Richter scale value", fill = "# deaths") +
#' theme(legend.position="bottom")
#'
#' eq_clean_loc_tl <- ggplot(data = subset(eq_clean_loc, COUNTRY %in% c("USA" , "CHINA"))) +
#' geom_timeline(aes(x = DATE,
#'		     xmin = as.Date("2000-01-01"),
#'		     xmax = as.Date("2015-12-31"),
#'		     y = COUNTRY,
#'		     size = EQ_PRIMARY,
#'           	     fill = TOTAL_DEATHS          		
#' )) + 
#' geom_timeline_label(aes(x = DATE,
#'			   xmin = as.Date("2000-01-01"),
#'			   xmax = as.Date("2015-12-31"),
#'			   y = COUNTRY, 
#'			   label = LOCATION_NAME, 
#'			   size = EQ_PRIMARY), 
#'			   n_max = 5) +
#' labs(size = "Richter scale value", fill = "# deaths") +
#' theme(legend.position="bottom")
#' } 
#'
#' @export
#' @importFrom ggplot2 layer
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., na.rm = FALSE,
                                n_max = NULL, show.legend = NA,
                                inherit.aes = TRUE) {

      ggplot2::layer(
      geom = geomtimelinelabel, mapping = mapping,
      data = data, stat = stat, position = position,
      show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, n_max = n_max, ...)
     )
}




#' Geom for plotting timeline labels for earthquake data timelines
#'
#' This geom (geomtimelinelabel) subsets the data to values between xmin and xmax using the filter_ function from the dplyr package (\code{\link[dplyr]{filter_}}).
#' If applicable the data is grouped by the y variable using the group_by_ function from the dplyr package (\code{\link[dplyr]{group_by_}}).  
#' The top n_max values of the size variable are kept for each group (if applicable) using the top_n function from the dplyr package (\code{\link[dplyr]{top_n}}).
#' The data is ungrouped (if applicable) using the ungroup function from the dplyr package (\code{\link[dplyr]{ungroup}}).
#' The line elements of the graph are created using the polylineGrob function from the grid package (\code{\link[grid]{polylineGrob}}).
#' The text elements of the graph are created using the textGrob function from the grid package (\code{\link[grid]{textGrob}}).
#' The line and text elements are plotted together using the gList function from the grid package (\code{\link[grid]{gList}}).
#' Returns a timeline label showing information regarding eathquake sizes, intensities, and locations between specified dates.
#' Required_aes A character vector of aesthetics needed to render the geom; Date field - x, lowest date to include - xmin. 
#' Highest date to include - xmax, variable holding labels - label.
#' Default_aes A list generated by aes() of default values for aesthetics; Group variable where separate timelines should be created for each unique value - y.  
#' Variable where values distinguished by point size - size.
#' Setup_data Called once for each layer, after setp_params(); Returns modified data between specified dates.
#' Draw_key Renders a single legend key.
#' Draw_panel Function to transform the data to appropriate graphing coordinate system.
#' Not a user facing element of package.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter_ group_by_ top_n ungroup
#' @importFrom grid polylineGrob textGrob gpar gList
#' @importFrom ggplot2 ggproto Geom aes draw_key_blank
geomtimelinelabel <- ggplot2::ggproto("geomtimelinelabel", ggplot2::Geom,
          				required_aes = c("x", "xmin", "xmax", "label"),
					default_aes = ggplot2::aes(y = 0.1, size = 0.1),
          				draw_key = ggplot2::draw_key_blank,
          				setup_data = function(data, params) {
                    
						data <- data %>%
						dplyr::filter_(~ x >= xmin & x <= xmax)
						

						if (!is.null(params$n_max)){
							if ("y" %in% colnames(data)) {
								data <- data %>%	
                         				dplyr::group_by_(~ y) %>%
                         				dplyr::top_n(params$n_max, size) %>%
                         				dplyr::ungroup()
							} else {
								data <- data %>%
                         				dplyr::top_n(params$n_max, size)
							}
						}
						data
               
          				},
          				draw_panel = function(data, panel_scales, coord, n_max) {

               				coords <- coord$transform(data, panel_scales)
               				n_groups <- length(unique(data$y))
               				offset <- 0.2 / n_groups

               				lines <- grid::polylineGrob(	x = unit(c(coords$x, coords$x), "npc"),
                    								y = unit(c(coords$y, coords$y + offset), "npc"),
                    								id = rep(1:dim(coords)[1], 2),
                    								gp = grid::gpar(col = "grey")
               									)
												
               				text <- grid::textGrob(
                    			label = coords$label,
                    			x = unit(coords$x, "npc"),
                    			y = unit(coords$y + offset, "npc"),
                    			just = c("left", "bottom"),
                    			rot = 45
               				)

               				grid::gList(lines, text)
}
)




#' Create an interactive map of earthquake data with labels
#'
#' This function (eq_map) creates an interactive map of earthquake data using the leaflet function from the leaflet package (\code{\link[leaflet]{leaflet}}).
#' 
#' @param data Name of input data frame on which to apply function.
#' @param annot_col Name of field containing label information.
#'
#' @return This function returns an interactive map of the earthquake data.
#'
#' @examples
#' \dontrun{
#' 
#' eq_map_tl <- eq_read_data("signif.txt") %>% 
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
#' eq_map(annot_col = "DATE")
#' 
#' readr::read_delim("signif.txt", delim = "\t") %>% 
#' eq_clean_data() %>% 
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
#' eq_map(annot_col = "DATE")
#' }
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers
eq_map <- function(data, annot_col) {
  
	leaflet::leaflet() %>%
    	leaflet::addProviderTiles("Esri.WorldStreetMap") %>%
    	leaflet::addCircleMarkers(	data = data,
      					radius = data$EQ_PRIMARY ,
					lat = ~ LATITUDE,
      					lng = ~ LONGITUDE,
					fillOpacity = 0.4,
      					popup  = data[[annot_col]])
}




#' Create an interactive map of earthquake data with labels
#'
#' This function (eq_create_label) creates and applies formatting to the labels for the points on the interactive earthquake map.
#' 
#' @param data Name of input data frame on which to apply function.
#'
#' @return This function creates labels with formatting for the earthquake data.
#'
#' @examples
#' \dontrun{
#' 
#' eq_map_tl <- eq_read_data("signif.txt") %>% 
#' eq_clean_data() %>%
#' eq_location_clean() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
#' dplyr::mutate(popup_text = eq_create_label(.)) %>% 
#' eq_map(annot_col = "popup_text")
#' 
#' readr::read_delim("signif.txt", delim = "\t") %>% 
#' eq_clean_data() %>% 
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
#' dplyr::mutate(popup_text = eq_create_label(.)) %>% 
#' eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data) {
     location <- ifelse(data$LOCATION_NAME == "", "", paste("<b>Location:</b>", data$LOCATION_NAME, "<br />"))
     magnitude <- ifelse(is.na(data$EQ_PRIMARY), "", paste("<b>Magnitude:</b>", data$EQ_PRIMARY, "<br />"))
     deaths <- ifelse(is.na(data$TOTAL_DEATHS), "", paste("<b>Total deaths:</b>", data$TOTAL_DEATHS, "<br />"))
     out <- paste(location, magnitude, deaths)
     return(out)
}



