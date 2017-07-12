# Load required packages
#library(readr)
#library(magrittr)
library(dplyr)
#library(lubridate)
#library(stringr)
#library(grid)
library(ggplot2)
#library(leaflet)
library(Eq)
library(testthat)

context("eq_read_data tests")
x <- system.file("extdata", "signif.txt", package = "Eq")
y <- eq_read_data(x)

test_that("dataset exists and key columns are correct type", {
expect_identical(y, y)
expect_is(y, "data.frame")
expect_is(y$LATITUDE, "character")
expect_is(y$LONGITUDE, "character")
expect_is(y$COUNTRY, "character")
expect_is(y$LOCATION_NAME, "character")
expect_is(y$TOTAL_DEATHS, "character")
expect_is(y$EQ_PRIMARY, "character")
})

context("eq_clean_data tests")
x <- system.file("extdata", "signif.txt", package = "Eq")
x <- eq_read_data(x)
y <- eq_clean_data(x)

test_that("dataset exists and key columns are correct type", {
expect_identical(y, y)
expect_is(y, "data.frame")
expect_is(y$LATITUDE, "numeric")
expect_is(y$LONGITUDE, "numeric")
expect_is(y$COUNTRY, "character")
expect_is(y$LOCATION_NAME, "character")
expect_is(y$TOTAL_DEATHS, "numeric")
expect_is(y$EQ_PRIMARY, "numeric")
expect_is(y$DATE, "Date")
})

context("eq_location_clean tests")
x <- system.file("extdata", "signif.txt", package = "Eq")
x <- x %>%
eq_read_data() %>%
eq_clean_data()
y <- eq_location_clean(x)

test_that("dataset exists and key columns are correct type", {
expect_identical(y, y)
expect_is(y, "data.frame")
expect_is(y$LATITUDE, "numeric")
expect_is(y$LONGITUDE, "numeric")
expect_is(y$COUNTRY, "character")
expect_is(y$LOCATION_NAME, "character")
expect_is(y$TOTAL_DEATHS, "numeric")
expect_is(y$EQ_PRIMARY, "numeric")
expect_is(y$DATE, "Date")
})

context("geom_timeline tests")
x <- system.file("extdata", "signif.txt", package = "Eq")
x <- x %>%
eq_read_data() %>%
eq_clean_data() %>%
eq_location_clean()
y <- ggplot(data = subset(x, COUNTRY %in% c("USA" , "CHINA"))) +
geom_timeline(aes(	x = DATE,
					xmin = as.Date("2000-01-01"),
					xmax = as.Date("2015-12-31"),
					y = COUNTRY,
					size = EQ_PRIMARY,
					fill = TOTAL_DEATHS          		
)) +
labs(size = "Richter scale value", fill = "# deaths") +
theme(legend.position="bottom")

test_that("plot exists and is correct type", {
expect_identical(y, y)
expect_is(y, "ggplot")
})

context("geom_timeline_label tests")
x <- system.file("extdata", "signif.txt", package = "Eq")
x <- x %>%
eq_read_data() %>%
eq_clean_data() %>%
eq_location_clean()
y <- ggplot(data = subset(x, COUNTRY %in% c("USA" , "CHINA"))) +
geom_timeline(aes(	x = DATE,
		     		xmin = as.Date("2000-01-01"),
		     		xmax = as.Date("2015-12-31"),
		     		y = COUNTRY,
		     		size = EQ_PRIMARY,
            		fill = TOTAL_DEATHS          		
)) + 
geom_timeline_label(aes(x = DATE,
						xmin = as.Date("2000-01-01"),
						xmax = as.Date("2015-12-31"),
						y = COUNTRY, 
						label = LOCATION_NAME, 
						size = EQ_PRIMARY), 
						n_max = 5) +
labs(size = "Richter scale value", fill = "# deaths") +
theme(legend.position="bottom")

test_that("plot exists and is correct type", {
expect_identical(y, y)
expect_is(y, "ggplot")
})

context("eq_map tests")
x <- system.file("extdata", "signif.txt", package = "Eq")
x <- x %>%
eq_read_data() %>%
eq_clean_data() %>%
eq_location_clean()
y <- x %>%
dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
eq_map(annot_col = "DATE")

test_that("plot exists and is correct type", {
expect_identical(y, y)
expect_is(y, "leaflet")
})

context("eq_create_label tests")
x <- system.file("extdata", "signif.txt", package = "Eq")
x <- x %>%
eq_read_data() %>%
eq_clean_data() %>%
eq_location_clean()
y <- x %>%
dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
dplyr::mutate(popup_text = eq_create_label(.)) %>% 
eq_map(annot_col = "popup_text")

test_that("plot exists and is correct type", {
expect_identical(y, y)
expect_is(y, "leaflet")
})
