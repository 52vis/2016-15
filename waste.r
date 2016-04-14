library(albersusa) # devtools::install_github("hrbrmstr/hrbrmisc")
library(rgeos)
library(maptools)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(jsonlite)
library(tidyr)
library(dplyr)
library(purrr)

#' WSJ Waste Lands: http://projects.wsj.com/waste-lands/
#' Data from: http://www.lm.doe.gov/default.aspx?id=2602 &
#'            http://www.cdc.gov/niosh/ocas/ocasawe.html

sites <- fromJSON("sites.json", flatten=TRUE)

#' need to replace the 0-length data.frames with at least one row of `NA`s
#' so we can safely `unnest()` them later

sites$locations <- map(sites$locations, function(x) {
  if (nrow(x) == 0) {
    data_frame(latitude=NA, longitude=NA, postal_code=NA, name=NA, street_address=NA)
  } else {
    x
  }
})

#' we'll need this later

sites$site.rating <- factor(sites$site.rating,
                           levels=c(3:0),
                           labels=c("Remote or no potential for radioactive contamination, based on criteria at the time of FUSRAP review.",
                                    "Referred to another agency or program, no authority to clean up under FUSRAP, or status unclear.",
                                    "Cleanup declared complete under FUSRAP.",
                                    "Cleanup in progress under the Formerly Utilized Sites Remedial Action Program (FUSRAP)."))

#' One teensy discrepancy:

nrow(sites)

#' ## [1] 517
#'
#' The stacked bars total on the WSJ site is 515.
#' Further complication is that sites$locations is a list column with nested
#' data.frames:

sites <- unnest(sites)

nrow(sites)

#' ## [1] 549

sum(complete.cases(sites[,c("longitude", "latitude")]))

#' ## [1] 352
#'
#' So, just mapping long/lat is going to miss part of the story. But, I'm just
#' providing a kick-start for folks, so I'll just map long/lat :-)

glimpse(sites)

#' ## Observations: 549
#' ## Variables: 11
#' ## $ site.city      (chr) "Flint", "Albuquerque", "Buffalo", "Los...
#' ## $ site.name      (chr) "AC Spark Plug, Dort Highway Plant", "A...
#' ## $ site.rating    (fctr) Remote or no potential for radioactive...
#' ## $ site.state     (chr) "MI", "NM", "NY", "NM", "PA", "NY", "OH...
#' ## $ site.state_ap  (chr) "Mich.", "N.M.", "N.Y.", "N.M.", "Pa.",...
#' ## $ site.slug      (chr) "1-ac-spark-plug-dort-highway-plant", "...
#' ## $ latitude       (dbl) 43.02938, NA, NA, 35.88883, 39.95295, 4...
#' ## $ longitude      (dbl) -83.65525, NA, NA, -106.30502, -75.5927...
#' ## $ postal_code    (chr) "48506", NA, NA, "87544", "19382", "100...
#' ## $ name           (chr) "", NA, NA, "", "", "", "Former Buildin...
#' ## $ street_address (chr) "1300 North Dort Highway", NA, NA, "Pue...

#' Note that `site.slug` can be used with this URL:
#' `http://projects.wsj.com/waste-lands/site/SITE.SLUG.HERE/` to get to
#' detail pages on the WSJ site.

#' I want to use my `albersusa` mutated U.S. shapefile for this (NOTE: I'm moving
#' `albersus` into one of the rOpenSci pacakges soon vs publishing it standalone to CRAN)
#' so I need to mutate the Alaska points (there are no Hawaii points).
#' This step is *not necessary* unless you plan on displaying points on this
#' mutated map. I also realized I need to provide a mutated projection translation
#' function for AK & HI for the mutated Albers mapss.

tmp  <- data.frame(dplyr::select(filter(sites, site.state=="AK"), longitude, latitude))
coordinates(tmp) <- ~longitude+latitude
proj4string(tmp) <- CRS(us_longlat_proj)
tmp <- spTransform(tmp, CRS(us_laea_proj))
tmp <- elide(tmp, rotate=-50)
tmp <- elide(tmp, scale=max(apply(bbox(tmp), 1, diff)) / 2.3)
tmp <- elide(tmp, shift=c(-2100000, -2500000))
proj4string(tmp) <- CRS(us_laea_proj)
tmp <- spTransform(tmp, us_longlat_proj)
tmp <- as.data.frame(tmp)

sites[sites$site.state=="AK",]$longitude <- tmp$x
sites[sites$site.state=="AK",]$latitude <- tmp$y

#' and now we plot the sites

us_map <- fortify(usa_composite(), region="name")

gg <- ggplot()
gg <- gg + geom_map(data=us_map, map=us_map,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.15, fill="#e5e3df")
gg <- gg + geom_point(dat=sites, aes(x=longitude, y=latitude, fill=site.rating),
                      shape=21, color="white", stroke=1, alpha=1, size=3)
gg <- gg + scale_fill_manual(name="", values=c("#00a0b0", "#edc951", "#6a4a3c", "#eb6841"))
gg <- gg + coord_proj(us_laea_proj)
gg <- gg + guides(fill=guide_legend(override.aes=list(alpha=1, stroke=0.2, color="#2b2b2b", size=4)))
gg <- gg + theme_map()
gg <- gg + theme(legend.position="bottom")
gg <- gg + theme(legend.direction="vertical")
gg <- gg + theme(legend.key=element_blank())
gg

