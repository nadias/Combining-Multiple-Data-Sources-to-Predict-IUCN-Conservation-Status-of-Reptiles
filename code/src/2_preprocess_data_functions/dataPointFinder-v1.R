
library(raster)
library(sf)
library(units)


#' Data finder for missing point data
#'
#' Finds data for missing points (i.e., that do not intersect raster layers)
#' using an incremental buffer approach and polygon-raster extraction.
#' By default the average function is used to aggregate found values for all
#' intersected cells.
#'
#' @param pts A sf object containing points
#'
#' @param rst A RasterStack object from raster package
#'
#' @param dMax Maximum distance for the buffer (in meters; default: 2500m)
#'
#' @param dIncrement Buffer increment distance (in meters; default: 500m)
#'
#' @param verbose Print progress messages? (default: TRUE)
#'
#' @param baseCRS Base Coordinate Reference System SRID code
#' (default: WGS 84 GCS, EPSG code: 4326)
#'
#' @param projCRS Base Projected Coordinate Reference System SRID code
#' (default: WGS 84 World Mercator, EPSG code: 3395). This is used to convert
#' points from a lat/lon geographic CRS into a planar one.
#'
#' @return A matrix object with a number of columns equal to nlayers(rst) plus two.
#' The first column ("PointID") indicates the point sequential ID in the original point
#' dataset (i.e. pts object). The second column indicates the minimum distance
#' at which data was found (if possible). The remaining columns are for extracted
#' data from the rst RasterStack object averaged across all found raster cells
#'
#'

dataPointFinder <- function(pts, rst, dMax = 2500, dIncrement = 500, verbose = TRUE,
                            baseCRS = 4326, projCRS = 3395){

  if(dMax == dIncrement){
    warning("dMax and dIncrement are equal! The data search will run only once.
            Is that really what you want?")
  }
  if(dIncrement > dMax){
    stop("dIncrement cannot be higher than dMax distance! Check inputs")
  }

  # For clarity define units of values (meters)
  units(dMax) <- "m"
  units(dIncrement) <- "m"

  # Make results matrix
  resMatrix <- matrix(NA,nrow = nrow(pts), ncol = nlayers(rst)+2,
                      dimnames = list(1:nrow(pts),
                                      c("PointID","Dist_m",names(rst))))

  # EPSG:3395
  #proj4str <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "

  if(verbose) pb <- txtProgressBar(1, nrow(pts), style = 3)

  ### LOOP THROUGH POINTS ###
  ### ------------------- ###
  for(i in 1:nrow(pts)){
    print(paste0("Row: ", i))

    # Convert the active point to WGS 1984 / World Mercator planar system in order
    # to use the st_buffer function which can't use geographic lat/lon coordinates
    # Check https://epsg.io/3395
    pta <- st_transform(pts[i,], crs = projCRS)

    # Test first the dMax distance value!! Why?:
    # If dMax does not work it is useless to use incremental search and
    # hence potentially saving time
    # Before extraction, the point is converted back to baseCRS (default: WGS 1984 GCS) system!!!
    ptb <- st_transform(st_buffer(pta, dist = dMax), crs = baseCRS)

    # By default, if multiple values are found in the search found these will
    # be averaged and NA's removed
    # extDF will be a vector with one value per layer in the raster stack
    extDF <- extract(rst, ptb, method="simple", fun=mean, na.rm=TRUE)[1,]


    if(any(is.na(extDF))){ # Data not found at dMax

      resMatrix[i, 1] <- i

      if(verbose) cat(crayon::yellow("\n-> POINT ID:",i,"| DATA NOT FOUND AT DMAX DISTANCE!\n\n"))
      next

    }else{ # Data found at dMax - now check at smaller incremental distances

      d <- dIncrement
      resMatrix[i, 1] <- i

      # Loop incrementally till data is found on land
      # OR till dMax is reached.. whatever comes first :-)
      while(d <= dMax){

        # Make buffer and convert back to baseCRS
        ptb <- st_transform(st_buffer(pta, dist = d), crs = baseCRS)

        # Extract data for the polygon buffer around the point
        extDF <- extract(rst, ptb, method = "simple",
                         fun = mean, na.rm = TRUE)[1,]

        if(any(is.na(extDF))){ # not found yet
          d <- d + dIncrement
          next
        }else{ # data found
          # Write average data by proximity
          resMatrix[i,1:2] <- c(i,d)
          resMatrix[i,3:ncol(resMatrix)] <- extDF

          if(verbose) cat(crayon::green("\n-> POINT ID:",i,"| DATA FOUND AT MIN DISTANCE =",d,"meters .......\n\n"))

          break
        }
      }# end while loop
    }
    if(verbose) setTxtProgressBar(pb,i)

    saveRDS(as.data.frame(resMatrix), paste0("data/processed/impute_NA/row_",i,".rds"))
  }

  return(resMatrix)
}

#' Find length of a one-degree arc given latitude
#'
#' An helpful function to calculate the size of a 1 degree arc
#' given the latitude of an input point
#'
#' @param x A sf point object
#'
#' @return Numeric value

getArcLenByLatitude <- function(x){
  lat <- st_coordinates(x)[1,"Y"]
  pt1 <- st_sfc((st_point(x=c(0, lat))), crs = 4326)
  pt2 <- st_sfc((st_point(x=c(1, lat))), crs = 4326)
  return(as.numeric(st_distance(x = pt1, pt2)))
}

