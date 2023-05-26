# bezier_path
#
# Author: Roger Beecham
###############################################################################

# Convert degrees to radians.
get_radians <- function(degrees) {
  (degrees * pi) / (180)
}

# Takes a df defining an OSGB OD pair (e.g. cartesian coordinates) and count. 
# Returns a df of coord pairs representing asymmetric curve. 
# Parametrtisation follows that published in Wood et al. 2011. doi: 10.3138/carto.46.4.239.

get_trajectory <- function(data) {
  o_east=data$o_east
  o_north=data$o_north
  d_east=data$d_east
  d_north=data$d_north 
  od_pair=data$od_pair  
  
  curve_angle=get_radians(-90)
  east=(o_east-d_east)/6
  north=(o_north-d_north)/6
  c_east=d_east + east*cos(curve_angle) - north*sin(curve_angle)
  c_north=d_north + north*cos(curve_angle) + east*sin(curve_angle)
  d <- tibble(
    x=c(o_east,c_east,d_east),
    y=c(o_north,c_north,d_north),
    od_pair=od_pair
  )
}