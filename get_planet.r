#R get planet

get_planet <- function(bbox,date_in){
    #read in dates
    #read in Shapefile
    
    #convert dates to correct format
    #convert shaepfile to geojson and EPSG:4326 Projection
    
    
    geo_json_geometry = list(
      type=unbox("Polygon"),
      coordinates = list(list(
        c(bbox@xmin,
          bbox@ymin),
        c(bbox@xmin,
          bbox@ymax),
        c(bbox@xmax,
          bbox@ymax),
        c(bbox@xmax,
          bbox@ymin),
        c(bbox@xmin,
          bbox@ymin)
        
        # c(-122.52227783203125,
        #   40.660847697284815),
        # c(-122.52227783203125,
        #   40.987154933797335),
        # c(-122.01690673828124,
        #   40.987154933797335),
        # c(-122.01690673828124,
        #   40.660847697284815),
        # c(-122.52227783203125,
        #   40.660847697284815)
      ))
    )
    
    
    # filter for items the overlap with our chosen geometry
    geometry_filter = list(
      type= unbox("GeometryFilter"),
      field_name= unbox("geometry"),
      config= geo_json_geometry
    )
    
    date_current = date_in
    date_1month = date_in - months(1)
    dategte = paste0(date_1month,"T00:00:00.000Z")
    datelte = paste0(date_current,"T00:00:00.000Z")
    
    # filter images acquired in a certain date range
    date_range_filter = list(
      type= unbox("DateRangeFilter"),
      field_name= unbox("acquired"),
      config= list(
        gte= unbox(dategte),
        lte= unbox(datelte))
      )
    
    # filter any images which are more than 10% clouds
    cloud_cover_filter = list(
      type= unbox("RangeFilter"),
      field_name= unbox("cloud_cover"),
      config = list(
        lte= unbox(0.05))
    )

    # coverage_filter = list(
    #   type= unbox("RangeFilter"),
    #   field_name= unbox("usable_data"),
    #   config = list(
    #     gte= unbox(0.3))
    # )
    
    # create a filter that combines our geo and date filters
    # could also use an "OrFilter"
    filter_configs = list(
      type= unbox("AndFilter"),
      config = list(date_range_filter, cloud_cover_filter,geometry_filter)
    )
    
    search_endpoint_request = list(
      item_types = "PSOrthoTile",
      filter = filter_configs
    )
    
    body_json <- toJSON(search_endpoint_request,pretty=TRUE)
    
    #r body 
    url <- 'https://api.planet.com/data/v1/quick-search'
    body <- body_json
    api_key = "27d323a68bd24f97852dfd0416128a35"
    
    
    r <- POST(url, body = body, content_type_json(), authenticate(api_key, ""))
    
    response = content(r)
    response = response$features[[1]]$id
   # browser()
    
    return(response)
    
  
}


