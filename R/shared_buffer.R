shared_buffer <- function(x, group, dist = units::set_units(1, km), density = units::set_units(4, 1/km)) {
  x_union <- x %>% 
    summarise()
  
  x_seas <- x_union %>% st_buffer(
    dist = dist
  ) %>% st_cast()
  
  x_donut <- x_seas %>% 
    st_difference(x_union) %>% 
    st_cast()
  
  x_points <- x_union %>% 
    st_cast("MULTILINESTRING") %>% 
    st_cast("LINESTRING") %>% 
    st_line_sample(density = density) %>% 
    st_sf()
  
  x_voronoi <- x_points %>% 
    st_combine() %>% 
    st_voronoi() %>% 
    st_cast()
  
  x_voronoi_donut <- st_make_valid(x_voronoi) %>% 
    st_intersection(st_make_valid(x_donut)) %>% 
    st_cast() %>% 
    st_sf()
  
  x_vd_region_distinct <- x_voronoi_donut %>% 
    mutate(id_v = 1:n()) %>% 
    st_join(x,
            join = st_is_within_distance, dist=units::set_units(5, m)) %>% 
    distinct(id_v, .keep_all = TRUE)
  
  x_seas_result <- x_vd_region_distinct %>% 
    group_by_at(group) %>% 
    summarise()
  
  return(x_seas_result)
}
