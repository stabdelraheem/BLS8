

# interpolate_at <- seq(0, 1, length.out = 30)
# trips_averaged <- tracking_f %>% 
#   group_by(TOPPID) %>% 
#   mutate(trip_prop = seq(0, 1, length.out = n())) %>% 
#   ungroup() %>% 
#   group_by(Sex, Age, TOPPID) %>% 
#   summarize(lon = list(approx(trip_prop, lon, xout = interpolate_at)$y),
#             lat = list(approx(trip_prop, lat, xout = interpolate_at)$y),
#             trip_prop = list(interpolate_at),
#             .groups = "drop") %>%
#   group_by(Sex, Age, trip_prop) %>% 
#   summarize(lon = pmap(lon))