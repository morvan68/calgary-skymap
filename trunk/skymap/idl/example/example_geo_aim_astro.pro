;# Example use of skymap_geo_aim to determine azimuth & elevation of astronomical object.

;skymap_object__define

skymap_geographic__define           ;# make sure object type is defined
location= [100.0, 56.4, 265.4]      ;# Gillam Manitoba Canada: altitude, latitude, longitude (degrees)
site= skymap_geographic(location)   ;# create object

skymap_geo_aim__define              ;# Make sure object type is defined
gaim= skymap_geo_aim(site)          ;# create object

vega_radec= [279.23, 38.78]         ;# right ascension, declination (degrees)  <== !! check this !!
aim= DBLARR(1440,2)                 ;# az/el pair for each minute of day
FOR indx=0,1439 DO BEGIN
  gaim->set_time,[2012,01,01,indx/60,indx MOD 60,0],/ymdhms    ;# 1-minute steps
  gaim->set_aim,vega_radec,/astro,/degrees  
;  print,gaim->get_time(),gaim->get_aim(/degrees)
  aim[indx,0]= gaim->get_aim(/degrees)                         ;# azimuth, zenith angle
ENDFOR

PLOT,FINDGEN(1440)/60,aim[*,1],XTITLE='Hour',YTITLE='Zenith angle (degrees)',TITLE='Vega from Gillam on January 01, 2012'

END