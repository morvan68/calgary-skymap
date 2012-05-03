 ;# skymap.pro
 ;#
 ;# 2006-10-20 (c) Brian Jackel
 ;# 2007-01-07 Bjj -force spherical degrees into (0,360]
 ;# 2007-01-31 Bjj -add SKYMAP_IMG_MAP
 ;#
 ;# OBSOLETE: use SKYMAP_DEFINE

; Depends on
;
;  geodetic_convert     -note coordinate convention: [north,east,down] (not [east, north, zenith])
;  geospace_convert     -note array order 3xN (not Nx3) ??add /TRANSPOSE keyword??
;  yalebrightstar_readfile
;  imager_info_readfile(?)
;  site_info_readfile(?)
;
; Assembled from
;
; gmap_tool.pro
; asi_map_tools.pro

; TO DO:
;  -rename FRAME to IMG?
;

; Common block variables are named with trailing dollar signs, eg.
;
;  COMMON EXAMPLE,scalar$,vector$,array$
;

;#===========================================================================
;# Basic utility routines
;#
;# Angular conventions
;#  1) theta: counter-clockwise from "x" [0, 2pi]
;#     phi: latitude [-pi/2, pi/2]
;#     -used by GEO, GEI, MAG
;#
;#  2) alpha: azimuth clockwise from "y" (eg. north) [0, 2pi]
;#     zeta: co-latitude, zenith angle [0, pi]
;#     -used by AIM, LENS
;#
;#  3) alpha: azimuth clockwise from "-y" (eg. top) [0, 2pi]
;#     zeta: co-latitude, zenith angle [0, pi]
;#     -used by CCD, IMG

FUNCTION SKYMAP_EULER_MATRIX,yaw,pitch,tilt,DEGREES=degrees
  COMPILE_OPT HIDDEN  ;# utility routine

  IF KEYWORD_SET(DEGREES) THEN deg2rad= !dpi/180.0d0 ELSE deg2rad=1.0d0

  tmp= yaw * deg2rad  &  cos_yaw= COS(tmp)  &  sin_yaw= SIN(tmp)
  _pitch= pitch*deg2rad
  _tilt= tilt*deg2rad
  yaw_matrix= [ [cos_yaw,sin_yaw,0], [-sin_yaw,cos_yaw,0], [0,0,1.0d0] ]
  pitch_matrix= [ [COS(_pitch),0,-SIN(_pitch)], [0,1.0d0,0], [SIN(_pitch),0,COS(_pitch)] ]
  tilt_matrix= [ [1.0d0,0,0], [0,COS(_tilt),SIN(_tilt)], [0,-SIN(_tilt),COS(_tilt)] ]
  result= tilt_matrix##(pitch_matrix##yaw_matrix)
RETURN,result
END;-----------------------------------------------------------------


FUNCTION SKYMAP_HMS2DEG,hours,minutes,seconds
  COMPILE_OPT HIDDEN  ;# utility routine
  np= N_PARAMS()
  IF (np EQ 0) THEN hours=0
  IF (np LE 1) THEN minutes=0
  IF (np LE 2) THEN seconds=0.0
  result= (seconds/60.0d0 + minutes)/60.0d0 + hours
  result= result*(360.0d0/24.0d0)
RETURN,result
END;-----------------------------------------------------------------


FUNCTION SKYMAP_DMS2DEG,degrees,minutes,seconds
  COMPILE_OPT HIDDEN  ;# utility routine
  np= N_PARAMS()
  IF (np LE 2) THEN seconds=0.0
  IF (np LE 1) THEN minutes=0
  IF (np EQ 0) THEN degrees=0
  result= (seconds/60.0d0 + minutes)/60.0d0 + degrees
RETURN,result
END;-----------------------------------------------------------------


PRO SKYMAP_SPHERICAL2CARTESIAN,vector3,ALPHA_ZETA=alpha_zeta,DEGREES=degrees
  COMPILE_OPT HIDDEN  ;# utility routine

  nvec= N_ELEMENTS(vector3)/3  ; should complain if nvec MOD 3 NE 0
  vector3= REFORM(vector3,nvec,3) ; fltarr(3) -> fltarr(1,3)

  IF KEYWORD_SET(DEGREES) THEN BEGIN
    deg2rad= !dpi/180.0d0
    angle0= vector3[*,0] * deg2rad
    angle1= vector3[*,1] * deg2rad
  ENDIF ELSE BEGIN
    angle0= vector3[*,0]
    angle1= vector3[*,1]
  ENDELSE
  r= vector3[*,2]
  ;r= r + 1.0d0*(ABS(r) LE 1.0d-6) ;require minimum length?

  IF KEYWORD_SET(ALPHA_ZETA) THEN BEGIN
    vector3[*,2]= r * COS(angle0)  &  angle0= r * SIN(angle0)
    vector3[*,1]= angle0 * COS(angle1)
    vector3[*,0]= angle0 * SIN(angle1)
  ENDIF ELSE BEGIN
    vector3[*,2]= r * SIN(angle0)  &  angle0= r * COS(angle0)
    vector3[*,1]= angle0 * SIN(angle1)
    vector3[*,0]= angle0 * COS(angle1)
  ENDELSE

  RETURN
END;-----------------------------------------------------------------


PRO SKYMAP_CARTESIAN2SPHERICAL,vector3,ALPHA_ZETA=alpha_zeta,DEGREES=degrees
  COMPILE_OPT HIDDEN  ;# utility routine

  nvec= N_ELEMENTS(vector3)/3
  vector3= REFORM(vector3,nvec,3)
  vec= vector3  ;working copy

  vector3[*,2]= SQRT(TOTAL(vec^2,2))  ;r = vector length

  IF KEYWORD_SET(ALPHA_ZETA) THEN BEGIN
    vector3[*,0]= ATAN(SQRT(vec[*,0]^2+vec[*,1]^2),vec[*,2]) ;zeta
    vector3[*,1]= ATAN(vec[*,0],vec[*,1]) ;alpha
  ENDIF ELSE BEGIN
    vector3[*,0]= ATAN(vec[*,2],SQRT(vec[*,0]^2+vec[*,1]^2)) ;theta
    vector3[*,1]= ATAN(vec[*,1],vec[*,0]) ;phi
  ENDELSE

  IF KEYWORD_SET(DEGREES) THEN BEGIN
    rad2deg= 180.0d0/!dpi
    vector3[*,0]= vector3[*,0] * rad2deg
    vector3[*,1]= (vector3[*,1] * rad2deg + 360.0d0) MOD 360.0d0
  ENDIF

RETURN
END;-----------------------------------------------------------------


FUNCTION SKYMAP_TEST_CARTESIAN2SPHERICAL,nvectors
  COMPILE_OPT HIDDEN  ;# utility routine

  vec0= DOUBLE(RANDOMN(seed,nvectors,3))  &  nfail=0
  FOR type=0,1 DO FOR units=0,1 DO BEGIN
    vec1= vec0
    SKYMAP_CARTESIAN2SPHERICAL,vec1,ALPHA_ZETA=type,DEGREES=units
    SKYMAP_SPHERICAL2CARTESIAN,vec1,ALPHA_ZETA=type,DEGREES=units
    dot= TOTAL(vec0*vec1,2)/SQRT(TOTAL(vec0^2,2)*TOTAL(vec1^2,2))
    w= WHERE( ABS(ACOS(dot<1.0d0)) GE 1.0d-7,nw)
    IF (nw GT 0) THEN BEGIN
      PRINT,nw,type,units,FORMAT='(I," errors for ALPHA_ZETA=",I,", DEGREES=",I)'
      nfail= nfail+nw
    ENDIF
  ENDFOR

RETURN,nfail
END;-----------------------------------------------------------------



;#===========================================================================
;#
;# LENS (optical) parameters
;#
;# Central assumption: optics are azimuthally invariant around a central axis.
;#
;# This allows a simple model where rays enter with
;#   axial angle zeta= [0,pi/2]
;#   polar angle alpha= [0,2pi]
;# are transformed by the optical chain and exit with
;#   axial angle zeta'= f(zeta)
;#   polar angle alpha'= alpha
;#
;# A general "xyflip" matrix allows for any combination of optical
;# mirroring or ccd readout reversals:
;#
;#  [xflip      0]
;#  [0      yflip]
;#
;# where both xflip and yflip default to +1 (no reversal).
;#
;# The optics are oriented with respect to the local geodetic
;# coordinate system using a a sequence of three rotations.
;#
;#  1) start with the optical axis ..

PRO SKYMAP_IMAGER__DEFINE
  COMMON SKYMAP_IMAGER,imager$,aim2lens$,zangle$,rpixel$
  imager$= {SKYMAP_IMAGER  $
          ,uid:''         $  ;# eg. themis02, rainbow-01, taqqiila
          ,unix_time:0L $    ;#
          ,optical_orientation:DBLARR(3)    $
          ,optical_projection:DBLARR(9,2)   $
          ,optical_reflection:0b	$
          ,ccd_size:INTARR(2)		$
          ,ccd_center:DBLARR(2)		$
          ,pixel_aspect_ratio:1.0d0	$
          ,img_flip:BYTARR(2)		}

   ;# rotation matrix from optics to local geodetic
  aim2lens$= DBLARR(3,3)

   ;# lookup tables for optical projection
  zangle$= DBLARR(183)
  rpixel$= DBLARR(183)
RETURN
END;-----------------------------------------------------------------



PRO SKYMAP_IMAGER_LOAD,skymap_imager

  SKYMAP_IMAGER__DEFINE
  COMMON SKYMAP_IMAGER
  imager$= skymap_imager

  yaw= imager$.optical_orientation[0]
  pitch= imager$.optical_orientation[1]
  tilt= imager$.optical_orientation[2]
  aim2lens$= SKYMAP_EULER_MATRIX(yaw, pitch, tilt, /DEGREES)

  zangle$=  !dpi/2.0d0 * DINDGEN(183)/180.0d0  ;# 0 to 91 degrees in radians
  c= imager$.optical_projection & nc= N_ELEMENTS(c[*,0])
  c[0,1]= 1.0d0  ;by definition

  numer= 0.0d0  &  denom= 0.0d0
  FOR indx=nc-1,0,-1 DO numer= ABS(zangle$)*numer + c[indx,0]
  FOR indx=nc-1,0,-1 DO denom= ABS(zangle$)*denom + c[indx,1]
  rpixel$= numer/denom

RETURN
END;-----------------------------------------------------------------




;#===========================================================================
;#
;# The CCD detector is a rectangle of dimensions Lx, Ly
;# containing a grid of Nx, Ny pixels.
;#
;# CCD coordinates:
;#   x  0..nx-1 (left to right ie. columns)
;#   y  0..ny-1 (top to bottom ie. rows)
;#   z  perpendicular to CCD (!! not right-handed !!)
;#
;#  optical axis on CCD at xc,yc
;#
;#  pixel aspect ratio = dx / dy = (Lx/Nx) / (Ly/Ny)
;#
;#  azimuth angle= atan(x,-y) clockwise from -y
;#  zenith angle
;#
;#  See IMG below for image offset and binning.

FUNCTION SKYMAP_CCD2LENS,vector2,SPHERICAL=spherical,DEGREES=degrees

  COMMON SKYMAP_IMAGER

  nvec= N_ELEMENTS(vector2)/2  &  result= DBLARR(nvec,3)

  bad= (vector2[*,0] LT 0.0) OR (vector2[*,0] GE imager$.ccd_size[0]) OR $
       (vector2[*,1] LT 0.0) OR (vector2[*,1] GE imager$.ccd_size[1]) OR $
       (FINITE(vector2[*,0]) NE 1)

  ;# shift origin to optical center and correct for rectangular pixels
  xccd=  vector2[*,0] - imager$.ccd_center[0]
  yccd= (vector2[*,1] - imager$.ccd_center[1])/ imager$.pixel_aspect_ratio

  rpixel= SQRT(xccd^2 + yccd^2)
  result[0,0]= INTERPOL(zangle$,rpixel$,rpixel,/QUADRATIC) ;# zenith angle "zeta"

  aangle= ATAN(xccd,-yccd) & IF (imager$.optical_reflection) THEN aangle= aangle - !dpi
  result[0,1]= aangle

  result[*,2]= 1.0d0 ;# dummy value for radius

  SKYMAP_SPHERICAL2CARTESIAN,result,/ALPHA_ZETA

  where_bad= WHERE(bad OR (rpixel GT MAX(rpixel$)), nbad)
  IF (nbad GT 0) THEN result[where_bad,*]= !values.d_nan

  IF KEYWORD_SET(SPHERICAL) THEN SKYMAP_CARTESIAN2SPHERICAL,result,/ALPHA_ZETA,DEGREES=degrees

RETURN,result
END;----------------------------------------------------------------


FUNCTION SKYMAP_LENS2CCD,vector3,SPHERICAL=spherical,DEGREES=degrees
  COMMON SKYMAP_IMAGER

  vec= vector3
  IF KEYWORD_SET(SPHERICAL) THEN SKYMAP_SPHERICAL2CARTESIAN,vec,/ALPHA_ZETA,DEGREES=degrees
  SKYMAP_CARTESIAN2SPHERICAL,vec,/ALPHA_ZETA

  zangle= REFORM(vec[*,0])
  rpixel= INTERPOL(rpixel$,zangle$,zangle,/QUADRATIC) ;# radians to pixels
  aangle= REFORM(vec[*,1]) & IF (imager$.optical_reflection) THEN aangle= aangle + !dpi

  nccd= N_ELEMENTS(vec)/3  &  result= DBLARR(nccd,2)
  result[0,0]= imager$.ccd_center[0] + SIN(aangle)*rpixel
  result[0,1]= imager$.ccd_center[1] - COS(aangle)*rpixel*imager$.pixel_aspect_ratio

  bad= (result[*,0] LT 0.0) OR (result[*,0] GE imager$.ccd_size[0]) OR $
       (result[*,1] LT 0.0) OR (result[*,1] GE imager$.ccd_size[1]) OR $
       (FINITE(result[*,0]) NE 1) OR (zangle GT MAX(zangle$))

  where_bad= WHERE(bad,nbad)
  IF (nbad GT 0) THEN result[where_bad,*]= !values.d_nan

RETURN,result
END;----------------------------------------------------------------


FUNCTION SKYMAP_TEST_LENS2CCD,nvectors
  COMPILE_OPT HIDDEN
  nfail= 0

  vec0= DOUBLE(RANDOMN(seed,nvectors,3))
  vec0[*,2]= ABS(vec0[*,2])  ;# clip to upper hemisphere

  vec1= SKYMAP_CCD2LENS(SKYMAP_LENS2CCD(vec0))
  dot= TOTAL(vec0*vec1,2)/SQRT(TOTAL(vec0^2,2)*TOTAL(vec1^2,2))
  w= WHERE( FINITE(dot) AND ABS(ACOS(dot<1.0d0)) GE 1.0d-6,nw)
  IF (nw GT 0) THEN BEGIN
    PRINT,nw,type,FORMAT='(I," errors")'
    nfail= nfail+nw
  ENDIF

RETURN,nfail
END;-----------------------------------------------------------------


FUNCTION SKYMAP_TEST_CCD2LENS,nvectors
  COMPILE_OPT HIDDEN
  nfail= 0

  vec0= [RANDOMU(seed,nvectors)*256, RANDOMU(seed,nvectors)*256]
  vec0= REFORM(DOUBLE(vec0),nvectors,2)

  FOR type=0,1 DO FOR units=0,1 DO BEGIN
    vec1= SKYMAP_CCD2LENS(vec0,SPHERICAL=type,DEGREES=units)
    ;wgood= WHERE(vec1[*,2] GT -1.0,nw)
    vec1= SKYMAP_LENS2CCD(vec1,SPHERICAL=type,DEGREES=units)
    dot= TOTAL(vec0*vec1,2)/SQRT(TOTAL(vec0^2,2)*TOTAL(vec1^2,2))
    w= WHERE( FINITE(dot) AND ABS(ACOS(dot<1.0d0)) GE 1.0d-6,nw) ;# allow larger errors due to interpolation
    IF (nw GT 0) THEN BEGIN
      PRINT,nw,type,units,FORMAT='(I," errors for SPHERICAL=",I,", DEGREES=",I)'
      nfail= nfail+nw
    ENDIF
  ENDFOR

RETURN,nfail
END;-----------------------------------------------------------------



;#===========================================================================
;#
;# Site location in geodetic coordinates
;#
;#   latitude
;#   longitude
;#   altitude   metres above mean sea level (geoid)

PRO SKYMAP_SITE__DEFINE
  COMMON SKYMAP_SITE,map$,geo$,geo2aim$,window$
  map$= {SKYMAP_SITE, latitude:0.0d0, longitude:0.0d0, altitude:0.0d0, uid:'', unix_time:0L}
  geo$= DBLARR(3)      ;# cartesian (x,y,z) geocentric location
  geo2aim$= DBLARR(3,3)     ;# rotation matrix from geocentric (x,y,z) to local geodetic (east, north, zenith)
END


;# pre-calculate geo2aim and geocentric coordinates based on geodetic alt/lat/lon
;PRO SKYMAP_SITE_LOAD,latitude,longitude,altitude,UID=uid
PRO SKYMAP_SITE_LOAD,skymap_site

  SKYMAP_SITE__DEFINE
  COMMON SKYMAP_SITE

  map$= skymap_site

  pos= [map$.latitude, map$.longitude, map$.altitude]
  geo$= REFORM(GEODETIC_CONVERT(pos,aim2geo,/FROM_GEODETIC,/TO_CARTESIAN),1,3)
  tmp= DBLARR(3,3)
  tmp[0,*]=  aim2geo[1,*] ;# east
  tmp[1,*]=  aim2geo[0,*] ;# north
  tmp[2,*]= -aim2geo[2,*] ;# up
  geo2aim$= TRANSPOSE(tmp) ;# aim2geo -> geo2aim

RETURN
END;-----------------------------------------------------------------


;# Wrapper for "map_set" which stores the lat/lon for each pixel.
;# This is moderately time consuming but only needs to be done
;# once for each projection and/or window size.
;#
PRO SKYMAP_MAP_SET,p0lat,p0lon,rot,_EXTRA=_extra
  COMMON SKYMAP_SITE
  MAP_SET,p0lat,p0lon,rot,_EXTRA=_extra
  nx= !d.x_size & ny= !d.y_size
  xx= INDGEN(nx) # REPLICATE(1,ny)
  yy= REPLICATE(1,nx) # INDGEN(ny)
  pos= CONVERT_COORD(xx,yy,/DEVICE,/TO_DATA)  ;# [longitude, latitude, altitude]
  window$= REFORM([ [[pos[1,*]]], [[pos[0,*]]], [[pos[2,*]]] ],nx,ny,3)
RETURN
END ;-----------------------------------------------------------------


FUNCTION SKYMAP_GEO2AIM,vector3,FROM_SPHERICAL=from_spherical,FROM_DEGREES=from_degrees $
                               ,INTO_SPHERICAL=into_spherical,INTO_DEGREES=into_degrees
  COMMON SKYMAP_SITE  ;# for geo2aim$
  nvec= N_ELEMENTS(vector3)/3  &  tmp= REFORM(vector3,nvec,3)
  IF KEYWORD_SET(FROM_SPHERICAL) THEN SKYMAP_SPHERICAL2CARTESIAN,tmp,DEGREES=from_degrees
  tmp= tmp - REBIN(REFORM(geo$,1,3),nvec,3,/SAMPLE)
  tmp= geo2aim$ ## tmp
  IF KEYWORD_SET(INTO_SPHERICAL) THEN SKYMAP_CARTESIAN2SPHERICAL,tmp,/ALPHA_ZETA,DEGREES=into_degrees
RETURN,tmp
END;----------------------------------------------------------------



;;!!!FIXME  Not really useful for anything...
;FUNCTION SKYMAP_GEO2MAP,vector3,SPHERICAL=spherical,DEGREES=degrees
;
;  result= vector3
;  IF KEYWORD_SET(SPHERICAL) THEN SKYMAP_SPHERICAL2CARTESIAN,result,DEGREES=degrees
;  result= GEODETIC_CONVERT(TRANSPOSE(result),/FROM_CARTESIAN,/TO_GEODETIC)
;
;RETURN,TRANSPOSE(result)
;END;----------------------------------------------------------------


FUNCTION SKYMAP_MAP2MAG,vector3,EPOCH=epoch
  IF NOT KEYWORD_SET(EPOCH) THEN CDF_EPOCH,epoch,2000,01,01,00,00,00,/COMPUTE
  result= GEODETIC_CONVERT(vector3,/FROM_GEODETIC,/TO_CARTESIAN)
  result= GEOSPACE_CONVERT(result,'GEO','MAG',/INTO_SPHERICAL,CDF_EPOCH=epoch)
RETURN,result
END;----------------------------------------------------------------


FUNCTION SKYMAP_MAP2AIM,vector3,SPHERICAL=spherical,DEGREES=degrees
  COMMON SKYMAP_SITE
  nvec= N_ELEMENTS(vector3)/3
  tmp= GEODETIC_CONVERT(vector3,/FROM_GEODETIC,/TO_CARTESIAN)
  tmp= REFORM(tmp,nvec,3) - REBIN(REFORM(geo$,1,3),nvec,3,/SAMPLE)
  tmp= geo2aim$ ## tmp
  IF KEYWORD_SET(SPHERICAL) THEN SKYMAP_CARTESIAN2SPHERICAL,tmp,/ALPHA_ZETA,DEGREES=degrees
RETURN,tmp
END;----------------------------------------------------------------


FUNCTION SKYMAP_AIM2MAP,vector3,altitude,SPHERICAL=spherical,DEGREES=degrees
  COMMON SKYMAP_SITE

  dims= SIZE(vector3,/DIMENSIONS)
  nvec= N_ELEMENTS(vector3)/3L

  aim= vector3
  IF KEYWORD_SET(SPHERICAL) THEN SKYMAP_SPHERICAL2CARTESIAN,aim,/ALPHA_ZETA,DEGREES=degrees
  dgeo= TRANSPOSE(geo2aim$)##aim  ;# east,north,zenith -> north,east,nadir

  IF (N_PARAMS() EQ 1) THEN BEGIN
    geo0= REBIN(REFORM(geo$,1,3),nvec,3,/SAMPLE)  ;# origin
    geo= REFORM(geo0+dgeo,nvec,3) ;& stop
    RETURN,GEODETIC_CONVERT(geo,/FROM_CARTESIAN,/TO_GEODETIC)
  END

  ;dr= SQRT(TOTAL(dgeo^2,2))
  ;dgeo= dgeo / REBIN(dr,nvec,3,/SAMPLE)

  map= DBLARR(nvec,3)
  FOR indx=0L,nvec-1L DO BEGIN

    dgeo_indx= dgeo[indx,*]
    altitude_indx= (altitude[[indx]])[0]

   ;!!FIXME!! better initial guess would speed up convergence
    rr= [-1.0d4, 0.0d0, 1.0d4, 1.0d5, 1.0d6, 1.0d7, 1.0d8, 1.0d9]
    hh= rr * 0.0d0  &  nn= N_ELEMENTS(rr)  &  geo= REBIN(geo$,nn,3,/SAMPLE)
    FOR jndx=0,nn-1 DO geo[jndx,0]= geo[jndx,*] + rr[jndx] * dgeo_indx
    hh= (GEODETIC_CONVERT(geo,/FROM_CARTESIAN,/TO_GEODETIC))[*,2]

    IF (TOTAL(FINITE(hh)) NE nn) THEN BEGIN
      map[indx,*]= REPLICATE(!values.d_nan,1,3)
      CONTINUE
    ENDIF


    ;FOR jndx=0,N_ELEMENTS(hh)-1 DO BEGIN
    ;  geo= geo$ + rr[jndx] * dgeo_indx ;& stop
    ;  hh[jndx]= (GEODETIC_CONVERT(geo,/FROM_CARTESIAN,/TO_GEODETIC))[2]
    ;ENDFOR

    FOR jndx=0,9 DO BEGIN

      IF (jndx LE 4) AND ((jndx MOD 2) EQ 0) THEN BEGIN
        lo= MAX(WHERE(hh LT altitude_indx))  &  hi= MIN(WHERE(hh GT altitude_indx))
        rr0= (rr[lo]+rr[hi])/2.0d0 ;& print,lo,hi ;& stop
        ;rr0= INTERPOL(rr,ATAN(hh/1d5),ATAN(altitude_indx/1d5))
      ENDIF ELSE IF (jndx LE 4) AND ((jndx MOD 2) EQ 1) THEN BEGIN
        rr0= INTERPOL(rr,ATAN(hh/1d6),ATAN(altitude_indx/1d6))
      ENDIF ELSE BEGIN
        rr0= INTERPOL(rr,hh,altitude_indx,/QUADRATIC)
      ENDELSE

      geo= geo$ + rr0[0] * dgeo_indx ;& stop
      _map= GEODETIC_CONVERT(geo,/FROM_CARTESIAN,/TO_GEODETIC)
      IF (ABS(_map[2]-altitude_indx) LE 0.1) THEN BREAK  ;# sub-metre accuracy
      hh= [_map[2], hh]  &  rr= [rr0[0], rr]  ;& stop
      s= UNIQ(hh,SORT(hh)) & hh= hh[s] & rr= rr[s] ; & print,rr,hh ;& stop
      ;print,rr,hh,altitude_indx
    ENDFOR
;IF (ABS(altitude_indx-_map[2]) GE 1.0) THEN STOP
    map[indx,*]= REFORM(_map,1,3)
  ENDFOR

  map[0,1]= (map[*,1] + 360.0) MOD 360.0  ;# keep IDL plotting happy
RETURN,map
END;----------------------------------------------------------------


FUNCTION SKYMAP_TEST_AIM2MAP,nvectors
  COMPILE_OPT HIDDEN
  nfail=0

  altitude= DOUBLE(RANDOMU(seed,nvectors)*999.0d3)
  vec0= DOUBLE(RANDOMN(seed,nvectors,3))
  vec0[*,2]= ABS(vec0[*,2])  ;# clip to upper hemisphere

  vec1= SKYMAP_AIM2MAP(vec0,altitude)
  w= WHERE( ABS(vec1[*,2] - altitude) GE 1.0,nw) ;# 1 metre
  IF (nw GT 0) THEN BEGIN
    PRINT,nw,nvectors,altitude[w[0:(3<nw)-1]],vec1[w[0:(3<nw)-1],2] $
      ,FORMAT='(I," height errors in ",I," vectors",4(F8.1,","),4(F8.1,","))'
      stop
    nfail= nfail+nw
  ENDIF

  vec1= SKYMAP_MAP2AIM(vec1)
  dot= TOTAL(vec0*vec1,2)/SQRT(TOTAL(vec0^2,2)*TOTAL(vec1^2,2))
  w= WHERE( FINITE(dot) AND ABS(ACOS(dot<1.0d0)) GE 2.0d-5,nw)
  IF (nw GT 0) THEN BEGIN
    PRINT,nw,nvectors,FORMAT='(I," angular errors in ",I," vectors")'
    nfail= nfail+nw
  ENDIF

RETURN,nfail
END;----------------------------------------------------------------





;#===========================================================================
;#
;# The image frame (IMG) is obtained by selecting a rectangular
;# subregion of the CCD, possibly applying binning, and possibly
;# flipping x and/or y on readout:
;#
;#  offset= x,y        pixels relative to CCD origin
;#  size= nx,ny
;#  binning= bx,by
;#  img_flip=[ 0 or 1, 0 or 1]
;#
;# to produce an image frame of dimensions nx/bx, ny/by
;#
;# LINEAR coordinates are used to
;#
;#
;# Each frame also has an associated time (epoch) that can be used
;# to carry out transformations betweem GEO and GEI coordinates.
;#

PRO SKYMAP_IMG__DEFINE
  COMMON SKYMAP_IMG,offset$,binning$,frame$,epoch$,gei2geo$
RETURN
END;-----------------------------------------------------------------


 ; pre-calculate gei2geo based on exposure time of each frame
PRO SKYMAP_IMG_LOAD,epoch,frame,ccd_offset,ccd_binning

  SKYMAP_IMG__DEFINE
  COMMON SKYMAP_IMG

  IF (N_PARAMS() LE 2) THEN offset$=[0,0] ELSE offset$= ccd_offset
  IF (N_PARAMS() LE 3) THEN binning$=[1,1] ELSE binning$= ccd_binning

  dims= SIZE(frame,/DIMENSIONS)
  IF (N_ELEMENTS(dims) EQ 3) THEN nframes= dims[2] ELSE nframes=1

  epoch$= epoch
  frame$= frame
  gei2geo$= DBLARR(3,3,nframes)

  r= [1.0,0,0]
  FOR indx=0,nframes-1 DO BEGIN
    dummy= GEOSPACE_CONVERT(r,'GEI','GEO',CDF_EPOCH=epoch$[indx],ROTATION_MATRIX=rot)
    gei2geo$[0,0,indx]= rot
  ENDFOR

RETURN
END;-----------------------------------------------------------------


 ;# x,y coordinates of image pixel centers (default) or corners
FUNCTION SKYMAP_IMG_XY,nx,ny,CORNERS=corners
  SKYMAP_IMG__DEFINE  &  COMMON SKYMAP_IMG

  IF (N_PARAMS() EQ 2) THEN dim=[nx[0],ny[0]] ELSE $
                            dim= SIZE(frame$,/DIMENSIONS)

  IF KEYWORD_SET(CORNERS) THEN BEGIN
    nx= dim[0]+1 & ny=dim[1]+1
    x= LINDGEN(nx) # REPLICATE(1L,ny)
    y= REPLICATE(1L,nx) # LINDGEN(ny)
  ENDIF ELSE BEGIN
    nx= dim[0] & ny=dim[1]
    x= LINDGEN(nx) # REPLICATE(1L,ny) + 0.5
    y= REPLICATE(1L,nx) # LINDGEN(ny) + 0.5
  ENDELSE

RETURN,[[[x]],[[y]]] ;# nx,ny,2 = ncolumns, nrows, 2
END;-----------------------------------------------------------------



FUNCTION SKYMAP_IMG2CCD,vector2,LINEAR=linear

  COMMON SKYMAP_IMAGER
  COMMON SKYMAP_IMG

  dim= SIZE(frame$,/DIMENSIONS)
  IF KEYWORD_SET(LINEAR) THEN BEGIN
    result= LONARR(N_ELEMENTS(vector2),2)
    result[0,0]= REFORM(LONG(vector2) MOD dim[0])
    result[0,1]= REFORM(LONG(vector2) / dim[0])
    bad= vector2 EQ -1L
  ENDIF ELSE BEGIN
    result= vector2
    bad= (result[*,0] EQ -1L) OR (result[*,1] EQ -1L)
  ENDELSE

  IF (imager$.img_flip[0]) THEN result[0,0]= (dim[0]-1) - result[*,0]
  IF (imager$.img_flip[1]) THEN result[0,1]= (dim[1]-1) - result[*,1]

  ;# convert from image pixels to CCD device pixels
  result[0,0]= result[*,0] * binning$[0] + offset$[0]
  result[0,1]= result[*,1] * binning$[1] + offset$[1]

  where_bad= WHERE(bad,nbad)
  IF (nbad GT 0) THEN result[wbad,*]= !values.d_nan

RETURN,result
END;-----------------------------------------------------------------



 ;# convert from physical CCD pixels to binned image pixels
FUNCTION SKYMAP_CCD2IMG,vector2,LINEAR=linear

  COMMON SKYMAP_IMAGER
  COMMON SKYMAP_IMG

  nccd= N_ELEMENTS(vector2)/2
  result= DBLARR(nccd,2,/NOZERO)
  result[0,0]= DOUBLE(vector2[*,0]-offset$[0]) / binning$[0]
  result[0,1]= DOUBLE(vector2[*,1]-offset$[1]) / binning$[1]

  dim= SIZE(frame$,/DIMENSIONS)
  IF (imager$.img_flip[0]) THEN result[0,0]= dim[0]-1 - result[*,0]
  IF (imager$.img_flip[1]) THEN result[0,1]= dim[1]-1 - result[*,1]

  bad= (result[*,0] LT 0.0) OR (result[*,0] GE dim[0]) OR $
       (result[*,1] LT 0.0) OR (result[*,1] GE dim[1])
  where_bad= WHERE(bad,nbad) ;# check if actually within frame

  IF KEYWORD_SET(LINEAR) THEN BEGIN
    result= FLOOR(result,/L64) ;# don't use "fix()"
    result= REFORM( result[*,0] + result[*,1]*dim[0] ,nccd)
    IF (nbad GT 0) THEN result[where_bad]= -1L
  ENDIF ELSE BEGIN
    IF (nbad GT 0) THEN result[where_bad,*]= !values.f_nan
  ENDELSE

RETURN,result
END;-----------------------------------------------------------------


FUNCTION SKYMAP_TEST_IMG2CCD,nvectors
  COMPILE_OPT HIDDEN
  nfail= 0

  COMMON SKYMAP_IMG
  dim= SIZE(frame$,/DIMENSIONS)

  vec0= DBLARR(nvectors,2)
  vec0[0,0]= RANDOMU(seed,nvectors) * dim[0]
  vec0[0,1]= RANDOMU(seed,nvectors) * dim[1]

  vec1= SKYMAP_IMG2CCD(vec0)
  vec1= SKYMAP_CCD2IMG(vec1)

  dot= TOTAL(vec0*vec1,2)/SQRT(TOTAL(vec0^2,2)*TOTAL(vec1^2,2))
  w= WHERE( FINITE(dot) AND ABS(ACOS(dot<1.0d0)) GE 1.0d-6,nw)
  IF (nw GT 0) THEN BEGIN
    PRINT,nw,nvectors,FORMAT='(I," errors in ",I," vectors")'
    nfail= nfail+nw
  ENDIF

RETURN,nfail
END;-----------------------------------------------------------------


 ;# These are useful for  eg. contours of zenith angle
 ;#
FUNCTION SKYMAP_IMG2AIM,vector2,SPHERICAL=spherical,DEGREES=degrees,LINEAR=linear
  COMMON SKYMAP_IMAGER
  result= SKYMAP_IMG2CCD(vector2,LINEAR=linear)
  result= TRANSPOSE(aim2lens$) ## SKYMAP_CCD2LENS(result)
  IF KEYWORD_SET(SPHERICAL) THEN SKYMAP_CARTESIAN2SPHERICAL,result,/ALPHA_ZETA,DEGREES=degrees
RETURN,result
END;----------------------------------------------------------------


FUNCTION SKYMAP_AIM2IMG,vector3,SPHERICAL=spherical,DEGREES=degrees,LINEAR=linear
  COMMON SKYMAP_IMAGER
  result= vector3
  IF KEYWORD_SET(SPHERICAL) THEN SKYMAP_SPHERICAL2CARTESIAN,result,/ALPHA_ZETA,DEGREES=degrees
  result= SKYMAP_LENS2CCD(aim2lens$ ## result)
  result= SKYMAP_CCD2IMG(result,LINEAR=linear)
RETURN,result
END;-----------------------------------------------------------------





; Two trivial functions that are very useful in GEI<>FRAME conversions
;  eg.  IMG<>CCD<>LENS<>AIM<>GEO <time> GEI

FUNCTION SKYMAP_IMG2GEO,vector2,LINEAR=linear
  COMMON SKYMAP_SITE
  COMMON SKYMAP_IMAGER
  result= SKYMAP_IMG2CCD(vector2,LINEAR=linear)
  result= TRANSPOSE(aim2lens$##geo2aim$) ## SKYMAP_CCD2LENS(result)
RETURN,result
END;----------------------------------------------------------------


FUNCTION SKYMAP_GEO2IMG,vector3,LINEAR=linear
  COMMON SKYMAP_SITE
  COMMON SKYMAP_IMAGER
  result= SKYMAP_LENS2CCD((aim2lens$##geo2aim$)##vector3)
  result= SKYMAP_CCD2IMG(result,LINEAR=linear)
RETURN,result
END;----------------------------------------------------------------




PRO SKYMAP_IMG_MAP__DEFINE
  dummy= {SKYMAP_IMG_MAP, img:FLTARR(2), aim:FLTARR(3)  $
          ,azimuth:0.0, zenith_angle:0.0 $
          ,latitude:0.0, longitude:0.0, altitude:0.0}
RETURN
END;----------------------------------------------------------------


 ;# Produce a handy image shaped structure containing pointing info
 ;#
FUNCTION SKYMAP_IMG_MAP,altitude,CORNERS=corners,AIM_ONLY=aim_only
  COMMON SKYMAP_IMAGER
  COMMON SKYMAP_IMG

  img_xy= SKYMAP_IMG_XY(CORNERS=corners)
  dim= SIZE(img_xy,/DIMENSIONS)  &  nx= dim[0]  &  ny= dim[1]
  result= REPLICATE({SKYMAP_IMG_MAP},nx,ny)
  result.img= TRANSPOSE(img_xy,[2,0,1])
  pixel= REFORM(img_xy,nx*ny,2)

  aim= SKYMAP_IMG2AIM(pixel,/SPHERICAL,/DEGREES)
  result.zenith_angle= REFORM(aim[*,0],nx,ny)
  result.azimuth= REFORM(aim[*,1],nx,ny)

  aim= SKYMAP_IMG2AIM(pixel)  ;# cartesian 3-vector
  result.aim= TRANSPOSE( REFORM(aim,nx,ny,3), [2,0,1])
  IF KEYWORD_SET(AIM_ONLY) THEN RETURN,result

   ;# very time consuming
  outval= SKYMAP_AIM2MAP(aim,altitude) ;# usually 110km
  result.latitude= REFORM(outval[*,0],nx,ny)
  result.longitude= REFORM(outval[*,1],nx,ny)

RETURN,result
END;----------------------------------------------------------------





;#===========================================================================
;#
;# GEI coordinates:
;#
;#   declination
;#   right ascension
;#
;#
;# All transformations to this point have been time independent.  These will now
;# require use of an epoch value such as the one associated with each image frame.
;#


FUNCTION SKYMAP_GEI2AIM,vector3,epoch,FROM_SPHERICAL=from_spherical,INTO_SPHERICAL=into_spherical $
                                     ,FROM_DEGREES=from_degrees,INTO_DEGREES=into_degrees
  COMMON SKYMAP_SITE

  nvec= N_ELEMENTS(vector3)/3  &  tmp= REFORM(vector3,nvec,3)
  IF KEYWORD_SET(FROM_SPHERICAL) THEN SKYMAP_SPHERICAL2CARTESIAN,tmp,DEGREES=from_degrees
  dummy= GEOSPACE_CONVERT([1.0,0.0,0.0],'GEI','GEO',CDF_EPOCH=epoch,ROTATION_MATRIX=gei2geo)
  tmp= (geo2aim$ ## gei2geo) ## tmp
  IF KEYWORD_SET(INTO_SPHERICAL) THEN SKYMAP_CARTESIAN2SPHERICAL,tmp,/ALPHA_ZETA,DEGREES=into_degrees
RETURN,tmp
END;----------------------------------------------------------------


FUNCTION SKYMAP_AIM2GEI,vector3,epoch,FROM_SPHERICAL=from_spherical,INTO_SPHERICAL=into_spherical $
                                     ,FROM_DEGREES=from_degrees,INTO_DEGREES=into_degrees
  COMMON SKYMAP_SITE

  nvec= N_ELEMENTS(vector3)/3  &  tmp= REFORM(vector3,nvec,3)
  IF KEYWORD_SET(FROM_SPHERICAL) THEN SKYMAP_SPHERICAL2CARTESIAN,tmp,/ALPHA_ZETA,DEGREES=from_degrees
  dummy= GEOSPACE_CONVERT([1.0,0.0,0.0],'GEI','GEO',CDF_EPOCH=epoch,ROTATION_MATRIX=gei2geo)
  tmp= TRANSPOSE(geo2aim$ ## gei2geo) ## tmp
  IF KEYWORD_SET(INTO_SPHERICAL) THEN SKYMAP_CARTESIAN2SPHERICAL,tmp,DEGREES=into_degrees
RETURN,tmp
END;----------------------------------------------------------------


FUNCTION SKYMAP_TEST_GEI2AIM,nvectors
  COMPILE_opt hidden
  nfail= 0

   ;# Forward/inverse pairs
   ;#
  site= {SKYMAP_SITE}
  site.latitude= 25.0
  site.longitude= 115.0
  site.altitude= 1234.0
  SKYMAP_SITE_LOAD,site
  CDF_EPOCH,epoch,2006,10,22,20,30,40,/COMPUTE
  vec0= DOUBLE(RANDOMN(seed,nvectors,3))
  vec1= SKYMAP_AIM2GEI(SKYMAP_GEI2AIM(vec0,epoch),epoch)

  dot= TOTAL(vec0*vec1,2)/SQRT(TOTAL(vec0^2,2)*TOTAL(vec1^2,2))
  w= WHERE( ABS(ACOS(dot<1.0d0)) GE 1.0d-6,nw)
  IF (nw GT 0) THEN BEGIN
    PRINT,nw,nvectors,FORMAT='(" GEI2AIM2GEI errors: ",I," of ",I)'
    nfail= nfail+nw
  ENDIF


   ;# Known values
   ;#
  note= 'Vega from Calgary according to US Naval Observatory'

  ;# 51.079528N 114.129778W 1143m
  site.latitude= SKYMAP_DMS2DEG(51,04,46.3)
  site.longitude= -SKYMAP_DMS2DEG(114,7,47.2)
  site.altitude= 1143.0
  SKYMAP_SITE_LOAD,site
  CDF_EPOCH,epoch,2006,10,19,10,20,30,/COMPUTE

  zangle= SKYMAP_DMS2DEG(86,14,00.3) ;86.233417N
  aangle= SKYMAP_DMS2DEG(336,36,54.8) ;336.615222W
  aim= [zangle, aangle, 1.0d0] ;# zenith angle, azimuth angle, range

  ras= SKYMAP_HMS2DEG(18,37,9.524) ;38.792444d0
  dec= SKYMAP_DMS2DEG(38,47,32.92) ;279.289583d0
  gei= [dec, ras, 1.0d0] ;# declination, right ascension, range

  vec0= aim & SKYMAP_SPHERICAL2CARTESIAN,vec0,/DEGREES,/ALPHA_ZETA
  vec1= SKYMAP_AIM2GEI(vec0,epoch)
  vec0= gei & SKYMAP_SPHERICAL2CARTESIAN,vec0,/DEGREES
  dot= TOTAL(vec0*vec1,2)/SQRT(TOTAL(vec0^2,2)*TOTAL(vec1^2,2))
  IF (ABS(ACOS(dot<1.0d0)) GE 1.0d-5 GT 0) THEN BEGIN
    PRINT," AIM2GEI error: ",note ;& stop
    nfail= nfail+1
  ENDIF

  vec0= gei & SKYMAP_SPHERICAL2CARTESIAN,vec0,/DEGREES
  vec1= SKYMAP_GEI2AIM(vec0,epoch)
  vec0= aim & SKYMAP_SPHERICAL2CARTESIAN,vec0,/DEGREES,/ALPHA_ZETA

  dot= TOTAL(vec0*vec1,2)/SQRT(TOTAL(vec0^2,2)*TOTAL(vec1^2,2))
  IF (ABS(ACOS(dot<1.0d0)) GE 1.0d-5 GT 0) THEN BEGIN
    PRINT," GEI2AIM error: ",note ;& stop
    nfail= nfail+1
  ENDIF


   ;# Known values
   ;#
  note= 'Rankin Inlet zenith according to Skycalc'

  site.latitude= SKYMAP_DMS2DEG(62,49.5)
  site.longitude= -SKYMAP_DMS2DEG(92,06.9)
  site.altitude= 6.0d0
  SKYMAP_SITE_LOAD,site
  CDF_EPOCH,epoch,2006,10,20,02,32,46,/COMPUTE

  zangle= SKYMAP_DMS2DEG(0)
  aangle= SKYMAP_DMS2DEG(270)
  aim= [zangle, aangle, 1.0d0] ;# zenith angle, azimuth angle, range

  ras= SKYMAP_HMS2DEG(22,17,59.4)
  dec= SKYMAP_DMS2DEG(62,49,29)
  gei= [dec, ras, 1.0d0] ;# declination, right ascension, range

  vec0= aim & SKYMAP_SPHERICAL2CARTESIAN,vec0,/DEGREES,/ALPHA_ZETA
  vec1= SKYMAP_AIM2GEI(vec0,epoch)
  vec0= gei & SKYMAP_SPHERICAL2CARTESIAN,vec0,/DEGREES
  dot= TOTAL(vec0*vec1,2)/SQRT(TOTAL(vec0^2,2)*TOTAL(vec1^2,2))
  IF (ABS(ACOS(dot<1.0d0)) GE 1.0d-5 GT 0) THEN BEGIN
    PRINT," AIM2GEI error: ",note ;& stop
    nfail= nfail+1
  ENDIF

  vec0= gei & SKYMAP_SPHERICAL2CARTESIAN,vec0,/DEGREES
  vec1= SKYMAP_GEI2AIM(vec0,epoch)
  vec0= aim & SKYMAP_SPHERICAL2CARTESIAN,vec0,/DEGREES,/ALPHA_ZETA
  dot= TOTAL(vec0*vec1,2)/SQRT(TOTAL(vec0^2,2)*TOTAL(vec1^2,2))
  IF (ABS(ACOS(dot<1.0d0)) GE 1.0d-5 GT 0) THEN BEGIN
    PRINT," GEI2AIM error: ",note ;& stop
    nfail= nfail+1
  ENDIF

RETURN,nfail
END;-----------------------------------------------------------------


FUNCTION SKYMAP_GEI2IMG,vector3,epoch,SPHERICAL=spherical,DEGREES=degrees
  COMMON SKYMAP_SITE
  COMMON SKYMAP_IMAGER

  nvec= N_ELEMENTS(vector3)/3  &  tmp= REFORM(vector3,nvec,3)
  IF KEYWORD_SET(SPHERICAL) THEN SKYMAP_SPHERICAL2CARTESIAN,tmp,DEGREES=degrees
  dummy= GEOSPACE_CONVERT([1,1,1],'GEI','GEO',CDF_EPOCH=epoch,ROTATION_MATRIX=gei2geo)
  mat= aim2lens$ ## ( geo2aim$ ## gei2geo )
  tmp= SKYMAP_CCD2IMG(SKYMAP_LENS2CCD(mat##tmp))
RETURN,tmp
END;----------------------------------------------------------------

FUNCTION SKYMAP_IMG2GEI,vector2,epoch,LINEAR=linear,SPHERICAL=spherical,DEGREES=degrees
  COMMON SKYMAP_SITE
  COMMON SKYMAP_IMAGER

  dummy= GEOSPACE_CONVERT([1,1,1],'GEI','GEO',CDF_EPOCH=epoch,ROTATION_MATRIX=gei2geo)
  gei2lens= aim2lens$ ## ( geo2aim$ ## gei2geo )
  result= TRANSPOSE(gei2lens) ## SKYMAP_CCD2LENS(SKYMAP_IMG2CCD(vector2,LINEAR=linear))
  IF KEYWORD_SET(SPHERICAL) THEN SKYMAP_CARTESIAN2SPHERICAL,result,DEGREES=degrees
RETURN,result
END;----------------------------------------------------------------



 ;# Sun pointing from definition of GSE coordinates
 ;#
FUNCTION SKYMAP_SUN_AIM,epoch,SPHERICAL=spherical,DEGREES=degrees
  COMMON SKYMAP_SITE
  gse= [1.0d0, 0.0d0, 0.0d0]
  geo= GEOSPACE_CONVERT(gse,'GSE','GEO',CDF_EPOCH=epoch)
  result= geo2aim$ ## geo
  IF KEYWORD_SET(SPHERICAL) THEN SKYMAP_CARTESIAN2SPHERICAL,result,/ALPHA_ZETA,DEGREES=degrees
RETURN,result
END;----------------------------------------------------------------





PRO SKYMAP_STAR_LOAD
  COMMON SKYMAP_STAR,ybsc$,ras$,dec$

  IF (N_ELEMENTS(ybsc$) NE 0) THEN RETURN

  MESSAGE,'loading Yale Bright Star Catalog',/INFORMATIONAL & WAIT,0.01
  ybsc$= YALEBRIGHTSTAR_READFILE(/COMPACT)
  ybsc$= ybsc$[SORT(-ybsc$.declination)]

  bright= ybsc$.visual_magnitude LE 3.5
  north= (ybsc$.declination GT -20.0) AND (ybsc$.declination NE 0.0)
  w= WHERE(bright AND north,nstars)
  ras$= ybsc$[w].right_ascension
  dec$= ybsc$[w].declination

RETURN
END ;--------------------------------------------------------------------



;??FIXME?? consider removing structure and just having Nx3 GEI array plus support info
PRO SKYMAP_GRID__DEFINE
  COMMON SKYMAP_GRID,grid$ ;#,grid_frame$

  grid= {SKYMAP_GRID         $
         ,gei:DBLARR(3)         $
         ,declination:0.0d0     $
         ,ascension:0.0d0       $
         ,brightstar_angle:0.0d0 }

  RETURN
END

PRO SKYMAP_GRID_LOAD,declination,right_ascension,spin,GRID_NX=grid_nx,GRID_NY=grid_ny,DANGLE=dangle
  COMMON SKYMAP_STAR
  COMMON SKYMAP_GRID

  IF (N_PARAMS() LT 3) THEN spin=0.0
  IF NOT KEYWORD_SET(DANGLE) THEN dangle= 6.0
  deg2rad= !dpi/180.0d0

; dras= dangle*deg2rad &  ddec= dangle*deg2rad
  IF KEYWORD_SET(GRIDNX) THEN grid_nx= gridnx ELSE grid_nx= 70
  IF KEYWORD_SET(GRIDNY) THEN grid_ny= gridny ELSE grid_ny= 80
  npoints= LONG(grid_nx)*grid_ny

  ngrids= N_ELEMENTS(right_ascension) > N_ELEMENTS(declination)
  grid$= REPLICATE({SKYMAP_GRID},grid_nx,grid_ny,ngrids)

  grid0= DBLARR(grid_nx,grid_ny,3)
  grid0[0,0,0]= dangle* ( (2*FINDGEN(grid_nx)/(grid_nx-1.0) - 1) # REPLICATE(1.0,grid_ny) )
  grid0[0,0,1]= dangle* ( REPLICATE(1.0,grid_nx) # (2*FINDGEN(grid_ny)/(grid_ny-1.0) - 1) )
  grid0[0,0,2]= REPLICATE(1.0d0,grid_nx,grid_ny) ;# fake radius
  SKYMAP_SPHERICAL2CARTESIAN,grid0,/DEGREES
  grid0= REFORM(grid0,grid_nx*grid_ny,3)

  FOR indx=0,ngrids-1 DO BEGIN
    roty= SKYMAP_EULER_MATRIX(0.0, declination[[indx]], 0.0,/DEGREES)
    rotz= SKYMAP_EULER_MATRIX(-right_ascension[[indx]], 0.0, 0.0,/DEGREES)
    rotn= SKYMAP_EULER_MATRIX(0.0, 0.0, spin[[indx]] ,/DEGREES)
    gei= REFORM((rotn##(rotz##roty))##grid0,grid_nx,grid_ny,3) ;# N x 3
    grid$[*,*,indx].gei= TRANSPOSE(gei,[2,0,1]) ;# 3 x N
  ENDFOR

   ;# !!FIXME!! use CARTESIAN2SPHERICAL
  gei= grid$.gei
  grid$.ascension= REFORM(ATAN(gei[1,*,*,*],gei[0,*,*,*])/deg2rad)
  grid$.ascension= (grid$.ascension+360.0) MOD 360.0
  grid$.declination= REFORM(ATAN(gei[2,*,*,*],SQRT(gei[0,*,*,*]^2+gei[1,*,*,*]^2))/deg2rad)

  IF (N_ELEMENTS(ras$) GT 0) THEN BEGIN
    star_angle= -1.0d0
    gei= REFORM(gei,3,npoints*ngrids)
    one= REPLICATE(1.0d0,npoints*ngrids)
    FOR indx=0,N_ELEMENTS(ras$)-1 DO BEGIN
      star_gei= REFORM([dec$[indx], ras$[indx], 1.0d0],3,1)
      SKYMAP_SPHERICAL2CARTESIAN,star_gei,/DEGREES
      star_gei= REFORM(star_gei) # one
      star_angle= star_angle > TOTAL(star_gei*gei,1)
    ENDFOR
    grid$[*].brightstar_angle= ACOS(star_angle)/deg2rad
  ENDIF

RETURN
END ;------------------------------------------------------------




 ; This confusingly named function projects each image frame
 ; onto one or more common celestial grids
 ;
FUNCTION SKYMAP_IMG2GRID
  COMMON SKYMAP_GRID
  COMMON SKYMAP_IMG

  dim= SIZE(grid$,/DIMENSIONS)
  grid_nx= dim[0]  &  grid_ny= dim[1]
  IF (N_ELEMENTS(dim) LE 2) THEN ngrids=1 ELSE ngrids= dim[2]
  npoints= grid_nx*grid_ny*ngrids
  gei= TRANSPOSE(REFORM(grid$.gei,3,npoints)) ;# N x 3

  nframes= N_ELEMENTS(epoch$)
  grid_frame= UINTARR(npoints,nframes)

  FOR indx=0,nframes-1 DO BEGIN
    geo= REFORM(gei2geo$[*,*,indx]) ## gei
    pixel= SKYMAP_GEO2IMG(geo,/LINEAR)
    counts= (REFORM(frame$[*,*,indx]))[pixel]
    valid= (pixel NE -1L) ;AND (frame_pixel NE -1L)
    grid_frame[0,indx]= counts * valid
  ENDFOR

  grid_frame= REFORM(grid_frame,grid_nx,grid_ny,ngrids,nframes)
;  grid_frame= TRANSPOSE(grid_frame,[3,0,1,2])

RETURN,grid_frame
END ;------------------------------------------------------------





PRO SKYMAP_FIT_LOAD_VALUE,tagname,value,subscript,CLEAR=clear
  COMMON SKYMAP_FIT,param$

  IF (N_PARAMS() LT 3) THEN subscript=0
  IF (N_PARAMS() LT 2) THEN value=0.0d0
  IF (N_PARAMS() LT 1) THEN tagname=''

  new= {SKYMAP_FIT2, tagname:STRING(tagname), subscript:LONG(subscript), value:DOUBLE(value)}
  ;new= {SKYMAP_FIT, tagname, subscript, value}

  IF KEYWORD_SET(CLEAR) OR (N_ELEMENTS(param$) EQ 0) THEN param$= new

  w= WHERE((param$.tagname EQ tagname) AND (param$.subscript EQ subscript),nw)
  IF (nw EQ 0) THEN param$= [param$, new] ELSE param$[w]= new

RETURN
END





FUNCTION SKYMAP_FIT_METRIC1,param,SHOW=show,MAX_ANGLE=max_angle
  COMMON SKYMAP_IMAGER
  COMMON SKYMAP_IMG
  COMMON SKYMAP_GRID
  COMMON SKYMAP_FIT

  IF NOT KEYWORD_SET(MAX_ANGLE) THEN max_angle=3

  np= N_ELEMENTS(param)
  FOR indx=0,np-1 DO param$[indx].value= param[indx]

  imager= imager$
  tagnames= STRUPCASE(TAG_NAMES(imager))
  FOR indx=0,N_ELEMENTS(param$)-1 DO BEGIN
    tmp= param$[indx]
    w= WHERE(STRUPCASE(tmp.tagname) EQ tagnames,nw)
    IF (nw EQ 1) THEN imager.(w)[tmp.subscript]= tmp.value
  ENDFOR

  SKYMAP_IMAGER_LOAD,imager
  grid_frame= SKYMAP_IMG2GRID()
  dim= SIZE(grid_frame,/DIMENSION)
  ngrids=dim[2]  &  nframes= dim[3]

  tmp= TOTAL(grid_frame,4,/NAN)/nframes
  FOR indx=0,ngrids-1 DO BEGIN
    tmp[0,0,indx]= tmp[*,*,indx] - MIN(tmp[*,*,indx],/NAN)
    tmp[0,0,indx]= tmp[*,*,indx] / (TOTAL(tmp[*,*,indx],/NAN) > 1)
  ENDFOR

  weight= (grid$.brightstar_angle<max_angle) - max_angle
  result= TOTAL(tmp*weight,/NAN) ;& print,result
  print,result,param

  IF KEYWORD_SET(SHOW) OR 1 THEN BEGIN
    _p= !p
    IF (ngrids GE 2) THEN !p.multi=[0,2,2]
    IF (ngrids GE 5) THEN !p.multi=[0,3,2]
    IF (ngrids GE 7) THEN !p.multi=[0,3,3]
   img= SQRT(tmp) & img=tmp
   levels= [0,0.5,INDGEN(max_angle)+1]
   FOR indx=0,ngrids-1 DO BEGIN
     SHADE_SURF,img[*,*,indx]*0,shade=BYTSCL(img[*,*,indx]),ax=90,az=0
     CONTOUR,grid$[*,*,indx].brightstar_angle,/overplot,level=levels,color=!p.background
   ENDFOR
    !p= _p
  ENDIF
 ; print,result,parameters
RETURN,result
END



FUNCTION SKYMAP_FIT_METRIC2,param,SHOW=show
  COMMON SKYMAP_IMAGER
  COMMON SKYMAP_IMG
  COMMON SKYMAP_GRID
  COMMON SKYMAP_FIT

  np= N_ELEMENTS(param)
  FOR indx=0,np-1 DO param$.value= param[indx]

  tagnames= STRUPCASE(TAG_NAMES(asi_info$))
  FOR indx=0,N_ELEMENTS(param$)-1 DO BEGIN
    tmp= param$[indx]
    w= WHERE(STRUPCASE(tmp.tagname) EQ tagnames,nw)
    IF (nw EQ 1) THEN asi_info$.(w)[tmp.subscript]= tmp.value
    print,tmp.tagname,tmp.value
  ENDFOR
  SKYMAP_ASI_LOAD,asi_info$

  grid_frame= SKYMAP_IMG2GRID()
  dim= SIZE(grid_frame,/DIMENSION)
  ngrids=dim[2]  &  nframes= dim[3]

  tmp= TOTAL(grid_frame,4)/nframes
  tmp= tmp - MIN(tmp)
  tmp= ROUND(SQRT(tmp))
  result= SYMBOL_ENTROPY(tmp) & print,result

  ;FOR indx=0,ngrids-1 DO BEGIN
  ;  tmp[0,0,indx]= tmp[*,*,indx] - MIN(tmp[*,*,indx])
  ;  tmp[0,0,indx]= tmp[*,*,indx] / TOTAL(tmp[*,*,indx])
  ;ENDFOR

  ;weight= (grid$.yalebrightstar_angle<4.0) - 4.0
  ;result= TOTAL(tmp*weight)


  ;show= fit_show OR KEYWORD_SET(SHOW)
  IF KEYWORD_SET(SHOW) OR 1 THEN BEGIN
    _p= !p
    IF (ngrids EQ 1) THEN !p.multi=[0,1,1]
    IF (ngrids GE 4) THEN !p.multi=[0,2,2]
    IF (ngrids GE 6) THEN !p.multi=[0,2,3]
    IF (ngrids GE 8) THEN !p.multi=[0,3,3]
   img= SQRT(tmp) & img=tmp
   FOR indx=0,ngrids-1 DO BEGIN
 ;  stop
 ;    grid_ras= grid[*,*,indx].right_ascension
 ;    grid_dec= grid[*,*,indx].declination
 ;    PLOT,grid_ras,grid_dec,/NODATA,/YNOZERO
 ;    PLOT_IMAGE,REFORM(img[*,*,indx]),/OVERPLOT

    ;LOADCT,27
     SHADE_SURF,img,shade=BYTSCL(img),ax=90,az=0
     CONTOUR,grid$.yalebrightstar_angle,/overplot,level=[0,1,2,3,4,5],color=!p.background

     ;SHADE_SURF,img,shade=BYTSCL(-grid$.yalebrightstar_angle)

   ENDFOR
    !p= _p
  ENDIF
 ; print,result,parameters
RETURN,result
END


FUNCTION SKYMAP_FIT,params
  COMMON SKYMAP_FIT
  COMMON SKYMAP_GRID

  np= N_ELEMENTS(params)
  FOR indx=0,np-1 DO param$.value= params[indx]
  result= SKYMAP_FIT_METRIC1()

RETURN,result
END



PRO SKYMAP_TEST

  imager= {SKYMAP_IMAGER}
  imager.uid='test-device' & imager.unix_time=0L
  imager.optical_orientation= [10.0, -2.0, 1.0] ;# yaw, pitch, tilt (degrees)
  ;imager.optical_angle= 170.0/2.0 * !dtor ;# maximum axial angle
  imager.optical_projection[[0,1,2,3],0]= [0.0, 160.0, 0.00, 4.5]
  imager.optical_projection[[0,1,2,3],1]= [1.0,   0.0, 0.25, 0.0]
  imager.optical_reflection= 0b
  imager.img_flip= [0b, 0b]
  imager.ccd_size= [752, 290]
  imager.ccd_center= [376.0, 145.0]
  imager.pixel_aspect_ratio= 0.518072d0
  SKYMAP_IMAGER_LOAD,imager

  site= {SKYMAP_SITE}
  site.uid='test-site' & site.unix_time=0L
  site.latitude= 45.0
  site.longitude= -123.5
  site.altitude= 1050
  SKYMAP_SITE_LOAD,site

  CDF_EPOCH,epoch,2006,10,17,10,20,30,/COMPUTE
  frame= UINTARR(256,256)
  ccd_offset= [125, 33]
  ccd_binning= [2, 1]
  SKYMAP_IMG_LOAD,epoch,frame,ccd_offset,ccd_binning


  tests=['SKYMAP_TEST_CARTESIAN2SPHERICAL' $
;         ,'SKYMAP_TEST_IMAGER_LOAD' $
         ,'SKYMAP_TEST_LENS2CCD', 'SKYMAP_TEST_CCD2LENS' $
         ,'SKYMAP_TEST_AIM2MAP' $
         ,'SKYMAP_TEST_IMG2CCD' $
         ,'SKYMAP_TEST_GEI2AIM']

  nvectors=999
  FOR indx=0,N_ELEMENTS(tests)-1 DO BEGIN
    testname= tests[indx]
    result= CALL_FUNCTION(testname,nvectors) EQ 0 ? 'PASS' : 'FAIL'
    PRINT,result,testname,FORMAT='(A," : ",A)' & EMPTY
  ENDFOR

RETURN
END



PRO SKYMAP_SHOW_FIT_VECTORS,declination,ascension,epoch
  COMMON SKYMAP_IMAGER

  imager0= imager$
  gei0= [declination, ascension, 1.0d0]
  img= SKYMAP_GEI2IMG(gei0,epoch,/SPHERICAL,/DEGREES)
  gei0= [declination, ascension, 1.0d0]  ;!!FIXME!! function overwriting bad...

  dgei= 3.0*SQRT(SIN(declination*!dtor)) * [-1,1]
  PLOT,gei0[1]+dgei,gei0[0]+dgei,/NODATA,/YNOZERO $
    ,XTITLE='Right Ascension (degrees)',YTITLE='Declination (degrees)'

  FOR indx=0,1 DO BEGIN
    imager1= imager0 & imager1.ccd_center[indx]= imager0.ccd_center[indx]+1
    SKYMAP_IMAGER_LOAD,imager1
    gei1= SKYMAP_IMG2GEI(img,epoch,/SPHERICAL,/DEGREES)
    ARROW,gei0[1],gei0[0],gei1[1],gei1[0],/DATA,THICK=3,/SOLID
  ENDFOR

  FOR indx=0,2 DO BEGIN
    imager1= imager0 & imager1.optical_orientation[indx]= imager0.optical_orientation[indx]+1
    SKYMAP_IMAGER_LOAD,imager1
    gei1= SKYMAP_IMG2GEI(img,epoch,/SPHERICAL,/DEGREES)
    ARROW,gei0[1],gei0[0],gei1[1],gei1[0],/DATA,THICK=2,COLOR=50   ;,/SOLID
  ENDFOR

  FOR indx=1,3,2 DO BEGIN
    imager1= imager0 & imager1.optical_projection[indx]= imager0.optical_projection[indx]+1
    SKYMAP_IMAGER_LOAD,imager1
    gei1= SKYMAP_IMG2GEI(img,epoch,/SPHERICAL,/DEGREES)
    ARROW,gei0[1],gei0[0],gei1[1],gei1[0],/DATA,HTHICK=2,THICK=2,COLOR=150 ;SOLID ;,THICK=2,/SOLID
  ENDFOR


  SKYMAP_IMAGER_LOAD,imager0
RETURN
END


PRO SKYMAP__DEFINE
 dummy=0
RETURN
END

LOADCT,39
!p.color=0 & !p.background=255
!p.multi=[0,1,2]



  SKYMAP_SITE_LOAD,54.72,246.69,676,UID='atha'
;p0= [376.23,141.61, -15.00,0.06,-0.93, 165.82,-4.0]
  imager= {SKYMAP_IMAGER}
  imager.optical_orientation= [-15.0, 0.00, -0.8] ;# yaw, pitch, tilt (degrees)
  ;imager.optical_angle= 170.0/2.0 * !dtor ;# maximum axial angle
  imager.optical_projection[[0,1,2,3,4,5],0]=  [0.0, 165.1, 0.0, -3.98, 0.0, -0.002]
  imager.optical_projection[[0,1,2,3],1]=  [1.0,   0.0, 0.0, 0.0]
  imager.optical_reflection= 0b
  imager.img_flip= [1b,0b]
  imager.ccd_size= [752,290]
  imager.ccd_center=[376.2, 141.6]
  imager.pixel_aspect_ratio= 0.518072d0
  SKYMAP_IMAGER_LOAD,imager

  CDF_EPOCH,epoch,2005,12,30,12,00,00,/COMPUTE
  frame= UINTARR(256,256)
  ccd_offset= [120, 14]
  ccd_binning= [2, 1]
  SKYMAP_IMG_LOAD,epoch,frame,ccd_offset,ccd_binning


  frame=lindgen(65536L)
  aim= SKYMAP_IMG2AIM(frame,/LINEAR,/SPHERICAL,/DEGREES)
  aim=REFORM(aim,256,256,3)


stop

  IF (0) THEN BEGIN
   dir= '\\themis-data\data\themis\imager\stream0\2005\12\30\atha_themis02\ut12\'
   f= FINDFILE(dir+'*full.pgm.gz',COUNT=nf)
   THEMIS_IMAGER_READFILE,f,data,mdata,/ALL_METADATA,COUNT=nframes
   mdata= mdata[nframes/2] & epoch= mdata.exposure_start_cdf

   frame_hour= TOTAL(data,3,/INTEGER)
   IF (nframes GT 20) THEN BEGIN
     frame_hour= frame_hour + TOTAL(data[*,*,nframes-20:*],3,/INTEGER)/20.0 * nframes/2
     nframes= nframes/2*3
   ENDIF
   frame_hour= UINT(frame_hour/nframes)

   SKYMAP_IMG_LOAD,epoch,frame_hour,mdata.ccd_offset,mdata.ccd_binning
  ENDIF

  img= frame_hour-MIN(frame_hour)
 ; img= img - SMOOTH(img,9)*0.8
  img= (SQRT(img)>10 - 10) < 40
  img= bytscl(img)
  img= img + 255*(img EQ 0)

  imgx= INDGEN(256)  ;& imgx= REVERSE(INDGEN(256))
  imgy= INDGEN(256)  ;& REVERSE(INDGEN(256))

  shade_surf,img*0,imgx,imgy,SHADE=img,az=0,ax=90 $
    ,XRANGE=[0,255],YRANGE=[255,0],XSTYLE=5,YSTYLE=1,ZSTYLE=4
  AXIS,0,0,XAXIS=1,XRANGE=!x.crange,XSTYLE=1

  angle=indgen(24)*15 & linestyle= 1-(angle EQ 0)+(angle EQ 90)
  contour,aim[*,*,1],levels=angle,/overplot,/follow,/closed,C_LINESTYLE=linestyle,C_LABEL=''
  contour,aim[*,*,0],levels=indgen(11)*10,/overplot,/follow,/closed,C_CHARSIZE=1.5

  indx=lindgen(65536L)
  radec= SKYMAP_IMG2GEI(indx,epoch,/LINEAR,/SPHERICAL,/DEGREES)
  radec=REFORM(radec,256,256,3)

  shade_surf,img*0,imgx,imgy,SHADE=img,az=0,ax=90 $
    ,XRANGE=[0,255],YRANGE=[255,0],XSTYLE=5,YSTYLE=1,ZSTYLE=4
  AXIS,0,0,XAXIS=1,XRANGE=!x.crange,XSTYLE=1

  angle=indgen(24)*15 & linestyle= 1-(angle EQ 0)+(angle EQ 90)
  contour,radec[*,*,1],levels=angle,/overplot,/follow,/closed,C_LINESTYLE=linestyle,C_LABEL=''
  contour,radec[*,*,0],levels=indgen(19)*10-90,/overplot,/follow,/closed,C_LABEL='',C_LINESTYLE=1,C_COLOR=!p.background

;  contour,radec[*,*,0],level=indgen(19)*10-90,/overplot;,C_LINESTYLE=1
;  contour,radec[*,*,1],level=indgen(12)*30,/overplot,C_LINESTYLE=2

  SKYMAP_STAR_LOAD
  COMMON SKYMAP_STAR,ybsc$,ras$,dec$
  w= WHERE(ybsc$.visual_magnitude LE 2.0,nw) & one= REPLICATE(1.0d,nw)
  tmp= SKYMAP_GEI2IMG([ybsc$[w].declination,ybsc$[w].right_ascension,one],epoch+30*60*1000.0,/spher,/deg)
  plots,tmp[*,0],tmp[*,1],psym=4,symsize=1.9
stop

  IF (1) THEN BEGIN
    dir= '\\themis-data\data\themis\imager\stream0\2005\12\30\atha_themis02\ut12\'
    f= FINDFILE(dir+'*full.pgm.gz',COUNT=nf)
    data=UINTARR(256,256,nf) & epoch= DBLARR(nf)
    FOR indx=0,nf-1 DO BEGIN
      THEMIS_IMAGER_READFILE,f[indx],_data,_mdata,/ALL_METADATA,COUNT=nframes
      data[0,0,indx]= TOTAL(_data,3,/INTEGER)/nf
      epoch[indx]= _mdata[0].exposure_start_cdf + 30*1000.0d0
    ENDFOR
    SKYMAP_IMG_LOAD,epoch,data,mdata.ccd_offset,mdata.ccd_binning
  ENDIF

  dec0= [19.18,   49.31,  61.75, 11.97,44.95]
  ras0= [213.91, 206.89, 165.93, 152.09,89.88]

  dec0= [21.0,   60.0,  52.0,  16.0, 42.0 ]
  ras0= [212.0, 190.0, 202.0, 154.0, 83.0 ]
  skymap_grid_load,dec0,ras0,dangle=9

;skymap_grid_load,60,190,dangle=15
;bright mystery object at dec=19, ras=133?

  SKYMAP_FIT_LOAD_VALUE,'ccd_center',370,0,/clear
  SKYMAP_FIT_LOAD_VALUE,'ccd_center',142,1

  SKYMAP_FIT_LOAD_VALUE,'optical_orientation',-12.0,0
  SKYMAP_FIT_LOAD_VALUE,'optical_orientation',0.0,1
  SKYMAP_FIT_LOAD_VALUE,'optical_orientation',0.0,2

  SKYMAP_FIT_LOAD_VALUE,'optical_projection',166.0,1 ;# a1
  SKYMAP_FIT_LOAD_VALUE,'optical_projection',-4.0,3  ;# a3
  ;SKYMAP_FIT_LOAD_VALUE,'optical_projection',1.0,11 ;# b2

  p0= [376.26,141.59, -15.01,0.07,-0.84, 165.830,-4.0]
  dp= [0.5,0.5, 0.2,0.1,0.1, 0.1,0.01]
  print,skymap_fit_metric1(p0,/show)
;  tmp= amoeba(1.0d-3,FUNCTION_NAME='skymap_fit_metric1',nmax=99,p0=p0,scale=dp)

END

DEFSYSV,'!ddtor',EXISTS=i & IF (NOT i) THEN DEFSYSV,'!ddtor',!dpi/180.0d0


site_info= SITE_INFO_READFILE('c:/tmp/ATHA.20050525')
SKYMAP_SITE_LOAD,site_info

asi_info= imager_info_readfile('c:/tmp/themis00.atha.20060622')
SKYMAP_ASI_LOAD,asi_info

dir= '\\themis-data\data\themis\imager\stream0\2005\12\30\atha_themis02\ut12\'
f= FINDFILE(dir+'*full.pgm.gz',COUNT=nf)

THEMIS_IMAGER_READFILE,f[0],data,mdata,/ALL_METADATA,COUNT=nframes
frame= UINT(TOTAL(data,3)/nframes) & mdata= mdata[nframes/2] & epoch= mdata.exposure_start_cdf
SKYMAP_FRAME_LOAD,epoch,frame,mdata.ccd_offset,mdata.ccd_binning

TVSCL,SQRT(frame)<70

;aim= skymap_gei2aim([90.0,0.0,0.0],epoch,/from_spherical,/into_spherical)  ;point at polaris
;ccd= skymap_aim2ccd(aim,/from_spherical) & plots,ccd[0],ccd[1],psym=4,/device

;aim= skymap_gei2aim([61.8,165.9,0.0],epoch,/from_spherical,/into_spherical)  ;point at polaris
;ccd= skymap_aim2ccd(aim,/from_spherical) & plots,ccd[0],ccd[1],psym=5,/device

;aim= skymap_gei2aim([55.4,165.5,0.0],epoch,/from_spherical,/into_spherical)  ;point at polaris
;ccd= skymap_aim2ccd(aim,/from_spherical) & plots,ccd[0],ccd[1],psym=5,/device

;aim= skymap_gei2aim([49.3,206.9,0.0],epoch,/from_spherical,/into_spherical)  ;point at polaris
;ccd= skymap_aim2ccd(aim,/from_spherical) & plots,ccd[0],ccd[1],psym=5,/device

;aim= skymap_gei2aim([46.0,79.2,0.0],epoch,/from_spherical,/into_spherical)  ;point at polaris
;ccd= skymap_aim2ccd(aim,/from_spherical) & plots,ccd[0],ccd[1],psym=6,/device

stop

frame= UINTARR(256,256,nf)
epoch= DBLARR(nf)
FOR indx=0,nf-1 DO BEGIN
  themis_imager_readfile,f[indx],data,metadata,COUNT=nimg
  frame[0,0,indx]= TOTAL(data,3)/nimg
  epoch[indx]= metadata[nimg/2].exposure_start_cdf
ENDFOR

tvscl,sqrt(total(frame,3)/60) < 68  ;show star trails

skymap_frame_load,epoch,frame,mdata.ccd_offset,mdata.ccd_binning

 nx=25 & ny= 30
 ras0= [213.91,206.89,165.93,152.09,89.88]
 dec0= [19.18,49.31,61.75,11.97,44.95]

 nx=80 & ny= 90
 dec0=90.0 & ras0= 0.0
; grid=SKYMAP_grid(dec0,ras0,gridnx=nx,gridny=ny,dangle=7)
 test= SKYMAP_FRAME2GRID(grid)
 ave= TOTAL(test,4)/nframes  &  lo= MIN(test,DIM=4)
END


CDF_EPOCH,epoch,2006,06,20,00,24,40,/COMPUTE
PRINT,SKYMAP_SUN_AIM(epoch)
PRINT,[90.0,360,0] + [-1,1,0]* SKYMAP_SUN_AIM(epoch,/spherical)

PRINT,SKYMAP_MAP2AIM([54.72, 246.72, 676.0+100.0],/INTO_SPHERICAL)

SKYMAP_MAP__DEFINE
SKYMAP_MAP_SET,50,-120,-30,/HAMMER,/GRID,LIMIT=[45,-140,70,-80],/CONTINENT
map=window$ & map[*,2]= 110.0d3
aim= skymap_map2aim(map,/into_spherical)
zang= 90.0 - REFORM(aim[*,0]) & zang= reform(zang,!d.x_size,!d.y_size) &help,zang

z= REFORM(aim[*,0], !d.x_size, !d.y_size)
x= REFORM(map[*,0], !d.x_size, !d.y_size)
y= REFORM(map[*,1], !d.x_size, !d.y_size)
contour,z,y,x,/overplot,level=[5,10,15,30,45,90],c_color=[250,210, 128,50,!p.color]
;w= where(aim[*,0] GE 0.0) & y= map[w,0] & x=map[w,1] & z=aim[w,0]
;contour,z,x,y,/irreg,level=[45.0, 60.0, 80.0, 85.0],/OVERPLOT



END


SKYMAP_grid_load,57.0,195.0,gridnx=150,gridny=170,dangle=20
SKYMAP_grid_load,dec0,ras0,gridnx=nx,gridny=ny,dangle=7
test= SKYMAP_IMG2GRID()
tmp=total(test,4)/60 & print,symbol_entropy(BYTE(SQRT(tmp))) & tvscl,sqrt(tmp)
shade_surf,tmp,shade= bytscl(-grid.yalebrightstar_angle)



 ;zoom in on Beta Ursi Minor and friend
 ;
SKYMAP_grid_load,73,226,gridnx=60,gridny=50,dangle=4



;PRO SKYMAP__DEFINE
;  COMMON SKYMAP,dtor$,geoid$,epoch2000$
;
;  dtor$= !dpi/180.0d0
;
;;  geoid$= {SKYMAP_GEOID, a:0.0d0, b:0.0d0, f:0.0d0, e:0.0d0}
;;  geoid$.a= 6378.137000d3 ;semi-major axis (from Trimble Acutime 2000)
;;  geoid$.b= 6356.752d3 ;semi-minor axis
;;  geoid$.f= 1.0d0/298.257223563d0
;;  geoid$.e2= 0.00669437999014d0  ;eccentricity squared (from Trimble Acutime 2000)
;
;  CDF_EPOCH,epoch2000$,2000,1,1,12,/COMPUTE
;
;RETURN
;END;-----------------------------------------------------------------

p0= [376.26,141.59, -15.01,0.07,-0.84, 165.830,-4.0, 0.0]


p0=[165.0, -3.8, 0.0] & dp=[0.1,0.1,0.01]
SKYMAP_FIT_LOAD_VALUE,'optical_projection',p0[0],1,/clear
SKYMAP_FIT_LOAD_VALUE,'optical_projection',p0[1],3
SKYMAP_FIT_LOAD_VALUE,'optical_projection',p0[3],5
skymap_grid_load,07,89,dangle=4 & print,skymap_fit_metric1(p0,/show)
tmp= amoeba(1.0d-4,FUNCTION_NAME='skymap_fit_metric1',nmax=49,p0=p0,scale=dp)



p0=[-15.0, 0.0, -1.0] & dp=[0.1,0.1,0.1]
SKYMAP_FIT_LOAD_VALUE,'optical_orientation',p0[0],/clear
SKYMAP_FIT_LOAD_VALUE,'optical_orientation',p0[1],1
SKYMAP_FIT_LOAD_VALUE,'optical_orientation',p0[2],2
skymap_grid_load,07,89,dangle=4 & print,skymap_fit_metric1(p0,/show)
tmp= amoeba(1.0d-4,FUNCTION_NAME='skymap_fit_metric1',nmax=49,p0=p0,scale=dp)

p0=[376.5, 141.5] & dp=[0.2,0.1]
SKYMAP_FIT_LOAD_VALUE,'ccd_center',p0[0],0,/clear
SKYMAP_FIT_LOAD_VALUE,'ccd_center',p0[1],1
skymap_grid_load,07,89,dangle=4 & print,skymap_fit_metric1(p0,/show)

tmp= amoeba(1.0d-4,FUNCTION_NAME='skymap_fit_metric1',nmax=49,p0=p0,scale=dp)

END