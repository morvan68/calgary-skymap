;+
; NAME:     GEODETIC_CONVERT
;
; PURPOSE:  To convert between geodetic, geocentric, and Cartesian coordinate systems.
;     One use might be to find the true distance between two points
;     given the geodetic coordinates: convert to Cartesian
;     and find the difference.
;
; CATEGORY: Coordinate systems, Geophysics
;
; CALLING SEQUENCE: result= GEODETIC_CONVERT(Location,[old2new])
;
; INPUTS:   Location  position in the original system,
;                  either a 3 element vector
;                  or a Nx3 element array
;
; KEYWORDS: FROM_GEODETIC      If one of these keywords is set, it determines what
;           FROM_GEOCENTRIC    the initial coordinate system is.  An error message
;           FROM_CARTESIAN     will be given if none of them are set, or if more than
;                                  one is set.
;
;           TO_GEODETIC        Set one of these keywords to determine the final
;           TO_GEOCENTRIC      coordinate system.
;           TO_CARTESIAN
;
;           GEODETIC_REFERENCE If set, this keyword must be a string containing the
;                              name of the geodetic reference spheroid to use.
;                              Available options are:
;
;                              "WGS 84"          used for GPS satellite positions, default
;                              "IAU 76"          astronomy standard
;                              "IAU 64"          old value
;
;         HELP            Returns this help documentation
;
;
; OUTPUTS:  result     3 (or Nx3 )-element vector of
;                  coordinates in new system
;
; OPTIONAL OUTPUTS:
;   old2new     3x3 conversion matrix from old to new
;      system at the input position.
;
;   Available ONLY for    Geodetic   ==> Cartesian
;          Geocentric ==> Cartesian
;   however, this is enough to transform vectors between all 3 systems
;   ie.  geod= INVERT(geod2cart) # ((geoc2cart)#geoc)
;
;   and ONLY if position is a 3 element vector (ie. not 3 x n)
;
; EXAMPLE:  geod= [0.200, 45.0, -10.0]
;         geoc= GEODETIC_CONVERT(geod,/FROM_GEODETIC,/TO_GEOCENTRIC)
;
; PROCEDURE:
;   Standard equations (ie. Heiskanen & Moritz, Astronomical Almanac)
;   are used to convert between the three coordinate systems:
;
;   GEODETIC
;       lat geodetic latitude (degrees North)
;       lon geodetic longitude (degrees East)
;       h   height above mean sea level (MSL) in metres
;
;   GEOGRAPHIC
;       lat geocentric latitude (degrees North)
;       lon geocentric longitude (degrees East)
;       r   distance from the centre of the earth in metres
;
;   CARTESIAN   [x,y,z] with [0,0,0] at the center of the earth
;                       and z along rotation axis, in metres
;
;NOTE: Geodetic coordinates assume an ellipsoidal earth, where Geocentric
;      coordinates assume a spherical earth.
;
; MODIFICATION HISTORY: Brian Jackel    March 20 1992
;      University of Western Ontario
;
;   Bjj   March 1 1993  - Added the conversion matrix option (to allow testing of IGRF code).
;   Bjj   April 19 1993 - Re-arranged the calling parameters, added the HELP keyword.
;   Bjj   November 5 1995 - Re-arranged the calling parameters again, added the HAYFORD_GEOID option.
;   Bjj   2010-06-22  Fixed bug found by Phil McGeachy
;-

;Cartesian (x,y,z)
;Geocentric (r,phi_prime,lambda)
;Geodetic (h,phi,lambda)

function GEODETIC_CONVERT,position,Old2new, HELP=help, DEBUG=debug     $
         ,FROM_GEODETIC=from_geodetic, TO_GEODETIC=to_geodetic      $
         ,FROM_GEOCENTRIC=from_geocentric, TO_GEOCENTRIC=to_geocentric    $
         ,FROM_CARTESIAN=from_cartesian, TO_CARTESIAN=to_cartesian       $
         ,REFERENCE_GEOID=reference_geoid


  IF KEYWORD_SET(DEBUG) THEN ON_ERROR,0 ELSE ON_ERROR,2
  ON_ERROR,0

  IF KEYWORD_SET(HELP) THEN BEGIN
    DOC_LIBRARY,'Geodetic_Convert'
    RETURN, [0,0,0]
  END


  ;Screen the input
  ;
  IF (N_PARAMS() LT 1) THEN MESSAGE,'At least one input parameter (position) required'

  errstring= 'Error- input position must be a 3 element array or N x 3 element matrix'
  siz= SIZE(position,/STRUCT)
  IF (siz.n_elements LT 3) THEN MESSAGE,errstring
  IF ((siz.n_elements MOD 3) NE 0) THEN MESSAGE,errstring
  IF (siz.dimensions[siz.n_dimensions-1] NE 3) THEN MESSAGE,errstring

  nvec= siz.n_elements / 3
  vec= REFORM(position,nvec,3)


  ;Determine the reference spheroid parameters
  ;
  IF KEYWORD_SET(REFERENCE_GEOID) THEN BEGIN
     geoid= STRUPCASE(reference_geoid)
     geoid= STRCOMPRESS(geoid,/REMOVE_ALL)
  ENDIF ELSE geoid='WGS84'        ;default

  CASE geoid OF
     'IAU64':BEGIN & a=6378160.0d0 & f1= 298.250d0  & END
     'IAU76':BEGIN & a=6378140.0d0 & f1= 298.2570d0 & END
     'WGS84':BEGIN & a=6378137.0d0 & f1= 298.257223563d0 & END
     ELSE:BEGIN
            MESSAGE,'Warning- unrecognized REFERENCE_GEOID, using WGS 1984',/INFORMATIONAL
            a=6378137.0d0 & f1= 298.257223563d0
          END
  ENDCASE

  f= 1.0d0/f1            ;flattening of the spheroid = (a-b)/a
  e2= f*(2.0d0-f)        ;eccentricity of the ellipsoid, e^2= (a^2 - b^2) / a^2
  eminus= (1.0d0 - f)^2  ;1-e^2 = (1-f)^2

  deg2rad= !dpi/180.0d0
  rad2deg= 180.0d0/!dpi


  ;Use a case statement to get all the different options.  Note that
  ;we shouldn't need ELSE statements, as the input has been carefully checked...
  ;
  IF KEYWORD_SET(FROM_GEODETIC) THEN BEGIN

     phi= REFORM( vec[*,0] ) * deg2rad     ;geodetic latitude
     h= REFORM( vec[*,2] )                 ;geodetic height
     Nphi= a / SQRT( 1.0d0 - e2*SIN(phi)^2 )    ;East-West curvature of the ellipsoid, aC in IAU notation

     IF KEYWORD_SET(TO_GEOCENTRIC) THEN BEGIN
       vec[0,0]= ATAN( (eminus*Nphi+h)/(Nphi+h) * TAN(phi) )  ;geocentric latitude "phi prime"
       vec[0,2]= (Nphi+h) * COS(phi)/COS(vec[*,0])            ;radius "rho"
       vec[0,0]= vec[*,0] * rad2deg
     ENDIF

     IF KEYWORD_SET(TO_CARTESIAN) THEN BEGIN
        lambda= REFORM( vec[*,1] ) * deg2rad
        cphi= COS(phi)  &  sphi= SIN(phi)
        vec[0,0]= (Nphi+h) * cphi * cos(lambda)
        vec[0,1]= (Nphi+h) * cphi * sin(lambda)
        vec[0,2]= (eminus*Nphi+h) * sphi
        IF (n_params() EQ 2) THEN BEGIN
             slam= sin(lambda)  &  clam= cos(lambda)
             east= [-slam, clam, 0.0]
             zenith= [cphi*clam, cphi*slam, sphi ]
             north= [-clam*sphi, -slam*sphi, cphi]
        ;     old2new= [ [-zenith],[north],[east] ]
             old2new= TRANSPOSE(REFORM([north,east,-zenith],3,3))
        END
     ENDIF

  ENDIF


  IF KEYWORD_SET(FROM_GEOCENTRIC) THEN BEGIN
     phi_prime= REFORM(vec[*,0]) * deg2rad
     lambda= REFORM(vec[*,1]) * deg2rad
     rho= REFORM(vec[*,2])
     cphi= COS(phi_prime)  &   sphi= SIN(phi_prime)

     IF KEYWORD_SET(TO_GEODETIC) THEN BEGIN
       r= rho*cphi  &  z= rho*sphi

       phi= ATAN(z/r)
       FOR indx=0,3 DO BEGIN        ;Enough for sub-metre accuracy
         sin_phi= SIN(phi)
         Nphi= a / SQRT(1.0d0 - e2*sin_phi^2)
         phi= ATAN( (z + Nphi*e2*sin_phi) / r )
       ENDFOR

       h= r/COS(phi) - Nphi
;       results=[ [phi*rad2deg], [lambda*rad2deg], [h] ]
;       vec= [ [phi*rad2deg], [lambda*rad2deg], [h] ]
       vec[0,0]= phi*rad2deg
       vec[0,1]= lambda*rad2deg
       vec[0,2]= h

     ENDIF

     IF KEYWORD_SET(TO_CARTESIAN) THEN BEGIN
         x= rho * cphi * cos(lambda)
         y= rho * cphi * sin(lambda)
         z= rho * sphi
         results=[ [x] ,[y], [z] ]
         IF (n_params() EQ 2) THEN BEGIN
;         IF (n_params() EQ 2) AND (nel EQ 3) THEN BEGIN
              slam= sin(lambda)  &  clam= cos(lambda)
              east= [-slam,clam,0.0]
              zenith= [cphi*clam,cphi*slam,sphi]
              north= [-clam*sphi,-slam*sphi,cphi]
              ;old2new= TRANSPOSE([ -[zenith],[north],[east] ])
              old2new= TRANSPOSE([ [north], [east], -[zenith] ])
         ENDIF
     ENDIF

  ENDIF


  IF KEYWORD_SET(FROM_CARTESIAN) THEN BEGIN
    x= REFORM(vec[*,0])  &  y= REFORM(vec[*,1])  &  z= REFORM(vec[*,2])

    IF KEYWORD_SET(TO_GEOCENTRIC) THEN BEGIN
      vec[0,2]= SQRT(x^2 + y^2 + z^2) 	;# radius "rho"
      vec[0,1]= ATAN(y,x) * rad2deg 	;# longitude "lambda"
      vec[0,0]= ASIN(z/vec[*,2]) * rad2deg  ;# latitude "phi_prime"
      ;results=[ [phi_prime*rad2deg], [lambda*rad2deg], [rho] ]
    ENDIF

    IF KEYWORD_SET(TO_GEODETIC) THEN BEGIN

      r= SQRT(x^2 + y^2)  &  phi= ATAN(z/r)
      FOR indx=0,3 DO BEGIN        ;Enough for sub-metre accuracy
        sin_phi= SIN(phi)
        Nphi= a/SQRT(1.0d0 - e2*sin_phi^2)
        phi= ATAN( (z + Nphi*e2*sin_phi) / r )
      ENDFOR

      vec[0,0]= phi * rad2deg  ;# latitude
      vec[0,1]= ATAN(y,x) * rad2deg  ;# longitude "lambda"
      vec[0,2]= r/COS(phi) - Nphi ;a/SQRT(1.0d0 - e2*SIN(phi)^2)  ;# height
      ;results=[ [phi*rad2deg], [lambda*rad2deg], [h] ]
    ENDIF

  ENDIF

;  IF (N_ELEMENTS(vec) EQ 0) THEN BEGIN
;     MESSAGE,'Warning- no coordinate conversion carried out.'
;     RETURN,vec
;  ENDIF ELSE RETURN,REFORM(vec)  ;would like to exit with Nx3 shape?

  RETURN,REFORM(vec,siz.dimensions[0:siz.n_dimensions-1])

  END


  FUNCTION GEODETIC_CONVERT_UTIL,z,r

    phi= ATAN(z/r)
    FOR indx=0,5 DO BEGIN        ;Enough for sub-metre accuracy
      sin_phi= SIN(phi)
      Nphi= a / SQRT(1.0d0 - e2*sin_phi^2)
      phi= ATAN( (z + Nphi*e2*sin_phi) / r )
    ENDFOR
    h= r/COS(phi) - Nphi

  RETURN,[h,phi]
  END

;pro GEODESY_TEST,location
;
;into= 'Cartesian'
;FOR i=0,1 DO BEGIN
;  IF (i EQ 0) THEN ffrom='Geodetic' ELSE ffrom='Geocentric'
;
;  x= GEODESY(location,ffrom,'Cartesian',old2new)
;
;;note that the first direction is "down"
;  x0= GEODESY(location + [-1.0,0.0,0.0] ,ffrom,into) - x
;  x1= GEODESY(location + [0.0,1.0,0.0] ,ffrom,into) - x
;  x2= GEODESY(location + [0.0,0.0,1.0] ,ffrom,into) - x
;
;  x0= x0 / SQRT(TOTAL(x0^2))
;  x1= x1 / SQRT(TOTAL(x1^2))
;  x2= x2 / SQRT(TOTAL(x2^2))
;
;  temp= [ [x0], [x1], [x2] ]
;  print,temp,old2new   ;these should be the same
;  print,' '
;  location= location + [6371.2,0.0,0.0]  ;change into geocentric
;ENDFOR
;
;
;  return
;END

geod= [37.5, 52.0, 105.0d3]
test=GEODETIC_CONVERT(geod,/FROM_GEODETIC,/TO_CARTESIAN) & PRINT,test
PRINT,GEODETIC_CONVERT(test,/FROM_CARTESIAN,/TO_GEODETIC)
;
test=GEODETIC_CONVERT(geod,/FROM_GEODETIC,/TO_GEOCENTRIC)
PRINT,GEODETIC_CONVERT(test,/FROM_GEOCENTRIC,/TO_GEODETIC)
;
test2= GEODETIC_CONVERT(test,/FROM_GEOCENTRIC,/TO_CARTESIAN)
test3= GEODETIC_CONVERT(test2,/FROM_CARTESIAN,/TO_GEOCENTRIC)
PRINT,GEODETIC_CONVERT(test3,/FROM_GEOCENTRIC,/TO_GEODETIC)
END

