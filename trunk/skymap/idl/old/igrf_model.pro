;+
; NAME:         IGRF_MODEL
;
; PURPOSE:      This routine provides values of the earth's magnetic field as
;               predicted by the International Geomagnetic Reference Field model.
;
; CATEGORY:     Geomagnetic Modelling
;
; CALLING SEQUENCE:     b= IGRF_MODEL( position )
;
; INPUTS:    position    a 3 element vector containing [altitude,latitude,longitude]
;                        in either geodetic or geocentric coordinates.  Default is
;                        geodetic, use the keyword /GEOCENTRIC to change.
;
;                         altitude     metres above Mean Sea Level (MSL)
;                         latitude     geodetic latitude in degrees
;                         longitude    geodetic longitude in degrees East
;
;
; KEYWORD PARAMETERS:
;
;     YEAR    model year to use.  IGRF coefficients are available
;             from 1945 to 2005 (extrapolated) at 5 year intervals.
;             Linear interpolation is used for years that aren't a
;             multiple of 5.  If YEAR is not provided the previous
;             value will be used; if there was no previous value
;             than the current year (as provided by SYSTIME) will be
;             used.
;
;     GEOCENTRIC  if this keyword is set, then the input position must
;                 be given in geocentric coordinates
;
;                         altitude     metres from the center of the earth
;                         latitude     geocentric latitude in degrees
;                         longitude    geocentric longitude in degrees East
;
;                 which assume a spherical earth of radius 6371.2e3 metres.
;                 Output values for the field will also be in geocentric
;                 coordinates.   Setting the /GEOCENTRIC keyword will produce
;                 a slight increase in speed, and may be more convenient for
;                 converting to other coordinate systems.
;
;
;     DATAFILE_DIRECTORY  where to look for the IGRF coefficient data files
;                         (of the form igrfxxxx.dat).  Defaults to the "resource"
;                         subdirectory of IDL.
;
;     DEGREE  degree of model (default is usually 10).  Leave this alone, unless
;             trying to test the effects of truncating coefficients.
;
;     HELP    if set, will use DOC_LIBRARY to return this information
;
; OUTPUTS:             B       the magnetic field vector (nanoTesla) in geodetic
;                              or geocentric coordinates.
;
; COMMON BLOCKS:        IGRF_MODEL,model_coefficients,recursion_coefficients
;
;                       This common block contains information about the model coefficients,
;                       which are read by IGRF_INITIALIZE from standard IGRF data file.
;                       This is done once, the first time IGRF_MODEL is called.  Some
;                       recursion coefficients (for the Legendre functions) are also
;                       stored here to speed up calculations.
;
; SIDE EFFECTS:         Will (must) read IGRF data files.
;
; PROCEDURE:    Evaluate the Earth's magnetic field as approximated by an
;               expansion in spherical harmonics, where the coefficients
;               are the result of a least squares fit to a large amount of data.
;
;       See     - Langel, R.A., Main Field, Chapter Four in GEOMAGNETISM,
;                       ed. J.A. Jacobs, Academic Press, London, 1987
;               - Langel, R.A., IGRF: 1991 Revision
;                       in EOS Vol 73, #16, April 21 1992
;
;EXAMPLE:       Given a known geodetic position (g), evaluate the magnetic
;                field given by IGRF90
;  IDL> g= [400, 47.0, -10.0]          ;400 metres above sea level, 47 degrees latitude, 10 degrees west longitude
;  IDL> b=igrf_model(g,year=1990)
;  IDL> print,b
;       41447.259       21316.903      -2926.8202
;
;where http://nssdc.gsfc.nasa.gov/space/model/models/igrf.html gives
;
;       0.21317   -.02927   0.41447    ([north,east,down] in Tesla)
;       
;       http://www.ngdc.noaa.gov/IAGA/vmod/igrf11coeffs.txt
;
; MODIFICATION HISTORY:
;
;     Brian Jackel June 1992          Written, sort of documented
;     University of Western Ontario
;
;     Bjj  April 1993       Placed coefficients in a common block, changed
;                                year from a keyword to a parameter.
;
;     Bjj  February 1995   Changed how coefficients were stored internally
;     Bjj  April 1995      Improved comments
;     Bjj  1997/01/31      Brought documentation up to date.
;     Bjj  1998/01/03      Cleaned up code, sped up recursion.
;     Bjj  2006/06/17      Modified to read single coefficient file, fixed for degree 13
;-


;=====================================================================================
PRO IGRF_MODEL_COEFFICIENTS__DEFINE
  n= 15 + 1; maximum degree, currently only 13th for 2005+
  template= {IGRF_MODEL_COEFFICIENTS, year:0, degree:0, g:FLTARR(n,n), h:FLTARR(n,n) }
  template= {IGRF_RECURSION_COEFFICIENTS, a:DBLARR(n), b:DBLARR(n,n), c:DBLARR(n,n) }
RETURN
END;--------------------IGRF_MODEL_COEFFICIENTS__DEFINE------------------------


PRO IGRF_MODEL_READFILE,datafile_directory

 COMMON IGRF_MODEL,model_coefficients,recursion_coefficients

 f= 'c:/data/download/igrf11coeffs.csv'
 OPENR,lun,f,/GET_LUN

 line= ''
 ;# should skip comments ("#" signs)
 READF,lun,line ;# g/h,n,m,1900,1905...2000,2005,sv
 parts= STRSPLIT(line,',',/EXTRACT,/PRESERVE_NULL,COUNT=nparts)
 years= parts[3:nparts-1]  & nyears= N_ELEMENTS(years)
 model_coefficients= REPLICATE( {IGRF_MODEL_COEFFICIENTS} ,nyears+1 )
 model_coefficients[1:nyears-1].year= years[0:nyears-2]
 model_coefficients.degree=0

 ON_IOERROR,done
 WHILE NOT EOF(lun) DO BEGIN
   READF,lun,line
   parts= STRSPLIT(line,',',/EXTRACT,/PRESERVE_NULL,COUNT=nparts)
   n= parts[1] & m= parts[2]

   CASE parts[0] OF
   'g':FOR indx=3,nparts-1 DO model_coefficients[indx-2].g[n,m]= parts[indx]
   'h':FOR indx=3,nparts-1 DO model_coefficients[indx-2].h[n,m]= parts[indx]
   ELSE:
   ENDCASE

   w= WHERE(parts[3:*] NE '',nw) ;& print,w & IF (nw LT 22) THEN stop
   IF (nw GT 0) THEN model_coefficients[w+1].degree= n; max degree so far
 ENDWHILE
 done:FREE_LUN,lun

 model_coefficients[nyears].year= model_coefficients[nyears-1].year + 5
 model_coefficients[nyears].g= model_coefficients[nyears-1].g + 5*model_coefficients[nyears].g
 model_coefficients[nyears].h= model_coefficients[nyears-1].h + 5*model_coefficients[nyears].h

RETURN
END

PRO IGRF_MODEL_READFILE_OLD,datafile_directory

 COMMON IGRF_MODEL,model_coefficients,recursion_coefficients

  filelist= FINDFILE(datafile_directory+'igrf*.dat')
  IF (STRLEN(filelist(0)) EQ 0) THEN MESSAGE,'No files of the form igrf*.dat found in '+datafile_directory

  n_years= N_ELEMENTS(filelist) ;the number of igrf coefficient files

  model_coefficients= REPLICATE( {IGRF_MODEL_COEFFICIENTS} ,n_years+1 )

  label= " "
  degree= 0
  re= 0.0
  year= 0.0
  FOR indx=1,n_years DO BEGIN
   OPENR,lun,filelist(indx-1),/GET_LUN
   READF,lun,label
   READF,lun,degree,re,year
   model_coefficients(indx).degree= degree
   model_coefficients(indx).year= year
   ncoefficients= TOTAL( INDGEN(degree)+2 )
   datarray= FLTARR(4,ncoefficients)
   READF,lun,datarray
   n= REFORM(datarray(0,*))
   m= REFORM(datarray(1,*))
   g= REFORM(datarray(2,*))
   h= REFORM(datarray(3,*))
   FOR jndx=0,ncoefficients-1 DO BEGIN
     model_coefficients(indx).g(n(jndx),m(jndx)) = g(jndx)
     model_coefficients(indx).h(n(jndx),m(jndx)) = h(jndx)
   ENDFOR

   FREE_LUN,lun
  ENDFOR    ;indx loop (# of files)

RETURN
END


;=====================================================================================
;This procedure reads in all the IGRF data files from the directory
;given in "datafile_directory".  It also calculates some coefficients
;for recursion relations of Legendre functions.
;
PRO IGRF_INITIALIZE,datafile_directory

 COMMON IGRF_MODEL,model_coefficients,recursion_coefficients

 IGRF_MODEL_READFILE,datafile_directory

;This is a good place to calculate the recursion coefficients
;that will be required later
;
 degree= MAX(model_coefficients.degree)
 recursion_coefficients= {IGRF_RECURSION_COEFFICIENTS}

 recursion_coefficients.a(0:1)= 1.0d0
 FOR indx=2,degree DO BEGIN
   alpha= SQRT(DOUBLE(2*indx-1)/(2*indx)) ; & stop
   recursion_coefficients.a(indx)= recursion_coefficients.a(indx-1)*alpha
 ENDFOR

 FOR n=1,degree DO BEGIN
   m= INDGEN(n)
   temp= SQRT(DOUBLE(n^2 - m^2))
   recursion_coefficients.b(n,0:n-1)= DOUBLE(2*n-1)/temp
   recursion_coefficients.c(n,0:n-1)= SQRT(DOUBLE((n-1)^2 - m^2))/temp
 ENDFOR

RETURN
END
;-------------------- End of IGRF_INITIALIZE------------------------



;=====================================================================================
PRO IGRF_INITIALIZE_OLD,datafile_directory
;

  COMMON IGRF_MODEL,model_coefficients,recursion_coefficients

  ;template= {IGRF_MODEL_COEFFICIENTS, year:0, degree:0, g:FLTARR(11,11), h:FLTARR(11,11) }
  ;template= {IGRF_RECURSION_COEFFICIENTS, a:FLTARR(11), b:FLTARR(11,11), c:FLTARR(11,11) }

;
;This is a good place to calculate the recursion coefficients
;that will be required later
;
 degree= MAX(model_coefficients.degree)
 recursion_coefficients= {IGRF_RECURSION_COEFFICIENTS}

 recursion_coefficients.a(0:1)= 1.0d0
 FOR indx=2.0d0,degree DO BEGIN
   alpha= SQRT((2*indx-1)/(2*indx))
   recursion_coefficients.a(indx)= recursion_coefficients.a(indx-1)*alpha
 ENDFOR

 FOR n=1.0d0,degree DO BEGIN
   m= DINDGEN(n)
   temp= SQRT(n^2 - m^2)
   recursion_coefficients.b(n,0:n-1)= (2*n-1)/temp
   recursion_coefficients.c(n,0:n-1)= SQRT((n-1)^2 - m^2)/temp
 ENDFOR

RETURN
END
;-------------------- End of IGRF_INITIALIZE------------------------


;=====================================================================================
PRO IGRF_SELECT_COEFFICIENTS,year

;This procedure calculates the IGRF coefficients for a given year
;by linear interpolation of the coefficients given every 5 years.
;The results are placed into the zeroth element of MODEL_COEFFICIENTS

 COMMON IGRF_MODEL,model_coefficients,recursion_coefficients

  all_years= model_coefficients(1:*).year

  min_year= MIN(all_years,MAX=max_year)
  IF (year GT max_year) THEN MESSAGE,'Warning- year is too large, clipping '+STRING(year),/INFORMATIONAL
  IF (year LT min_year) THEN MESSAGE,'Warning- year is too small, clipping'+STRING(year),/INFORMATIONAL
  year= (year < max_year) > min_year   ;clip to valid range

  w= WHERE( all_years EQ year, count)

  IF (count EQ 0) THEN BEGIN
    lo_year= (year - (year MOD 5)) > min_year
    lo_indx= WHERE(all_years LE lo_year)
    lo_year= MAX(all_years(lo_indx),MATCH)
    lo_indx= lo_indx(match) + 1

    hi_year= (lo_year + 5) < max_year         ;next coefficients may be 5 OR MORE years later
    hi_indx= WHERE(all_years GE hi_year)
    hi_year= MIN(all_years(hi_indx),match)
    hi_indx= hi_indx(match) + 1

    del_years= (year - lo_year) / FLOAT(hi_year-lo_year)
    model_coefficients(0).g= (1.0d0-del_years)*model_coefficients(lo_indx).g + del_years*model_coefficients(hi_indx).g
    model_coefficients(0).h= (1.0d0-del_years)*model_coefficients(lo_indx).h + del_years*model_coefficients(hi_indx).h
    model_coefficients(0).degree= MIN( model_coefficients([lo_indx,hi_indx]).degree )
    model_coefficients(0).year= year
  ENDIF ELSE model_coefficients(0)= model_coefficients(w(0)+1)

RETURN
END
;--------------------------------------------------------------------------------------------


;=====================================================================================
FUNCTION IGRF_MODEL,position,YEAR=year,DEGREE=degree,GEOCENTRIC=geocentric,CARTESIAN=cartesian, $
                            HELP=help,DATAFILE_DIRECTORY=datafile_directory,INIT=init

  IF KEYWORD_SET(HELP) THEN BEGIN
          DOC_LIBRARY,'IGRF_MODEL'
          return,[0,0,0]
  ENDIF

;Set up the common block, initialize it if necessary.  Note that if
;a "datafile_path" is not provided, we'll look for the IGRF data files
;in the "resource" subdirectory of the IDL distribution.
;
  COMMON IGRF_MODEL,model_coefficients,recursion_coefficients

  datafile_path= '/zippy/user/bjackel/idl/data/' ;'resource'
  IF (N_ELEMENTS(model_coefficients) EQ 0) OR KEYWORD_SET(INIT) THEN BEGIN
     IF KEYWORD_SET(DATAFILE_PATH) THEN datafile_directory= datafile_path $
                                   ELSE datafile_directory= FILEPATH('',SUBDIR='resource')
  ;   datafile_directory= '/zippy/user/bjackel/uwo/idl/lib/data/'  ;uncomment and edit this line to use some other hard-coded datafile directory
 datafile_directory= 'c:\user\idl\data\'
     IGRF_INITIALIZE,datafile_directory

     IF NOT KEYWORD_SET(YEAR) THEN BEGIN  ;if no year provided, default to the current date
       date= SYSTIME(0)
       year= FLOAT( STRMID( date, STRLEN(date)-4,4 ) )
       IGRF_SELECT_COEFFICIENTS,year(0)
     ENDIF

  ENDIF


;Check the input
;
  IF (n_params() LT 1) THEN MESSAGE,'Position input required'
  IF n_elements(position) NE 3 THEN MESSAGE,'Input must be a 3-vector (alt,lat,lon)'
  IF KEYWORD_SET(YEAR) THEN $
    IF (year(0) NE model_coefficients(0).year) THEN IGRF_SELECT_COEFFICIENTS,year(0)

  IF KEYWORD_SET(DEGREE) THEN degree=(degree(0)>1)<15 $
     ELSE degree=model_coefficients[0].degree



;The IGRF is given in terms of spherical coordinates, so start by converting
;the input position vector from Geodetic to Geocentric coordinates,
;using the IAU 1967 reference ellipsoid.  Useful constants:
;  a= 6378.160      ;equatorial radius in km
;  b= 6356.775      ;polar radius in km
;  f= 1.0/298.25    ;flattening of the spheroid, should be = (a-b)/a

; New suggestion is WGS-84
;  a= 6378.137      ;equatorial radius in km
;  b= 6356.752      ;polar radius in km
;  f= 1.0/298.25722    ;flattening of the spheroid, should be = (a-b)/a

Re= 6371.20d3      ;mean Earth Radius in metres

IF KEYWORD_SET(GEOCENTRIC) THEN BEGIN
  r= position(0)           ;distance from center of the earth in metres
  theta= !dpi/2.0d0 - position(1)*!dtor  ;geocentric co-latitude in radians
  phi= position(2)*!dtor   ;geocentric longitude in radians (same as geodetic)
ENDIF ELSE IF KEYWORD_SET(CARTESIAN) THEN BEGIN
  temp= GEODETIC_CONVERT(position,/FROM_CARTESIAN,/TO_GEOCENTRIC)
  r= temp(0)
  betaa= temp(1)*!dtor
  theta= !dpi/2.0d0 - betaa
  phi= temp(2)*!dtor
ENDIF ELSE BEGIN

 ; IAU-67
 ; a2= 40680925.0d6   ;a^2
 ; b2= 40408585.0d6   ;b^2

 ; WGS-84
  a2= 40680631.6d6   ;a^2
  b2= 40408296.0d6   ;b^2

  alt= position(0)
  lat= ( -89.9999 > position(1) ) < 89.9999  ;avoid singularity at the pole
  lon= position(2)
  hh= alt               ;distance above Mean Sea Level in m
  alpha= lat*!dtor      ;geodetic latitude in radians
  phi= lon*!dtor        ;geodetic/geocentric longitude in radians
  Calpha= cos(alpha)  &  Salpha=sin(alpha)
  N= a2 / SQRT( a2 * Calpha^2 + b2 * Salpha^2 )
  betaa= atan( (b2/a2*N+hh)/(N+hh) * (Salpha/Calpha) )   ;Geocentric Latitude
  theta= !dpi/2.0d0 - betaa                                 ;Geocentric co-latitude
  r= (N+hh) * Calpha / cos(betaa)                        ;Distance from the centre of the earth, metres
ENDELSE

;*************************************************************************
;Make the Legendre Polynomials (P) and their derivatives (dP).
;We directly calculate a few elements, then use a recursion relation
;to fill in the diagonal.  After that we use another recursion relation
;for each row.
;All this recursion is slow in IDL.  Any ideas for improvement?
;*************************************************************************
 stheta= DOUBLE(SIN(theta))
 ctheta= DOUBLE(COS(theta))
 order= degree

 p= DBLARR(degree+1,degree+1)
 dp= DBLARR(degree+1,degree+1)
 diag= INDGEN(degree+1)*(degree+2)

 _a_ = recursion_coefficients.a[0:degree]
 _b_ = recursion_coefficients.b[0:degree,0:degree]
 _c_ = recursion_coefficients.c[0:degree,0:degree]

;The main diagonal P(n,n) is easy to calculate, as are the
;derivatives along the main diagonal.
;
 n= INDGEN(degree+1)
 p(diag)= _a_ *stheta^n
 dp(diag)= n*ctheta*p(diag)/stheta

;The first off-diagonal is still fairly simple
;
 offdiag= diag(0:degree-1) + 1
 b= _b_(offdiag)
 p(offdiag)= b*ctheta*p(offdiag-1)
 dp(offdiag)= b*(ctheta*dp(offdiag-1) - stheta*p(offdiag-1))

;Harder
;
;? n= REVERSE(INDGEN(degree+1))
;? m= n+2
 FOR indx=2,degree DO BEGIN
   offdiag= diag[0:degree-indx] + indx
   b= _b_[offdiag]
   c= _c_[offdiag]

;pre-calculate some things for speed
   offdiag1= offdiag-1
   offdiag2= offdiag-2
   p1= p[offdiag1]

   p[offdiag]=  b*ctheta*p1 - c*p[offdiag2]
   dp[offdiag]= b*(ctheta*dp[offdiag1] - stheta*p1) - c*dp[offdiag2]

ENDFOR


;************************************************************************
;Having calculated the Legendre Polynomials, we now have to weight
;them with the appropriate coefficients, sines and cosines, and
;radial factors.  Try to do this in arrays, rather than loops.
;
;Then sum them.  Note that the P array is half empty, so a simple total
;is a bit inefficient.  However, I've tried adding only the non-zero parts
;and it doesn't speed up appreciably (due to time required for indexing).
;

 N= INDGEN(degree+1)    &       M=N
 Cphi= COS(M*phi)        &       Sphi= SIN(M*phi)
 Ratio= DOUBLE((Re/r)^(N+2))
 g= DOUBLE(model_coefficients(0).g[0:degree,0:degree])  & gP= g*P
 h= DOUBLE(model_coefficients(0).h[0:degree,0:degree])  & hP= h*P

 Br= TOTAL( (N+1) * Ratio * (gP#Cphi + hP#Sphi) )
 Btheta= - TOTAL( Ratio * ( (g*dP)#Cphi + (h*dP)#Sphi ) )
 Bphi= TOTAL( Ratio * ( gP#(Sphi*M) - hP#(Cphi*M) ) ) / stheta

 ;V= a * TOTAL( (Re/r)^(N+1) * (gP#Cphi + hP#Sphi) )  ;this is the geomagnetic potential

 IF KEYWORD_SET(GEOCENTRIC) THEN BEGIN
    result= [-Br,-Btheta,Bphi]   ;[down,north,east], spherical earth
 ENDIF ELSE IF KEYWORD_SET(CARTESIAN) THEN BEGIN
    cbeta= cos(betaa)    &  sbeta= sin(betaa)
    cphi= cos(phi)      &  sphi= sin(phi)
    north= [-cphi*sbeta,-sphi*sbeta,cbeta]
    east= [-sphi,cphi,0]
    up= [cbeta*cphi,cbeta*sphi,sbeta]
    result= Br*up + Bphi*east - Btheta*north
 ENDIF ELSE BEGIN   ;geodetic coordinates (the default)
    psi= alpha-betaa
    north= -Btheta * cos(psi) - Br * sin(psi)
    east= Bphi
    down= Btheta * sin(psi) - Br * cos(psi)
    result=[down,north,east]      ;[down,north,east], ellipsoidal earth
 ENDELSE

; "CARTESIAN":BEGIN
;      cbeta= cos(betaa)    &  sbeta= sin(betaa)
;      cphi= cos(phi)      &  sphi= sin(phi)
;      north= [-cphi*sbeta,-sphi*sbeta,cbeta]
;      east= [-sphi,cphi,0]
;      up= [cbeta*cphi,cbeta*sphi,sbeta]
;      result= Br*up + Bphi*east - Btheta*north
;             END


RETURN,result
END

PRO IGRF_TIMETEST,n

  IF (N_PARAMS() LT 1) THEN n=100

  pos= [105.0e3, 44.0, 111.0]
  startime= SYSTIME(1)
  FOR indx=1,n DO test= IGRF_MODEL(pos)
  PRINT,'IGRF_MODEL takes ',FLOAT(SYSTIME(1)-startime)/n,'  seconds per call'
  PRINT,test

RETURN
END

PRO IGRF_VALIDATE

  COMMON IGRF_MODEL

  x= [300.0e3,75.0,10.0]

  MESSAGE,SYSTIME(0),/INFORMATIONAL
  PRINT,' '
  PRINT,'Location',x
  PRINT,' '
  PRINT,'Year     Dipole     |B|              Vector Field                Dip   Declination'
  PRINT,'         Moment                  north     east      down  '
  PRINT,' '
  FOR year=1945,2000,5 DO BEGIN
    b= IGRF_MODEL(x,YEAR=year)
    bmag= SQRT(TOTAL(b^2))
    dimo= SQRT( TOTAL( model_coefficients(0).g(1,0)^2 + model_coefficients(0).g(1,1)^2 + model_coefficients(0).h(1,1)^2 ))
    dip= ATAN(b(0),SQRT(TOTAL(b(1)^2 + b(2)^2)))
    dec= ATAN(b(2),b(1))
    PRINT,year,dimo,bmag,b(1),b(2),b(0),dip*!radeg,dec*!radeg,FORMAT='(I4,2X,F9.2,2X,F9.2,3X,F8.2,2X,F8.2,2X,F9.2,7X,F5.2,3X,F5.2)'
  ENDFOR

RETURN
END


FUNCTION IGRF_GET_LVALUE,start_position,end_position,STEPSIZE=stepsize,MAX_DISTANCE=max_distance

   ;x= [300.0e3, 75.0, 10.0]
   x= GEODETIC_CONVERT(start_position,/FROM_GEODETIC,/TO_CARTESIAN)

   IF KEYWORD_SET(STEPSIZE) THEN ds=stepsize ELSE ds=-900.0e3
   IF KEYWORD_SET(MAX_DISTANCE) THEN x_max=max_distance ELSE x_max= 10.0
  ; ds= -900.0e3
   re= 6371.2d3
;   b= IGRF_MODEL(x,YEAR=1990,/CARTESIAN)
;   PLOT,[-10,15],[-10,15],/NODATA

   continue=0
   l_value= 0.0
   REPEAT BEGIN

     old_l_value= l_value

     b= IGRF_MODEL(x,/CARTESIAN)
     k1= b / SQRT(TOTAL(b^2))

     b= IGRF_MODEL(x+0.5*ds*k1,/CARTESIAN)
     k2= b / SQRT(TOTAL(b^2))

     b= IGRF_MODEL(x+0.5*ds*k2,/CARTESIAN)
     k3= b / SQRT(TOTAL(b^2))

     b= IGRF_MODEL(x+ds*k3,/CARTESIAN)
     k4= b / SQRT(TOTAL(b^2))

     dx= ds*(k1 +2*k2 +2*k3 +k4)/6.0

     x= x + dx
     l_value= SQRT(TOTAL(x^2))
   ;  PLOTS,SQRT(x(0)^2 + x(1)^2)/re, x(2)/re,CONTIN=continue
   ;  continue=1
   ;  WAIT,0
     ;print,x_mag/re
;stop
   ENDREP UNTIL (old_l_value GE l_value) OR (l_value LT 0.9) OR (l_value/re GT x_max)

   end_position= x
RETURN,l_value/re
END

PRO IGRF_TRACE_FIELDLINE2

   x= [300.0e3, 75.0, 10.0]
   x= GEODETIC_CONVERT(x,/FROM_GEODETIC,/TO_GEOCENTRIC)

   ds= -500.0e3
   re= 6371.2d3
   b= IGRF_MODEL(x,YEAR=1990,/GEOCENTRIC)
   PLOT,[-10,15],[-10,15],/NODATA

   continue=0
   x_mag= 0.0
   REPEAT BEGIN
;stop
     old_x_mag= x_mag
     b= IGRF_MODEL(x,/GEOCENTRIC)
     b= [-b(0), b(1)/x(0), b(2)/(x(0)*SIN((90.0-x(1))*!dtor)) ]
     k1= b / SQRT(TOTAL(b^2))

;stop
     b= IGRF_MODEL(x+0.5*ds*k1,/GEOCENTRIC)
     b= [-b(0), b(1)/x(0), b(2)/(x(0)*COS(x(1)*!dtor)) ]
     k2= b / SQRT(TOTAL(b^2))

     b= IGRF_MODEL(x+0.5*ds*k2,/GEOCENTRIC)
     b= [-b(0), b(1)/x(0), b(2)/(x(0)*COS(x(1)*!dtor)) ]
     k3= b / SQRT(TOTAL(b^2))

     b= IGRF_MODEL(x+ds*k3,/GEOCENTRIC)
     b= [-b(0), b(1)/x(0), b(2)/(x(0)*COS(x(1)*!dtor)) ]
     k4= b / SQRT(TOTAL(b^2))

     dx= ds*(k1 +2*k2 +2*k3 +k4)/6.0

     x= x + dx
     x_mag= x(0); SQRT(TOTAL(x^2))
     PLOTS,x(0)/re, x(0)/re,CONTIN=continue
     continue=1
     WAIT,0
     print,x ;x_mag/re
;stop
   ENDREP UNTIL (old_x_mag GE x_mag) ;(x_mag GT 15.0*re) OR (x_mag LT 1.0*re)
   PRINT,old_x_mag/re,x_mag/re

RETURN
END

PRO IGRF_DIPOLE

  COMMON IGRF_MODEL,model_coefficients,recursion_coefficients

  dummy= IGRF_MODEL([1,1,1])

  diplon= ATAN( -model_coefficients.h[1,1], -model_coefficients.g[1,1] )
  tmp= model_coefficients.g[1,1]*COS(diplon)
  tmp= tmp + model_coefficients.h[1,1]*SIN(diplon)
  diplat= !pi/2.0 - ASIN(tmp/model_coefficients.g[1,0])

  year= model_coefficients.year
  w= WHERE(year GT 1980 AND year NE 1995)  ;lon jumps in 1995, ignore

  yearlist= model_coefficients[w].year
  diplat= diplat[w]*!radeg
  diplon= diplon[w]*!radeg  +360.0

  latc= POLY_FIT(yearlist-2000,diplat,2,YFIT=lat)
  lonc= POLY_FIT(yearlist-2000,diplon,2,YFIT=lon)

  PRINT,'Polynomial fit to magnetic dipole position centered on year 2000'
  PRINT,'Years',yearlist
  PRINT,'Dipole Latitudes ',diplat
  PRINT,'Dipole Longitudes ',diplon
  PRINT,'Latitude Coefficients ',TRANSPOSE(latc[0,0:*])
  PRINT,'Longitude Coefficients ',TRANSPOSE(lonc[0,0:*])


  plot,yearlist-2000,diplon,/ynozero,psym=-4 & oplot,yearlist-2000,lon,THICK=3
  plot,yearlist-2000,diplat,/ynozero,psym=-4 & oplot,yearlist-2000,lat,THICK=3  
stop

RETURN
END


FUNCTION IGRF_GEO2HDZ,position,_EXTRA=_extra

 result= FLTARR(3,3)

 b= IGRF_MODEL(position,_EXTRA=_extra)
 bmag= SQRT(TOTAL(b^2))
 bhat= b/bmag

;
;Not really sure what I was doing here....
;

RETURN,result
END

