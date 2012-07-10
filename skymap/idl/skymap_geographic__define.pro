;# Subversion $Id$

;+
; This IDL object contains a single geographic location. 
; The internal representation is Cartesian (GEOc) using the SKYMAP_VECTOR object. 
;
; Input/output can be in
;  1) Cartesian 
;  2) Spherical
;  3) Geodetic (AKA geographic) relative to use a reference geoid (ie. WGS-84) <==DEFAULT 
;  
; Examples:
; 
;  IDL> g= SKYMAP_GEOGRAPHIC()
;  % SKYMAP_GEOGRAPHIC::SET: Warning- input value is undefined
;
;  IDL> print,g
;  OBJREF  =  SKYMAP_GEOGRAPHIC   !NULL
;
;  IDL> g.set,[500.0, 66.0, -130.5],CARTESIAN=0
;  IDL> g= SKYMAP_GEOGRAPHIC([500.0, 66.0, -130.5])
;  IDL> print,g
;  OBJREF  =  SKYMAP_GEOGRAPHIC           499.9999999990687         65.99999999999977        -130.5000000000000
;
;  IDL> print,g.n_elements()
;           1 
;           
;  IDL> print,g.dimensions()
;           1           0           0           0           0           0           0           0 
;  
;  IDL> print,g.get()
;       500.00000
;       66.000000
;      -130.50000
;
;  IDL> print,g.get(/radians)
;       500.00000
;       1.1519173
;      -2.2776547
;
;  IDL> print,g.get(/cartesian)    ;# X,Y,Z = Greenwich/Equator, ??, North Pole
;      -1689670.8
;      -1978350.3
;       5804404.3
;
;  IDL> help,g.get(/vector)
;  <Expression>    OBJREF    = <ObjHeapVar200(SKYMAP_VECTOR)>
;-


;# IDL defines this implicitly, but we want to also "pass through": if input is valid object, just return it. 
FUNCTION SKYMAP_GEOGRAPHIC,value,_EXTRA=_extra
  classname= 'SKYMAP_GEOGRAPHIC'  &  siz= SIZE(value,/STRUCT)
  IF (siz.type EQ 11) && (OBJ_CLASS(value) EQ classname) THEN RETURN,value
;  IF (N_PARAMS() EQ 0) THEN result= OBJ_NEW(classname) ELSE result= OBJ_NEW(classname,value,_EXTRA=_extra) 
  result= OBJ_NEW(classname,value,_EXTRA=_extra)
RETURN,result
END


;# called only by OBJ_NEW.  We allow empty input, screen so ->set doesn't complain
FUNCTION SKYMAP_GEOGRAPHIC::INIT,value,_EXTRA=_extra
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  void= self->set_geoid()  &  error_flag=0
  IF (N_PARAMS() GT 0) AND (value NE !NULL) THEN self->set,value,ERROR_FLAG=error_flag,_EXTRA=_extra
  RETURN,~error_flag
END ;#----------------------------------------------------------------------------


;# called only by OBJ_DESTROY
PRO SKYMAP_GEOGRAPHIC::CLEANUP
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  self->skymap_vector::cleanup
RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEOGRAPHIC::_overloadPrint ;,CARTESIAN=cartesian 
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (self.n_elements GT 1) THEN BEGIN
    d= self.dimensions  &  d= d[WHERE(d NE 0)]
    tmp= STRING(d,FORMAT='("[",8(I,:,","))')+']'
    tmp= STRCOMPRESS(tmp,/REMOVE_ALL)
  ENDIF ELSE IF PTR_VALID(self.vector) THEN tmp= '   ' + STRING(self.get(),FORMAT='(3(G,X))') $
  ELSE tmp= '   ' + '!NULL'
  RETURN,'OBJREF  =  '+OBJ_CLASS(self)+tmp
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEOGRAPHIC::SET_GEOID,semi_major_axis,flattening,WGS84=wgs84
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  status=0
  a=6378137.0d0 & f1= 298.257223563d0
  f= 1.0d0/f1            ;flattening of the spheroid = (a-b)/a
  e2= f*(2.0d0-f)        ;eccentricity of the ellipsoid, e^2= (a^2 - b^2) / a^2
  f2= (1.0d0 - f)^2  ;1-e^2 = (1-f)^2
  
  self.geoid.a = a
  self.geoid.f = f
  self.geoid.e2 = e2
  self.geoid.f2 = f2
  
  RETURN,status
END ;#----------------------------------------------------------------------------



PRO SKYMAP_GEOGRAPHIC::SET,value,ERROR_FLAG=error_flag,CARTESIAN=cartesian,RADIANS=radians
  ON_ERROR,2  &  error_flag= 1
  CATCH,status & IF (self->skymap_object::catch(status)) THEN RETURN

  siz= SIZE(value,/STRUCTURE)
  IF (siz.type EQ 0) THEN BEGIN
    MESSAGE,'Warning- input value is undefined',INFORM=1
    error_flag= 0  &  RETURN   ;# allow empty objects
  ENDIF

  IF KEYWORD_SET(CARTESIAN) THEN BEGIN
    self->skymap_vector::set,value,ERROR_FLAG=error_flag
    RETURN
  ENDIF

  vec= SKYMAP_VECTOR(value)  ;# attempt to cast input to vector
  IF NOT OBJ_VALID(vec) THEN MESSAGE,'Error- input must be valid vector'
  pos= vec.get(/POINTER)  &  pos= *pos  ;# Nx3 vector, may need REFORM

  geoid= self.geoid
  deg2rad= !dpi/180.0d0  ;&  rad2deg= 180.0d0/!dpi

  h= REFORM( pos[*,0] )                 ;geodetic height (metres above mean sea level)
  IF KEYWORD_SET(radians) THEN BEGIN
    phi= REFORM( pos[*,1] )               ;geodetic latitude
    lambda= REFORM( pos[*,2] )            ;geodetic longitude  
  ENDIF ELSE BEGIN  ;# convert from degrees
    deg2rad= !dpi/180.0d0
    phi= REFORM( pos[*,1] ) * deg2rad     ;geodetic latitude
    lambda= REFORM( pos[*,2] ) * deg2rad  ;geodetic longitude
  ENDELSE
  
  cphi= COS(phi)  &  sphi= SIN(phi)  
  Nphi= geoid.a / SQRT( 1.0d0 - geoid.e2*sphi^2 )    ;East-West curvature of the ellipsoid, aC in IAU notation
  
  tmp= (Nphi+h) * cphi         ;# calculation appears twice
  pos[0,0]= tmp * COS(lambda)
  pos[0,1]= tmp * SIN(lambda)
  pos[0,2]= (geoid.f2*Nphi+h) * sphi
  self->skymap_vector::set,pos,ERROR_FLAG=error_flag ;!! FIXME: what about reform?
  
RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEOGRAPHIC::GET,CARTESIAN=cartesian,VECTOR=vector,RADIANS=radians

  IF NOT PTR_VALID(self.vector) THEN RETURN,!NULL

  IF KEYWORD_SET(CARTESIAN) THEN RETURN,*self.vector  ;# reform from Nx3
 ; IF KEYWORD_SET(VECTOR) THEN RETURN,SKYMAP_VECTOR(self.skymap_vector::get())
  IF KEYWORD_SET(VECTOR) THEN RETURN,self->skymap_vector::copy()

  geoid= self.geoid
  deg2rad= !dpi/180.0d0  &  rad2deg= 180.0d0/!dpi
  
  vec= *self.vector
  x= REFORM(vec[*,0])  &  y= REFORM(vec[*,1])  &  z= REFORM(vec[*,2])
  
  ;# ?? TO DO ?? replace with Bowring 1985: faster and more accurate
  p= SQRT(x^2 + y^2)  ;&  phi= ATAN(z/p)
  phi= ATAN(z,p*(1d0-geoid.e2)) ; !!explain!!
  FOR indx=1,2 DO BEGIN        ;two passes is enough for sub-metre accuracy
    sin_phi= SIN(phi)
    Nphi= geoid.a/SQRT(1.0d0 - geoid.e2*sin_phi^2)
    phi= ATAN( z + Nphi*geoid.e2*sin_phi , p )
  ENDFOR

  IF KEYWORD_SET(RADIANS) THEN BEGIN
    vec[0,0]= p/COS(phi) - Nphi ;a/SQRT(1.0d0 - e2*SIN(phi)^2)  ;# height
    vec[0,1]= phi         ;# latitude
    vec[0,2]= ATAN(y,x)   ;# longitude "lambda"
  ENDIF ELSE BEGIN
    vec[0,0]= p/COS(phi) - Nphi ;a/SQRT(1.0d0 - e2*SIN(phi)^2)  ;# height  
    vec[0,1]= phi * rad2deg  ;# latitude
    vec[0,2]= ATAN(y,x) * rad2deg  ;# longitude "lambda"
  ENDELSE

RETURN,vec
END ;#----------------------------------------------------------------------------


;# need to fix for multiple positions
;# and/or cache to avoid pointless recalculation
FUNCTION SKYMAP_GEOGRAPHIC::LOCAL2GEO_MATRIX ;,latitude,longitude
  geo= self.get(/RADIANS)
  phi= REFORM( geo[*,1] )               ;geodetic latitude
  cphi= COS(phi)  &  sphi= SIN(phi)
  lambda= REFORM( geo[*,2] )            ;geodetic longitude  
  slam= sin(lambda)  &  clam= cos(lambda)
  east= [-slam, clam, 0.0]
  zenith= [cphi*clam, cphi*slam, sphi ]
  north= [-clam*sphi, -slam*sphi, cphi] 
  result= TRANSPOSE(REFORM([east,north,zenith],3,3))
RETURN,result
END ;#----------------------------------------------------------------------------



FUNCTION SKYMAP_GEOGRAPHIC::SELFTEST
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  flag= ['[PASS]','[FAIL]']+' '  &  ntest=0  &  nfail=0

  note= 'Create from empty (valid)'
  x= SKYMAP_GEOGRAPHIC()  &  fail= ~OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  g0= [0.0, 0.0, 0.0]
  note= 'Create from dblarr[3]  (valid)'
  x= SKYMAP_GEOGRAPHIC(g0)  &  fail= ~OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
  
  note= 'Set to dblarr[3]  (valid)'
  x.set,g0,ERROR_FLAG=err  &  fail= err
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Get = dblarr[3]  (valid)'
  tmp= x.get()  &  fail= TOTAL((g0-tmp)^2) GE 1d-6
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  g0= [1234.5, 12.3, 123.4]  &  x.set,g0,ERROR_FLAG=err
  note= 'Compare get after set'
  tmp= x.get()  &  fail= TOTAL((g0-tmp)^2) GE 1d-6
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  RETURN,FIX([nfail,ntest])  ;# ideally, nfail=0
END ;#----------------------------------------------------------------------------


PRO SKYMAP_GEOGRAPHIC__DEFINE
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace

  RESOLVE_ROUTINE,'SKYMAP_VECTOR__DEFINE',COMPILE_FULL_FILE=0,/EITHER,/NO_RECOMPILE
  
  void= {SKYMAP_GEOID, a:0.0d0, f:0.0d0, e2:0.0d0, f2:0.0d0} 
  
  void= {SKYMAP_GEOGRAPHIC             $
        ,geoid:{SKYMAP_GEOID}          $
        ,inherits SKYMAP_VECTOR       $                
        }
        
RETURN
END ;#----------------------------------------------------------------------------

 
g= SKYMAP_GEOGRAPHIC()
print,g.selftest()

END