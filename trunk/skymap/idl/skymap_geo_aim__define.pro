;# Subversion $Id: file rev date time user $
;
;# ?? TO DO ??  -factor out aim_[target,range,height] into skymap geographic
;#              -rename remainder to skymap_aim?
;
;
;+
; For a given geographic location (altitude, latitude, longitude) there are three common types of aiming
;   1) astro: right ascension, declination
;   2) local: azimuth, zenith angle (east, north, up)  <=== DEFAULT
;   3) device: orientation with respect to local
;
; Direction is stored internally using GEOSPACE_VECTOR objects in the GEO (cartesian spherical earth) 
; system and transformed using 3x3 transformation matrices ie.
;   astro= geo2gei ## geo
;   local= geo2local ## geo
;   device= local2dev ## local
;
; Examples:
;  
;  # Track Vega over 24 hours
;  gaim= skymap_geo_aim([100.0, 56.4, 265.4])                     ;# altitude, latitude, longitude (degrees)
;  FOR indx=0,1439 DO BEGIN
;    gaim->set_time,[2012,01,01,indx/60,indx MOD 60,0],/ymdhms    ;# 1-minute steps
;    gaim->set_aim,[279.23, 38.78],/astro,/degrees                ;# right ascension, declination (degrees) of Vega  
;    aim= gaim->get_aim(/degrees)                                 ;# azimuth, zenith angle
;  ENDFOR
;-
;






;# IDL defines this implicitly, but we want more complex behavior.
;# Specifically "pass through" ie. if input is valid object, just return it 
FUNCTION SKYMAP_GEO_AIM,location,time,_EXTRA=_extra ;COPY=copy
  classname= 'SKYMAP_GEO_AIM'  &  siz= SIZE(location,/STRUCT)
  IF (siz.type EQ 11) && (OBJ_CLASS(location) EQ classname) THEN RETURN,location
  result= OBJ_NEW(classname,location,time,_EXTRA=_extra)
RETURN,result
END


;# called only by OBJ_CREATE
FUNCTION SKYMAP_GEO_AIM::INIT,location,time,_EXTRA=_extra
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  self.location= SKYMAP_GEOGRAPHIC(location,_EXTRA=_extra)  ;# FIXME: allocate empty and use set method?
  self.direction= SKYMAP_GEOSPACE('GEO',[0,0,0],time,_EXTRA=_extra)
  self.set_orientation,self.orientation.euler_angles
  RETURN, OBJ_VALID(self.location) AND OBJ_VALID(self.direction) 
END ;#----------------------------------------------------------------------------


;# called only by OBJ_DESTROY  !!Do not destroy the objects, as the refs may also be used elsewhere
PRO SKYMAP_GEO_AIM::CLEANUP
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (HEAP_REFCOUNT(self.location) EQ 1) THEN OBJ_DESTROY,self.location
  IF (HEAP_REFCOUNT(self.direction) EQ 1) THEN OBJ_DESTROY,self.direction
  self->skymap_object::cleanup
RETURN
END ;#----------------------------------------------------------------------------


;# FIXME
FUNCTION SKYMAP_GEO_AIM::_overloadPrint 
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (self.n_elements GT 1) THEN BEGIN
    d= self.dimensions  &  d= d[WHERE(d NE 0)]
    tmp= STRING(d,FORMAT='("[",8(I,:,","))')+']'
    tmp= STRCOMPRESS(tmp,/REMOVE_ALL)
    RETURN,'      OBJREF  =  '+OBJ_CLASS(self)+tmp
  ENDIF 
  
  line1= self.location->_overloadPrint()
  line2= self.direction->_overloadPrint()
  RETURN,['      OBJREF  =  '+OBJ_CLASS(self), line1, line2]
END ;#----------------------------------------------------------------------------


PRO SKYMAP_GEO_AIM::SET_LOCATION,value,ERROR_FLAG=error_flag,_EXTRA=_extra  ;# only allow a single position?
  ON_ERROR,2  &  error_flag= 1
  CATCH,status & IF (self->skymap_object::catch(status)) THEN RETURN
  loc= SKYMAP_GEOGRAPHIC(value,_EXTRA=_extra)
  IF NOT OBJ_VALID(loc) THEN MESSAGE,'Error- input location must be valid SKYMAP_GEOGRAPHIC'
  IF (loc.n_elements() NE 1) THEN MESSAGE,'Error- more than one input location not allowed'
  self.location= loc  &  error_flag= 0  &  RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEO_AIM::GET_LOCATION,NUMERIC=numeric,_EXTRA=_extra
  IF KEYWORD_SET(NUMERIC) THEN RETURN,self.location->get(_EXTRA=_extra)
  RETURN,self.location  ;->copy()  ;# {SKYMAP_GEOGRAPHIC}
END ;#----------------------------------------------------------------------------


PRO SKYMAP_GEO_AIM::SET_TIME,value,ERROR_FLAG=error_flag,_EXTRA=_extra
  self.direction->set_time,value,ERROR_FLAG=error_flag,_EXTRA=_extra
  RETURN
END ;#----------------------------------------------------------------------------

 
FUNCTION SKYMAP_GEO_AIM::GET_TIME ;,_EXTRA=_extra
  RETURN,self.direction->get_time()
END ;#----------------------------------------------------------------------------


;#  Allow for an arbitrary rotation from local geodetic to internal device coordinates 
;#
PRO SKYMAP_GEO_AIM::SET_ORIENTATION,yaw_pitch_tilt,ERROR_FLAG=error_flag,_EXTRA=_extra
  ON_ERROR,2  &  error_flag=1
  CATCH,status  &  IF (self.skymap_object::catch(status)) THEN RETURN
  
  yaw= yaw_pitch_tilt[0]  &  pitch= yaw_pitch_tilt[0]  &  tilt= yaw_pitch_tilt[0]  
  
  vec= SKYMAP_VECTOR()  ;# temporary
  self.orientation.euler_angles= [yaw,pitch,tilt]  ;Z-X-Z
  self.orientation.local2device= vec.EULER_MATRIX(yaw, pitch, tilt, /DEGREES) 

  error_flag=0  &  RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEO_AIM::GET_ORIENTATION
  RETURN,self.orientation
END ;#----------------------------------------------------------------------------


;azimith/zenith angle  or  right ascension/declination
PRO SKYMAP_GEO_AIM::SET_AIM,angles,ERROR_FLAG=error_flag,ASTRONOMICAL=astronomical,DEVICE=device,_EXTRA=_extra

  IF KEYWORD_SET(ASTRONOMICAL) THEN BEGIN
    system='gei'    &  spherical=2   ;# right ascension, declination
  ENDIF ELSE IF KEYWORD_SET(INSTRUMENTAL) THEN BEGIN
    system='device' &  spherical=4    
  ENDIF ELSE BEGIN  
    system='local'  &  spherical=4   ;# azimuth, zenith angle
  ENDELSE
  
  IF KEYWORD_SET(VECTOR) THEN BEGIN
    self.direction->set_vector,angles,ERROR_FLAG=error_flag,_EXTRA=_extra
  ENDIF ELSE BEGIN
    siz= SIZE(angles,/STRUCT)  &  n= siz.n_elements / 2  &  angles= REFORM(angles,n,2) ;# need to check siz=Nx2
    vec= DBLARR(n,3)  &  vec[*,0]= 1.0d0  &  vec[*,1]= angles[*,1]  &  vec[*,2]= angles[*,0]
    self.direction->set_vector,vec,SPHERICAL=spherical,ERROR_FLAG=error_flag,_EXTRA=_extra  
  ENDELSE
  self.direction->set_system,system
  
  IF NOT KEYWORD_SET(ASTRONOMICAL) THEN BEGIN
    matrix= self.location.local2geo_matrix()
    IF KEYWORD_SET(INSTRUMENTAL) THEN matrix= matrix ## TRANSPOSE(self.orientation.local2device)
    self.direction.matrix_multiply,matrix
    system='geo'
  ENDIF  ;#  geo= loc2geo##(dev2loc##dev) = (loc2geo##dev2loc)##dev 
  
  RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEO_AIM::GET_AIM,ASTRONOMICAL=astronomical,DEVICE=device,DEGREES=degrees,STRUCTURE=structure,VECTOR=vector
  
;# !!FIXME!! vectorize  
  aim= self.direction  &  nel= aim.n_elements()  &  dim= aim.dimensions(/NO_ZERO)
  IF KEYWORD_SET(ASTRONOMICAL) THEN BEGIN
    aim= aim->get_vector('gei')  &  spherical=2  ;# ra/dec     ;# add hhhmmss for ra     
  ENDIF ELSE BEGIN
    aim= aim->get_vector('geo')  &  spherical=4  ;# az/za
    matrix= TRANSPOSE(self.location.local2geo_matrix()) ;# east,north,up
    IF KEYWORD_SET(INSTRUMENTAL) THEN matrix= self.orientation.local2device ## matrix
    aim.matrix_multiply,matrix
  ENDELSE
  
  IF KEYWORD_SET(VECTOR) THEN RETURN,aim    ;# {SKYMAP_VECTOR}
  
  aim= aim.get(SPHERICAL=spherical,DEGREES=degrees,/NO_REFORM)   ;#  DBLARR(N,3)  
  
  IF KEYWORD_SET(STRUCTURE) THEN BEGIN
    IF KEYWORD_SET(ASTRONOMICAL) THEN tmp={SKYMAP_GEO_AIM_ASTRONOMICAL} ELSE tmp= {SKYMAP_GEO_AIM_GEODETIC}
    result= REPLICATE(tmp,dim)
    result.(0)= aim[*,2]  &  result.(1)= aim[*,1] 
  ENDIF ELSE BEGIN
    result= DBLARR(nel,2)
    result[*,0]= aim[*,2]  &  result[*,1]= aim[*,1]
    result= REFORM(result,[dim,2])
  ENDELSE
  
  RETURN,result
END ;#----------------------------------------------------------------------------



; FUNCTION SKYMAP_GEO_AIM::AIM_TARGET  ;return local direction
; FUNCTION SKYMAP_GEO_AIM::AIM_RANGE   ;return target position
; FUNCTION SKYMAP_GEO_AIM::AIM_HEIGHT  ;return target position
; FUNCTION SKYMAP_GEO_AIM::AIM_SOLAR   ;return local direction
; FUNCTION SKYMAP_GEO_AIM::AIM_MAGNETIC  ;magnetic zenith?
; FUNCTION SKYMAP_GEO_AIM::AIM_ASTRO  



;# given a geographic location, set the aim  (AKA map2aim)
PRO SKYMAP_GEO_AIM::AIM_TARGET,geographic_location,ERROR_FLAG=error_flag,_EXTRA=_extra
  loc= SKYMAP_GEOGRAPHIC(geographic_location,_EXTRA=_extra)
  IF NOT OBJ_VALID(loc) THEN MESSAGE,'Error- input location must be valid SKYMAP_GEOGRAPHIC'
  aim= loc.get(/CARTESIAN) - self->get_location(/NUMERIC,/CARTESIAN)    ;# vector from origin to target
  self.direction->set_vector,aim,ERROR_FLAG=error_flag1
  self.direction->set_system,'geo',ERROR_FLAG=error_flag2
  error_flag= error_flag1 OR error_flag2
;  local= self.location.direction()
;  aimG= INVERT(local) ## (aimC)  ;# [east,north,up]
;  self.direction->set_vector,aimG,SUCCESS=success
;  self.direction->set_system,'geo' 
;  tmp= SKYMAP_VECTOR(aimG)  ;&  self.direction.skymap_vector::set,tmp
;  result= tmp.get(SPHERICAL=4,_EXTRA=_extra)  ;# zenith angle , azimuth clockwise from north
  RETURN ;,result  ;# range, zenith_angle, azimuth_angle
END ;#---------------------------------------------------------------------


;# For a given single local aim vector and one or more ranges, calculate the target position(s).
FUNCTION SKYMAP_GEO_AIM::TARGET_RANGE,target_range,_EXTRA=_extra
  aim= self.direction->get_vector('geo')
;  SKYMAP_VECTOR(aim_vector,COPY=1,_EXTRA=_extra)  ;# local east,north,up
;  IF (aim.n_elements() NE 1) THEN MESSAGE,'Error- more than one input aim_vector.;
;  aim= aim.matrix_multiply(self.position.direction()) ;# x,y.z
  aim.scalar_multiply,target_range/aim.magnitude() ;# unit vectors
  aim= aim.get()  &  loc= self.get_location(/NUMERIC,/CARTESIAN)
  FOR indx=0,2 DO aim[0,indx] = loc[indx] + aim[*,indx]  ;# OK for multiple aim, relies on only one position
  result= SKYMAP_GEOGRAPHIC(aim,/CARTESIAN)
RETURN,result
END ;#----------------------------------------------------------------------------



;# For a given single local aim vector and one or more heights, calculate the target position(s).
;# !! FIXME !! improve initial guesses for range
FUNCTION SKYMAP_GEO_AIM::TARGET_HEIGHT,target_height,_EXTRA=_extra
 ; aim= SKYMAP_VECTOR(aim_vector,_EXTRA=_extra)  ;# local east,north,up
 ; aim= self.direction->get_vector('geo')
  range= [1.0, 1.0d3, 500.0d3, 1500.0d3, 9000.0d3]  &  height= range*0
  tmp= self.target_range(range)
  height= (tmp.get())[*,0]
  dh= MIN(ABS(height - target_height),mindx)  &  r0= range[mindx]
 
  nloops=0  &  nmax=15  &  REPEAT BEGIN
    r= INTERPOL(range,height,target_height,QUADRATIC=0)
    r= (1.0d0 + ABS(r-r0)*[0.0, -0.2, +0.2, -1.0, +1.0]) + r[0]
    tmp= self.aim_range(aim,r)  &  h= (tmp.get())[*,0]
    height= [height,h]  &   range= [range,r]  &  r0= r[0]
    s= SORT(range)  &  range= range[s]  &  height= height[s]
    nloops= nloops+1  &  dh= target_height - h[0]  &  PRINT,nloops,dh
  ENDREP UNTIL (ABS(dh) LE 10.0) OR (nloops GE nmax)  ;# stop if within 10 metres or don't appear to be converging
  IF (nloops GE nmax) THEN MESSAGE,"Warning- failed to converge",/INFORMATIONAL 

  r= INTERPOL(range,height,target_height,/SPLINE)  ;# final range
  result= self.target_range(aim,r)                    ;# SKYMAP_GEOGRAPHIC

RETURN,result
END ;#----------------------------------------------------------------------------


;;# For a given single local aim vector and one or more ranges, calculate the target position(s).
;FUNCTION SKYMAP_GEO_AIM::AIM_SOLAR,time_value,_EXTRA=_extra
;  IF (N_PARAMS() EQ 0) THEN time= self.direction->get_time() ELSE $
;    time= SKYMAP_TIME(time_value,COPY=1,_EXTRA=_extra)
;
;  sun= SKYMAP_GEOSPACE('gse',[1.0,0.0,0.0]);,time[0],/Y2K_SECONDS)   
;  n= time.n_elements()  &  time= time.get(/Y2K_SECONDS)  &  result= DBLARR(n,3)
;  FOR indx=0,n-1 DO BEGIN
;  ;  sun.set,'gse',[1.0,0.0,0.0],time[indx],/Y2K_SECONDS
;    sun.set_time,time[indx],/Y2K_SECONDS  &  print,time[indx]
;    aim= sun.get_vector('geo',NO_COPY=0)  ;# SKYMAP_VECTOR
;    aim.matrix_multiply,TRANSPOSE(self.location.local2geo_matrix())  ;# east,north,up
;    result[indx,0]= aim.get()  ;# DBLARR(1,3)
;  ENDFOR
;
;RETURN,SKYMAP_VECTOR(result)
;END ;#----------------------------------------------------------------------------


;# Point at the sun.  Input time_value can be scalar or array.
PRO SKYMAP_GEO_AIM::AIM_SOLAR,time_value,_EXTRA=_extra
  IF (N_PARAMS() EQ 0) THEN time= self.direction->get_time() ELSE $
    time= SKYMAP_TIME(time_value,COPY=1,_EXTRA=_extra)

  sun= SKYMAP_GEOSPACE('gse',[1.0,0.0,0.0])  ;,time[0],/Y2K_SECONDS)   
  n= time.n_elements()  &  time= time.get(/Y2K_SECONDS)  &  result= DBLARR(n,3)
  FOR indx=0,n-1 DO BEGIN
    sun.set,'gse',[1.0,0.0,0.0],time[indx],/Y2K_SECONDS
  ;  sun.set_time,time[indx],/Y2K_SECONDS  &  print,time[indx]
    aim= sun.get_vector('geo',NO_COPY=0)  ;# SKYMAP_VECTOR
    aim.matrix_multiply,TRANSPOSE(self.location.local2geo_matrix())  ;# east,north,up
    result[indx,0]= aim.get()  ;# DBLARR(1,3)
  ENDFOR  ;  & stop

  self.direction->set_system,'geo'
  self.direction->set_vector,result  ;# Nx3
  RETURN
END ;#----------------------------------------------------------------------------



;# For a given single local aim vector and one or more ranges, calculate the target position(s).
FUNCTION SKYMAP_GEO_AIM::LOCAL2ASTRO_MATRIX,time_value,_EXTRA=_extra
  IF (N_PARAMS() LT 2) THEN time= self.direction->get_time() ELSE $
    time= SKYMAP_TIME(time_value,COPY=1,_EXTRA=_extra)

  matrix= self.skymap_geospace::matrix('geo','gei',time.get(/JULIAN_DATE))
  result= matrix ## self.location.direction    ;# local2gei
 
RETURN,result
END ;#----------------------------------------------------------------------------


;# to point at a star:
; q.set_aim(ra,dec,/ASTRO,/DEGREES)
; print,q.get_aim(/DEGREES) ;# geodetic azimuth, zenith_angle


;# generate a locus of aiming at the horizon
PRO SKYMAP_GEO_AIM::AIM_HORIZON,zenith_angle,azimuth,ERROR_FLAG=error_flag,_EXTRA=_extra
  IF (zenith_angle EQ !NULL) THEN BEGIN
    MESSAGE,'Warning- no input zenith_angle provided, using 90 degrees',/INFORM
    za= 90.0
  ENDIF ELSE za= zenith_angle[0]
  IF (azimuth EQ !NULL) THEN az= FINDGEN(360) ELSE az= azimuth
  n= N_ELEMENTS(az)  &  za= REPLICATE(za,n)
  self.set_aim,[[az],[za]],/DEGREES,ERROR_FLAG=error_flag
  RETURN
END ;#---------------------------------------------------------------------

;PRO SKYMAP_GEO_AIM::AIM_MERIDIAN,azimuth,zenith_angle,ERROR_FLAG=error_flag,_EXTRA=_extra


FUNCTION SKYMAP_GEO_AIM::SELFTEST
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  flag= ['[PASS]','[FAIL]']+' '  &  ntest=0  &  nfail=0

;  note= 'Create object from empty (invalid)'
;  x= SKYMAP2_TIME()  &  fail= OBJ_VALID(x)
;  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  location= SKYMAP_GEOGRAPHIC([0.0, 10.0, 23.4])
  target= SKYMAP_GEOGRAPHIC([500.0, 10.0, 23.4])
  time= SKYMAP_TIME('20010203T000000',/ISO)

  note= 'Create object with position only'
  g= SKYMAP_GEO_AIM(location)  &  fail= ~OBJ_VALID(g)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Create object with position and time'
  g= SKYMAP_GEO_AIM(location,time)  &  fail= ~OBJ_VALID(g)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
  
;  print,g.get_location()
;  print,g.get_time()
  
  angles= [70.0,10.0]

  note= 'set/get aim (local)'
  g.set_aim,angles,/DEGREES  &  fail= TOTAL((g.get_aim(/DEGREES)-angles)^2) GT 1d-12
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'set/get aim (astro)'
  g.set_aim,angles,/DEGREES,/ASTRO  &  fail= TOTAL((g.get_aim(/DEGREES,/ASTRO)-angles)^2) GT 1d-1
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1 

  note= 'set/get aim (astro->local->astro)'  
  g.set_aim,angles,/DEGREES,/ASTRO  &  aim= g.get_aim()
  g.set_aim,aim  &  fail= TOTAL((g.get_aim(/DEGREES,/ASTRO)-angles)^2) GT 1d-1
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  angles= [90,0]
  note= 'aim_target in zenith'
  g.set_location,location  ;# 500m straight up, zangle=0
  g.aim_target,target  &  fail= (g.get_aim(/DEGREES))[1] GT 1d-12  
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'target_range in zenith'  
  b= g.target_range(500.0)  &  fail= ABS((b.get())[0]-500.0) GT 1d-12
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Rankin Inlet zenith at 02:32:46 October 20 2006 from Skycalc'
  g.set_time,'20061020T023246',/ISO
  g.set_location,[6.0, 62+49.5/60.0, -(92+6.9/60.0)],/DEGREES
  ;g.set_aim,[90.0,0.0],/DEGREES  &  zenith= g.get_aim(/ASTRO,/DEGREES)  &   print,zenith
  ras= 360.0*(22+(17+59.4/60.0)/60.0)/24.0  &  dec=62.0+(49+29/60.0)/60.0 
  g.set_aim,[ras,dec],/DEGREES,/ASTRO & aim= g.get_aim(/DEGREES) & fail= (aim[1] GE 0.05) ;# 3 arc-minutes
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1  
  
  note= 'Vega from Calgary according to US Naval Observatory'
  g.set_time,'20061019T102030',/ISO
  g.set_location,[1143.0, 51.079528, -114.129778],/DEGREES
  vec= SKYMAP_VECTOR([1.0, 86.233417, 336.615222],spherical=4,/degrees)
  ras= 360.0*(18+(37+9.524/60.0)/60.0)/24.0  &  dec= 38.0+(47+32.92/60.0)/60.0
  g.set_aim,[ras,dec],/DEGREES,/ASTRO & aim= g.get_aim(/VECTOR)
  dot= aim.dot_product(vec,/angle,/degrees)  &  fail= (dot GE 0.35)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1   
 
stop 
  
  g.set_location,location
  g.aim_target,target  &  print,g.get_aim()  
  b= g.target_range([1.0,0.0,0.0],500,spherical=4,/degrees)  ;# 500m straight up  
  c= g.aim_target(b)

 b= g.aim_range([500.0,90.0,90.0],spherical=4,/degrees)  &  print,b
 b= g.aim_range([[500.0,0.0,0.0],[5000.0,0.0,0.0]],spherical=4,/degrees,/transpose)  &  print,b.get() 
  stop


  
  g.aim_solar,['20010203T000000','20010203T120000'],/ISO  &  print,g.get_aim(/degrees)  
  
  time= SKYMAP_TIME('20010203T000000',/ISO)  &  t=time.get(/Y2K) 
  t= t[0] + FINDGEN(1440)*60
 PROFILER,/CLEAR  &  PROFILER,/RESET  &  PROFILER  &   PROFILER,/SYSTEM
  st= SYSTIME(1)  &  g.aim_solar,t,/Y2K  &  st= SYSTIME(1)-st  &  PRINT,st  
 PROFILER,/REPORT
  z= g.get_aim(/degrees)  &  plot,(t-t[0])/3600,z[*,1]
  
  RETURN,FIX([nfail,ntest])  ;# ideally, nfail=0
END ;#----------------------------------------------------------------------------


PRO SKYMAP_GEO_AIM__DEFINE
  COMPILE_OPT IDL2,HIDDEN        ;# don't clutter user visible namespace

 ; RESOLVE_ROUTINE,'SKYMAP_VECTOR__DEFINE',/COMPILE_FULL_FILE,/EITHER,/NO_RECOMPILE
  RESOLVE_ROUTINE,'SKYMAP_OBJECT__DEFINE',/COMPILE_FULL_FILE,/EITHER,/NO_RECOMPILE
  RESOLVE_ROUTINE,'SKYMAP_GEOGRAPHIC__DEFINE',/COMPILE_FULL_FILE,/EITHER,/NO_RECOMPILE
  RESOLVE_ROUTINE,'SKYMAP_GEOSPACE__DEFINE',/COMPILE_FULL_FILE,/EITHER,/NO_RECOMPILE
 ; FORWARD_FUNCTION skymap_vector

  void= {SKYMAP_GEO_AIM_DEVICE, rotation_angle:0.0d0, axial_angle:0.0d0}
  void= {SKYMAP_GEO_AIM_GEODETIC, azimuth:0.0d0, zenith:0.0d0}
  void= {SKYMAP_GEO_AIM_ASTRONOMICAL, right_ascension:0.0d0, declination:0.0d0}

  void= {SKYMAP_ORIENTATION           $
        ,euler_angles:DBLARR(3)       $  ;# Euler angles (z,x,z)
        ,local2device:DBLARR(3,3)     $  ;#
        }
  
  void= {SKYMAP_GEO_AIM                      $
        ,location:OBJ_NEW()                  $  ;# {SKYMAP_GEOGRAPHIC} (only ever a single position)
        ,direction:OBJ_NEW()                 $  ;# {SKYMAP_GEOSPACE}
        ,orientation:{SKYMAP_ORIENTATION}    $           
        ,inherits SKYMAP_OBJECT              $                
        }
        
RETURN
END ;#----------------------------------------------------------------------------

  
;cd,'c:/Users/bjackel/Dropbox/work/idl'
x= SKYMAP_GEO_AIM()
PRINT,x.selftest()
STOP

position= SKYMAP_GEOGRAPHIC([0.0, 0.0, 0.0])
g= SKYMAP_GEO_AIM(position) & help,position,g
g= SKYMAP_GEO_AIM(position) & help,position,g
stop

target= SKYMAP_GEOGRAPHIC([10.0, 0.0, 0.0])
g= SKYMAP_GEO_AIM(position)
print,g.get_position()

g= SKYMAP_GEO_AIM(position,'20010203T000000',/ISO)
print,g.get_time()


help,g.aim_target(target)
stop
b= g.aim_range(500.0,0.0,0.0)
print,b

c= g.aim_target(b)
print,c

PROFILER,/CLEAR  &  PROFILER,/RESET  &  PROFILER  &   PROFILER,/SYSTEM
b= g.aim_height([1.0,90.0,0.0],110.0d3,spherical=4,/degrees)  &  print,b
PROFILER,/REPORT
; SKYMAP_VECTOR::SET          (U)      21    0.000341  0.000016    0.000458  0.000022
; SKYMAP_GEOGRAPHIC::GET      (U)      15    0.000230  0.000015    0.000305  0.000020
; KEYWORD_SET                 (S)     215    0.000146  0.000001    0.000146  0.000001
; SKYMAP_GEO_AIM::AIM_RANGE   (U)       5    0.000082  0.000016    0.001363  0.000273
END