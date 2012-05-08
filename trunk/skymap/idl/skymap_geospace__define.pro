;# Subversion $Id: file rev date time user $

;# TO DO: use JULIAN=2 to get modified julian dates

;# Objectification of "geospace_convert"


;# IDL defines this implicitly, but we want to also "pass through" : if input is valid object, just return it. 
FUNCTION SKYMAP_GEOSPACE,system,vector,time,_EXTRA=_extra

  classname= 'SKYMAP_GEOSPACE'
  IF (N_PARAMS() EQ 1) THEN BEGIN
    value= position
    siz= SIZE(value,/STRUCT)
    IF (siz.type EQ 11) && (OBJ_CLASS(value) EQ classname) THEN RETURN,value
  ENDIF
  
  result= OBJ_NEW(classname,system,vector,time,_EXTRA=_extra)
RETURN,result
END

;# called only by OBJ_CREATE
FUNCTION SKYMAP_GEOSPACE::INIT,system,vector,time,_EXTRA=_extra
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (N_PARAMS() GT 0) THEN self->set,system,vector,time,ERROR_FLAG=error_flag,_EXTRA=_extra
  RETURN,~error_flag
END ;#----------------------------------------------------------------------------


;# called only by OBJ_DESTROY
PRO SKYMAP_GEOSPACE::CLEANUP
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (HEAP_REFCOUNT(self.time) EQ 1) THEN OBJ_DESTROY,self.time  
  self->skymap_geographic::cleanup
RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEOSPACE::_overloadPrint,_EXTRA=_extra
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  result0= self.system
  result1= self->skymap_vector::_overloadPrint(_EXTRA=_extra)
  result2= self.time->skymap_time::_overloadPrint(_EXTRA=_extra)
RETURN,STRJOIN([result0,result1,result2],STRING(13b))
END ;#----------------------------------------------------------------------------
 
 
; !! LISTs are incredibly slow in IDL !!
;
;PRO SKYMAP_GEOSPACE::SET_SYSTEM,value,ERROR_FLAG=error_flag,_EXTRA=_extra
;  valid= LIST('GEO','GEI','GSE','GSM','SM','MAG')  &  error_flag=1
;  tmp= STRUPCASE(STRCOMPRESS(value,/REMOVE_ALL))
;  match= valid.FindValue(tmp[0],COUNT=nmatch)
;  IF (nmatch NE 1) THEN BEGIN
;    MESSAGE,'Error- unknown input value: '+tmp[0],/INFORM  &  RETURN
;  ENDIF
;  self.system= valid[match]  &  error_flag=0
;RETURN
;END ;#----------------------------------------------------------------------------

PRO SKYMAP_GEOSPACE::SET_SYSTEM,value,ERROR_FLAG=error_flag,_EXTRA=_extra
  ON_ERROR,2  &  error_flag=1
  CATCH,status  &  IF (self->skymap_object::catch(status)) THEN RETURN
  IF (value EQ !NULL) THEN MESSAGE,'Error- input value is empty or !null'
  valid= ['GEO','GEI','GSE','GSM','SM','MAG']
  tmp= STRUPCASE(STRCOMPRESS(value[0],/REMOVE_ALL))
  match= WHERE(tmp EQ valid,nmatch)
  IF (nmatch NE 1) THEN BEGIN
    MESSAGE,'Error- unknown input value: '+tmp,/INFORM  &  RETURN
  ENDIF
  self.system= valid[match]  &  error_flag=0
RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEOSPACE::GET_SYSTEM,ALL=all
  valid= ['GEO','GEI','GSE','GSM','SM','MAG'] 
  IF KEYWORD_SET(ALL) THEN RETURN,valid ELSE RETURN,self.system
END ;#----------------------------------------------------------------------------


PRO SKYMAP_GEOSPACE::SET_VECTOR,value,ERROR_FLAG=error_flag,_EXTRA=_extra
  self->SKYMAP_VECTOR::SET,value,ERROR_FLAG=error_flag,_EXTRA=_extra
RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEOSPACE::GET_VECTOR,system,NO_COPY=no_copy
  ;IF KEYWORD_SET(NO_COPY) THEN tmp=self ELSE tmp=self->SKYMAP_VECTOR::COPY()
  IF (N_PARAMS() GT 0) THEN self->convert,system
  IF KEYWORD_SET(NO_COPY) THEN result=self ELSE result=self->SKYMAP_VECTOR::COPY()
  RETURN,result
END ;#----------------------------------------------------------------------------


PRO SKYMAP_GEOSPACE::SET_TIME,value,ERROR_FLAG=error_flag,_EXTRA=_extra
  ON_ERROR,2  &  error_flag= 1
  CATCH,status & IF (self->skymap_object::catch(status)) THEN RETURN
  ;IF (value EQ !NULL) THEN BEGIN & error_flag=0 & RETURN & ENDIF ;# quietly accept missig value 
  time= SKYMAP_TIME(value,_EXTRA=_extra)
  IF NOT OBJ_VALID(time) THEN MESSAGE,'Error- input time must be valid SKYMAP_TIME'
  IF (time.n_elements() GT 1) THEN MESSAGE,'Error- more than one input time not allowed'
  self.time= time  &  error_flag= 0  &  RETURN
RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEOSPACE::GET_TIME,NO_COPY=no_copy
  IF KEYWORD_SET(NO_COPY) THEN RETURN,self.time ELSE RETURN,self.time->copy()   ;# SKYMAP_TIME object
END ;#----------------------------------------------------------------------------


PRO SKYMAP_GEOSPACE::SET,system,vector,time,ERROR_FLAG=error_flag,_EXTRA=_extra
  self->set_system,system,ERROR_FLAG=error_flag1,_EXTRA=_extra
  self->set_vector,vector,ERROR_FLAG=error_flag2,_EXTRA=_extra
  self->set_time,time,ERROR_FLAG=error_flag3,_EXTRA=_extra    
  error_flag= error_flag1 AND error_flag2 AND error_flag3
RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEOSPACE::MATRIX_PATH,from,into
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  _from= STRMID( STRCOMPRESS(STRUPCASE(from),/REMOVE_ALL) ,0,3 ) ;# not necessary for trusted input? FIXME
  _into= STRMID( STRCOMPRESS(STRUPCASE(into),/REMOVE_ALL) ,0,3 )
  IF (_from EQ _into) THEN RETURN,0   ;indentity transformation

;Determine what transformation ("T") matrices are required.  This is just a 6x6
;lookup table, from which we get a sequence of transformations.  The number indicates
;the transformation matrix (using Hapgood's convention), with 0 just a place keeper.
;
 CASE _from OF
  'GEI': matrices= [ [0,0,0],        [1,0,0],        [2,0,0],       [3,2,0],      [4,3,2],       [5,1,0] ]
  'GEO': matrices= [ [-1,0,0,0],     [0,0,0,0],      [2,-1,0,0],    [3,2,-1,0],   [4,3,2,-1],    [5,0,0,0] ]
  'GSE': matrices= [ [-2,0,0],       [1,-2,0],       [0,0,0],       [3,0,0],      [4,3,0],       [5,1,-2] ]
  'GSM': matrices= [ [-2,-3,0,0],    [1,-2,-3,0],    [-3,0,0,0],    [0,0,0,0],    [4,0,0,0],     [5,1,-2,-3] ]
  'SM':  matrices= [ [-2,-3,-4,0,0], [1,-2,-3,-4,0], [-3,-4,0,0,0], [-4,0,0,0,0], [0,0,0,0,0],   [5,1,-2,-3,-4] ]
  'MAG': matrices= [ [-1,-5,0,0,0],  [-5,0,0,0,0],   [2,-1,-5,0,0], [3,2,-1,-5,0],  [4,3,2,-1,-5], [0,0,0,0,0] ]
 ELSE:MESSAGE,'Parameter From must be one of GEI, GEO, GSE, GSM, SM, MAG'
 ENDCASE

 CASE _into OF
  'GEI': matrices= REFORM( matrices(*,0) )
  'GEO': matrices= REFORM( matrices(*,1) )
  'GSE': matrices= REFORM( matrices(*,2) )
  'GSM': matrices= REFORM( matrices(*,3) )
  'SM' : matrices= REFORM( matrices(*,4) )
  'MAG': matrices= REFORM( matrices(*,5) )
 ELSE:MESSAGE,'Parameter Into must be one of GEI, GEO, GSE, GSM, SM, MAG'
 ENDCASE

 result= matrices( WHERE(matrices NE 0) )         ;throw away any zeros

RETURN,result
END ;#----------------------------------------------------------------------------


;# !!FIXME!! use skymap_vector::rotation_matrix
FUNCTION SKYMAP_GEOSPACE::RMATRIX,angle,axis
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  
;axis is rotation axis, a string of 'X','Y' or 'Z'
;angle is rotation angle in degrees <== should be radians? 
;
;The basic idea is to
;  start with an empty 3x3 matrix
;  put a 1 in the diagonal element corresponding to the rotation axis
;  put cos(zeta) in the other diagonal elements
;  determine the two off-diagonal terms in the same columns and rows as the cos(zeta) values
;   put -sin(zeta) in the term above the diagonal
;   put sin(zeta) in the term below
;
;element #'s       example for a 'Z' rotation
; 0  1  2          cos(zeta)   -sin(zeta)      0
; 3  4  5          sin(zeta)    cos(zeta)      0
; 6  7  8           0               0          1

  sin_zeta= SIN(angle*!dtor)  &  cos_zeta= COS(angle*!dtor)
  fill= [1.0d0,cos_zeta,cos_zeta,-sin_zeta,sin_zeta]
  rmatrix= DBLARR(3,3)
  CASE STRUPCASE(axis) OF
  'X': rmatrix([0,4,8,7,5])= fill
  'Y': rmatrix([4,0,8,6,2])= fill
  'Z': rmatrix([8,0,4,3,1])= fill
  ELSE: BEGIN
         MESSAGE,'AXIS parameter must be string of X, Y, or Z',/INFORM
         rmatrix([0,4,8])= 1.0d0  ;# identity matrix
        END
  ENDCASE

  RETURN,rmatrix
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEOSPACE::MATRIX,from,into,julian_date,ERROR_FLAG=error_flag
  ON_ERROR,2  &  error_flag=1
  CATCH,status  &  IF (self->skymap_object::catch(status)) THEN RETURN,1

  IF (julian_date EQ !NULL) THEN MESSAGE,'Error- invalid input julian_date'
  ;# should do more input checking

  deg2rad= !dpi/180.d0
;  seconds_in_day= 24.0d0 * 60.0d0 * 60.0d0
;  utime= (mjdate MOD seconds_in_day) / 3600.0d0    ;time of day in hours
;  mjdate= LONG(mjdate / seconds_in_day)            ;modified julian date in days, truncated to integer
  mjdate= julian_date - 2400000.5             ;# epoch 00UT 17 November 1858  (Julian date 2400000.5)
  utime= (mjdate MOD 1.0d0) * 24.0
  T_0= (mjdate - 51544.5d0) / 36525.0d0       ;# time in Julian centuries from 12UT 1 January 2000
;  mjdate= LONG(mjdate)

  ;Make a stack for 6 3x3 rotation matrices, only the top five of which will be used.
  ;Use the 0th one as identity to keep consistent with notation in Hapgood
  tstack= DBLARR(3,3,6)  &  FOR indx=0,2 DO tstack[indx,indx,0]= 1.0d0

  matrices= self->matrix_path(from,into)
  w1= TOTAL( ABS(matrices) EQ 1 )
  w2= TOTAL( ABS(matrices) EQ 2 )
  w3= TOTAL( ABS(matrices) EQ 3 )
  w4= TOTAL( ABS(matrices) EQ 4 )
  w5= TOTAL( ABS(matrices) EQ 5 )


; To an accuracy within 0.1 second per century, Greenwich (Mean) Sidereal Time (in hours and decimal parts of an hour) can be calculated as
;  GMST = 18.697374558 + 24.06570982441908 * D ,
; where D is the interval, in UT1 days including any fraction of a day, since 2000 January 1, at 12h UT (interval counted positive if forwards to a later time than the 2000 reference instant), and the result is freed from any integer multiples of 24 hours to reduce it to a value in the range 0-24
  IF (w1 OR w3 or w4) THEN BEGIN
     theta= 100.461d0 + 36000.770d0*T_0 + 15.04107d0*utime      ;# Greenwich mean sidereal time
;     theta= 100.4606d0 + 36000.770d0*T_0 + 15.04107d0*utime      ;# Greenwich mean sidereal time    
     tstack[0,0,1] = self.rmatrix(theta,'Z')      ;GEI2GEO ("T1" in Hapgood, equation 2)
  ENDIF

; Wikipedia:  ε = 84381.448 − 46.84024T − (59 × 10−5)T2 + (1.813 × 10−3)T3, measured in seconds of arc, with T being the time in Julian centuries (that is, 36,525 days) since the ephemeris epoch of 2000 (which occurred on Julian day 2,451,545.0)
  IF (w2 OR w3 OR w4) THEN BEGIN
;     ecliptic_obliquity= 23.439d0 - 0.013d0*T_0
     ecliptic_obliquity= 23.4393d0 - 0.0130d0*T_0
     mean_anomaly= 357.528d0 + 35999.050d0*T_0 + 0.04107d0*utime
     mean_longitude= 280.460d0 + 36000.772d0*T_0 + 0.04107d0*utime
     ecliptic_longitude= mean_longitude + (1.915d0 - 0.0048d0*T_0)*sin(mean_anomaly*!dtor) + 0.020*sin(2*mean_anomaly*!dtor)
     Ez= self.rmatrix(ecliptic_longitude,'Z')
     Ex= self.rmatrix(ecliptic_obliquity,'X')
     tstack[0,0,2] = Ez##Ex  ;# GEI2GSE
  ENDIF

  IF (w3 or w4 or w5) THEN BEGIN   ;# magnetic dipole tilt from IGRF
;    dyear= (mjdate-46066.0d0)/365.25    ;should be centered on 1985
;    phi=     78.8 + 4.283d-2 *dyear
;    lambda=  289.1 - 1.413d-2*dyear

    dyear= (mjdate-51544.0d0)/365.25    ;centered on year 2000
    phi=     78.3506d0 + 0.0463006*dyear + 0.000491889*dyear^2
    lambda=  289.104d0 - 0.0562663*dyear - 0.000960865*dyear^2

    IF (w5) THEN BEGIN
       Ez= self.rmatrix(lambda,'Z')  &  Ey= self.rmatrix(phi-90.0,'Y')
       tstack[0,0,5] = Ey##Ez    ;# GEO2MAG
    ENDIF

  ENDIF

  IF (w3 or w4) THEN BEGIN
     phi_rad= phi*!dtor
     cos_phi_rad= cos(phi_rad)
     lambda_rad= lambda*!dtor
     Qg= [ cos_phi_rad*cos(lambda_rad), cos_phi_rad*sin(lambda_rad), sin(phi_rad) ]
     Qe= REFORM(tstack[*,*,2]) ## ( TRANSPOSE(REFORM(tstack[*,*,1]))##Qg )   ;equation (7)

     IF (w3) THEN BEGIN
        psi= ATAN( Qe(1),Qe(2) )*!radeg
        tstack[0,0,3] = self.rmatrix(-psi,'X')  ;# GSE2GSM
     ENDIF

     IF (w4) THEN BEGIN
        mu= ATAN( Qe(0), SQRT( Qe(1)^2+Qe(2)^2 ) )*!radeg
        tstack[0,0,4]= self.rmatrix(-mu,'Y')    ;# GSM2SM
     ENDIF

  ENDIF

 ;Combine the "T" matrices, do inversions (transpositions) on those matrices that need it
 ;
  nstack= N_ELEMENTS(matrices)
  tmatrix= REFORM(tstack[*,*,0])         ;# identity matrix
  FOR indx= nstack-1,0,-1 DO BEGIN
     temp= REFORM( tstack(*,*,ABS(matrices(indx))) )
     IF ( matrices(indx) LT 0 ) THEN temp= TRANSPOSE(temp)
     tmatrix= temp##tmatrix
  ENDFOR

  RETURN,tmatrix
END ;#----------------------------------------------------------------------------



;# this is simple for a single vector at a single time, but gets tricky for multiple
;# vectors or multiple times or both.  Start with brute force looping and leave any
;# thoughts of caching for *much* later..
PRO SKYMAP_GEOSPACE::CONVERT,into,TMATRIX=tmatrix,ERROR_FLAG=error_flag  ;# TRANSFORM??
  oldsystem= self.system
  self.set_system,into,ERROR_FLAG=error_flag  &  IF (error_flag) THEN RETURN
  IF (self.system EQ oldsystem) THEN RETURN  ;# identity transformation
  mjdate= self.time->get(/julian_day)
  tmatrix= self.matrix(oldsystem,self.system,mjdate)
  *self.vector= tmatrix ## *self.vector
  RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_GEOSPACE::SELFTEST
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  flag= ['[PASS]','[FAIL]']+' '  &  ntest=0  &  nfail=0

;  note= 'Create object from empty (invalid)'
;  x= SKYMAP2_TIME()  &  fail= OBJ_VALID(x)
;  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Create object (valid)'
  x= SKYMAP_GEOSPACE('geo',[6371.2d3,90.0,0.0],SPHERICAL=2,/DEGREES,'20000101T000000',/ISO8601_STRING)  &  fail= ~OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Get system'
  tla= x->get_system()  &  fail= tla NE 'GEO'
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Set system (valid)'
  x->set_system,'gse',ERROR_FLAG=error_flag  &  fail= error_flag
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Set system (invalid)'
  x->set_system,'gsx',ERROR_FLAG=error_flag  &  fail= ~error_flag
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  ;# celestial transformations should be good to single precision (ish)
  y=HASH()  &  x.set_time,'20000101T045700',/ISO8601_STRING
  y['GEO']= [-1.82577 ,  -0.35419 ,  -2.04050]  
  y['GEI']= [ 1.85155 ,   0.17499 ,  -2.04050]
  y['GSE']= [ 0.96501 ,   1.70908 ,  -1.94173]  
  tla='GEO'  &  x.set_system,tla  &  x.set_vector,y[tla]
  FOREACH vec,y,tla DO BEGIN  ;&  IF (tla EQ 'GEO') THEN CONTINUE
    note= 'Convert to '+tla+'  (eps=1d-5)'
    x.convert,tla &  fail= ~x.equal(vec,EPS=1d-5)  
    PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1 
  ENDFOREACH
  
  ;# magnetic transformations agre less well due to improved(?) dipole
  y=HASH()  &  x.set_time,'20000101T045700',/ISO8601_STRING
  y['GEO']= [-1.82577 ,  -0.35419 ,  -2.04050]  
  y['MAG']= [0.14029 ,  -1.84442 ,  -2.04962 ]
  y['GSM']= [ 0.96501 ,   1.83839 ,  -1.81978]  
  y['SM']= [-0.20466  ,  1.83839  , -2.04962 ]
  tla='GEO'  &  x.set_system,tla  &  x.set_vector,y[tla]
  FOREACH vec,y,tla DO BEGIN  &  IF (tla EQ 'GEO') THEN CONTINUE
    note= 'Convert to '+tla+'  (eps=2d-4)'
    x.convert,tla &  fail= ~x.equal(vec,EPS=2d-4)  
    PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1 
  ENDFOREACH  

  ;# try a different time  (should really be *years* different)
  y=HASH()  &  x.set_time,'20000101T000000',/ISO8601_STRING
  y['GEO']= [0.68262 ,  -2.04311  ,  6.89192 ]  
  y['GEI']= [1.89412 ,   1.02594  ,  6.89192 ]
  y['GSE']= [-3.30372 ,   2.49712  ,  5.91512 ]  
  y['MAG']= [0.85108 ,   0.00458  ,  7.17039  ]
  y['GSM']= [-3.30372 ,   0.82850  ,  6.36693 ]  
  y['SM']= [-0.19481 ,   0.82850  ,  7.17039 ]
  tla='GEO'  &  x.set_system,tla  &  x.set_vector,y[tla]
  FOREACH tla,x.get_system(/ALL) DO BEGIN  &  IF (tla EQ 'GEO') THEN CONTINUE
    note= 'Convert to '+tla+'  (eps=2d-4)'
    x.convert,tla &  fail= ~x.equal(y[tla],EPS=2d-4)   
    PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1 
  ENDFOREACH

  RETURN,FIX([nfail,ntest])  ;# ideally, nfail=0
END ;#----------------------------------------------------------------------------
 
 
PRO SKYMAP_GEOSPACE__DEFINE
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace

  RESOLVE_ROUTINE,'SKYMAP_TIME__DEFINE',/COMPILE_FULL_FILE,/NO_RECOMPILE,/EITHER
  RESOLVE_ROUTINE,'SKYMAP_GEOGRAPHIC__DEFINE',/COMPILE_FULL_FILE,/NO_RECOMPILE,/EITHER
    
  void= {SKYMAP_GEOSPACE             $
        ,system:''                   $  ;# GEI, GEO, GSE, GSM, SM, MAG                
        ,time:OBJ_NEW()              $  ;# 'SKYMAP_TIME'        
        ,inherits SKYMAP_GEOGRAPHIC  $
        }
       
RETURN
END ;#----------------------------------------------------------------------------


;cd,'c:/Users/bjackel/Dropbox/work/idl'
x= SKYMAP_GEOSPACE('geo',[6371.2d3,90.0,0.0],SPHERICAL=2,/DEGREES,'20000101T000000',/ISO8601_STRING)
PRINT,x.selftest()

;IDL> x= SKYMAP_GEOSPACE('geo',[6371.2d3,78.35,289.1],SPHERICAL=2,/DEGREES,'20000101T000000',/ISO8601_STRING)
;IDL> print,x.get(spheric=2,/degree)
;       6371200.0
;       78.349998
;      -70.899994
;IDL> x.convert,'mag'
;IDL> print,x.get(spheric=2,/degree)
;       6371200.0
;       89.998994
;      -53.264358
;IDL> x.convert,'geo'
;IDL> print,x.get(spheric=2,/degree)
;       6371200.0
;       78.349998
;      -70.899994



END

;############## CDAWeb
;Formats and units:
;    Day/Time format: YY/MM/DD HH.HHHHH
;    Degrees/Hemisphere format: Decimal degrees with 2 place(s).
;        Longitude 0 to 360, latitude -90 to 90.
;    Distance format: Earth radii with 5 place(s).
;
;polar
;       Time                   GEI (RE)                         GEO (RE)                          GM (RE)                         GSE (RE)                         GSM (RE)                          SM (RE)
;yy/mm/dd hh.hhhhh      X          Y          Z          X          Y          Z          X          Y          Z          X          Y          Z          X          Y          Z          X          Y          Z
;
;00/01/01  0.00000    1.89412    1.02594    6.89192    0.68262   -2.04311    6.89192    0.85108    0.00458    7.17039   -3.30372    2.49712    5.91512   -3.30372    0.82850    6.36693   -0.19481    0.82850    7.17039
;00/01/01  4.95000    1.85155    0.17499   -2.04050   -1.82577   -0.35419   -2.04050    0.14029   -1.84442   -2.04962    0.96501    1.70908   -1.94173    0.96501    1.83839   -1.81978   -0.20466    1.83839   -2.04962
;00/01/01  9.90000    7.25976    1.80121    3.82529   -4.29675    6.12262    3.82529   -7.74501   -2.15036    2.44379   -1.82681    7.70989    2.79316   -1.82681    7.99081    1.84148   -0.86959    7.99081    2.44379
;00/01/01 14.85000    5.14900    1.70083    7.19941    3.11398    4.43939    7.19941   -4.50089    4.35341    6.48277   -3.41217    5.86883    5.92878   -3.41217    5.95795    5.83921   -1.92691    5.95795    6.48277
;00/01/01 19.80000   -0.41009    0.40680    5.17012   -0.07490    0.57276    5.17012   -1.50754    0.10925    4.97787   -2.46367    0.04846    4.58167   -2.46367   -0.92680    4.48722   -1.19400   -0.92680    4.97787
