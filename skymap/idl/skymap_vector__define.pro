;# Subversion $Id$

;+
;
;
; Examples:
;
;   v= SKYMAP_VECTOR()                ;# create empty vector
;   v= SKYMAP_VECTOR([1,2,3])         ;# create single vector
;
;   v= SKYMAP_VECTOR(v)               ;# check if valid vector
;   IF NOT OBJ_VALID(v) THEN MESSAGE,'Error- "v" is not a valid vector'
;
;   u= SKYMAP_VECTOR(v,/COPY)         ;# if vector then copy, otherwise create
;-

;# why Nx3 is good?
;IDL> help,fltarr(3,3)#[1,2,3]
;<Expression>    FLOAT     = Array[3]
;IDL> help,fltarr(3,3)##[1,2,3]
;<Expression>    FLOAT     = Array[1, 3]
;
;# to do:
;#  -get keyword inheritance working
;#  -further strict input error checking


;# IDL defines this implicitly, but we want to also "pass through": if input is valid object, just return it. 
FUNCTION SKYMAP_VECTOR,value,ERROR_FLAG=error_flag,COPY=copy,_EXTRA=_extra
  error_flag=1
  siz= SIZE(value,/STRUCT)
  classname= ((SCOPE_TRACEBACK(/STRUCTURE))[-1]).routine ; & classname= 'SKYMAP_VECTOR'
  IF (siz.type_name EQ 'OBJREF') THEN IF (OBJ_CLASS(value) EQ classname) THEN BEGIN
    IF KEYWORD_SET(COPY) THEN result= value.copy() ELSE result= value
    error_flag=0  &  RETURN,result
  ENDIF
  result= OBJ_NEW(classname,value,_EXTRA=_extra)  &  error_flag= ~OBJ_VALID(result)
RETURN,result
END


;# called only by OBJ_NEW
FUNCTION SKYMAP_VECTOR::INIT,value,_EXTRA=_extra
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (value NE !NULL) THEN self->set,value,ERROR_FLAG=error_flag,_EXTRA=_extra ELSE error_flag=0
  RETURN,~error_flag
END ;#----------------------------------------------------------------------------


;# called only by OBJ_DESTROY
PRO SKYMAP_VECTOR::CLEANUP
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (HEAP_REFCOUNT(self.vector) EQ 1) THEN PTR_FREE,self.vector
  self.idl_object::cleanup
RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_VECTOR::_overloadPrint 
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (self.n_elements GT 1) THEN BEGIN
    d= self.dimensions(/NO_ZERO) ; &  d= d[WHERE(d NE 0)]
    tmp= STRING(d,FORMAT='("[",8(I,:,","))')+']'
    tmp= STRCOMPRESS(tmp,/REMOVE_ALL)
  ENDIF ELSE tmp= '   '+ STRING(self.get(),FORMAT='(3(G,X))')

  RETURN,'OBJREF = '+OBJ_CLASS(self)+tmp
END ;#----------------------------------------------------------------------------



;# called only by SIZE() and N_ELEMENTS()
FUNCTION SKYMAP_VECTOR::_overloadSize
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF NOT PTR_VALID(self.vector) THEN RETURN,0L ELSE $
  RETURN,self.dimensions[WHERE(self.dimensions NE 0)]  ;# FIXME: use self.dimensions(/NO_ZERO)
END ;#----------------------------------------------------------------------------


;# 
FUNCTION SKYMAP_VECTOR::_overloadEQ,left,right
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (SIZE(left,/TYPE) NE 11) OR (SIZE(right,/TYPE) NE 11) THEN RETURN,0
  IF NOT OBJ_ISA(left,OBJ_CLASS(self)) THEN RETURN,0
  IF NOT OBJ_ISA(right,OBJ_CLASS(self)) THEN RETURN,0    
  RETURN,TOTAL(self,3) ;BOOM
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_VECTOR::_overloadBracketsRightSide,isRange,sub1,sub2,sub3,sub4,sub5,sub6,sub7,sub8
   IF (MAX(isRange) GT 0) THEN BEGIN
      RETURN, 'Subscript Ranges are not allowed'
   ENDIF
   result= SKYMAP_VECTOR(self.vector[sub1,*])
   stop
  RETURN,result
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_VECTOR::N_ELEMENTS
  RETURN,self.n_elements
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_VECTOR::DIMENSIONS,NO_ZERO=no_zero
  result= self.dimensions 
  IF KEYWORD_SET(NO_ZERO) THEN result=result[WHERE(result NE 0)]
  RETURN,result
END ;#----------------------------------------------------------------------------

PRO SKYMAP_VECTOR::REFORM,dimensions
;  ;#COMPILE_OPT HIDDEN
  d= ULONG(dimensions[WHERE(dimensions NE 0)])
  n= PRODUCT(d,/INTEGER)
  IF (n NE self.n_elements) THEN MESSAGE,'Error- result must have the same number of elements'
  self.dimensions= d
END ;#----------------------------------------------------------------------------



;# really should do ratio of magnitudes, but what happens if denominator is zero? 
FUNCTION SKYMAP_VECTOR::EQUAL,vector,EPS=EPS
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  vec= SKYMAP_VECTOR(vector)  &  IF NOT OBJ_VALID(vec) THEN RETURN,0
  IF NOT KEYWORD_SET(EPS) THEN eps=1.0d-9
  m1= self.magnitude()  &  m2= vec.magnitude()  &  dot= self.dot_product(vec) / (m1*m2)
  rat= m1/m2  &  rat= (ABS(1.0-rat) LE eps) OR FINITE(rat,/NAN)  ;# 0.0/0.0=-NaN
  RETURN,(ABS(m1-m2) LE eps) AND (dot GE (1.0d0-eps))
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_VECTOR::MAGNITUDE,METHOD=method,NO_REFORM=no_reform
  ;#COMPILE_OPT HIDDEN
  IF NOT PTR_VALID(self.vector) THEN RETURN,!values.d_nan
  ;result= 0.0d0  &  FOR indx=0,2 DO result += (*self.vector[*,indx])^2
  result= TOTAL(*self.vector^2,2)
  IF NOT KEYWORD_SET(NO_REFORM) THEN result= REFORM(result,self.dimensions[WHERE(self.dimensions NE 0)],/OVERWRITE)
  IF (self.n_elements EQ 1) THEN result= result[0] ;# array[1] to scalar  
  RETURN,SQRT(result)
END ;#----------------------------------------------------------------------------

;PRO SKYMAP_VECTOR::NORMALIZE


FUNCTION SKYMAP_VECTOR::GET,SPHERICAL=spherical,DEGREES=degrees,POINTER=pointer,NO_REFORM=no_reform
  ;#COMPILE_OPT HIDDEN
  
  IF (self.vector EQ !NULL) THEN BEGIN
    MESSAGE,'Error- vector is empty or undefined',/INFORMATIONAL
    RETURN,REPLICATE(!values.d_nan,3)
  ENDIF
  
  IF KEYWORD_SET(POINTER) THEN RETURN,self.vector  ;# pointer to the Nx3 vector
 
  result= *self.vector
  IF KEYWORD_SET(SPHERICAL) THEN BEGIN
    r= SQRT(TOTAL(result^2,2))  ;# AKA self->(magnitude)
    theta= ACOS(result[*,2]/r)          &  IF ((spherical AND 2) NE 0) THEN theta= !dpi/2 - theta
    phi= ATAN(result[*,1],result[*,0])  &  IF ((spherical AND 4) NE 0) THEN phi= !dpi/2 - phi 
    ;phi= ATAN(result[*,1],result[*,0])  ;# longitude
    ;theta= ACOS(result[*,2]/r)          ;# latitude
    ;IF (spherical EQ 2) THEN theta= !dpi/2 - theta  ;# co-latitude to latitude
    IF KEYWORD_SET(DEGREES) THEN BEGIN
      rtod= 180.0d0 / !dpi  &  theta *= rtod  &  phi *= rtod 
    ENDIF
    result= [[r],[theta],[phi]]
  ENDIF ELSE IF KEYWORD_SET(DEGREES) THEN MESSAGE,'Warning- keyword SPHERICAL not set',/INFORM
  
  IF NOT KEYWORD_SET(NO_REFORM) THEN result= REFORM(result,[self.dimensions(/NO_ZERO),3],/OVERWRITE)  
  RETURN,result
END ;#----------------------------------------------------------------------------


PRO SKYMAP_VECTOR::SET,value,ERROR_FLAG=error_flag,NO_COPY=no_copy,TRANSPOSE=transpose,SPHERICAL=spherical,DEGREES=degrees,VERBOSE=verbose
  ;#COMPILE_OPT HIDDEN
  ON_ERROR,2  &  error_flag=1
  CATCH,status  &  IF (self->skymap_object::catch(status)) THEN RETURN
 
  IF NOT KEYWORD_SET(verbose) THEN verbose=0 ELSE verbose= FIX(verbose[0])
 
  IF (value EQ !NULL) THEN BEGIN
    MESSAGE,'Warning- input value was NULL',/INFORMATIONAL
    error_flag=0  &  RETURN
  ENDIF
    
  siz= SIZE(value,/STRUCTURE)
  CASE (siz.type) OF
  4: BREAK  ;# FLOAT
  5: BREAK  ;# DOUBLE  
  0: BEGIN
       MESSAGE,'Warning- input value is undefined',/INFORMATIONAL
       error_flag=0  &  RETURN
     END
  ;11: IF OBJ_ISA(value,OBJ_CLASS(self)) THEN BEGIN ;# !!FIXME!! fails for subclass ie. SKYMAP_VECTOR::SET: Error- input object must be of class SKYMAP_GEOSPACE, was SKYMAP_VECTOR
  ;# 'OBJREF' , copy (overwrite) self with value
  ;11: IF OBJ_ISA(value,'SKYMAP_VECTOR') THEN BEGIN 
  11: IF OBJ_ISA(OBJ_CLASS(self),OBJ_CLASS(value)) THEN BEGIN
        self->skymap_vector::set,(value->skymap_vector::get()),ERROR_FLAG=error_flag & RETURN
      ENDIF ELSE MESSAGE,'Error- input object must be of class '+OBJ_CLASS(self)+', was '+OBJ_CLASS(value)
  10: IF PTR_VALID(value) THEN BEGIN 
        self.set,*value,ERROR_FLAG=error_flag & RETURN 
      ENDIF ELSE MESSAGE,'Error- input was invalid pointer'
  8:  IF TAG_NAMES(value,/STRUCTURE_NAME) EQ OBJ_CLASS(self) THEN self=value ELSE MESSAGE,'Error- unrecognized structure'
  2:  IF (verbose) THEN MESSAGE,'Note- input value of type INTEGER, casting to double',/INFORM
  3:  IF (verbose) THEN MESSAGE,'Note- input value of type LONG, casting to double',/INFORM         
  ELSE:MESSAGE,'Warning- unexpected input type: '+siz.type_name,/INFORMATIONAL
  ENDCASE
  
;  IF (siz.type_name EQ 'COMPLEX') THEN MESSAGE,'Boom 6'
;  IF (siz.type_name EQ 'STRUCTURE') THEN MESSAGE,'Boom 8'  
;  IF (siz.type_name EQ 'DCOMPLEX') THEN MESSAGE,'Boom 9'
 
  IF (siz.n_elements LT 3) THEN MESSAGE,'Error- input value must have at least 3 elements'
  IF ((siz.n_elements MOD 3L) NE 0) THEN MESSAGE,'Error- input value must be Nx3 array'
  self.n_elements= siz.n_elements / 3L
  
  vec= PTR_NEW(DOUBLE(value),NO_COPY=no_copy)
  IF NOT PTR_VALID(vec) THEN MESSAGE,'Error- could not assign input value to pointer'
  
  IF KEYWORD_SET(TRANSPOSE) THEN siz= SIZE((*vec= TRANSPOSE(*vec)),/STRUCTURE)
  IF (siz.dimensions[siz.n_dimensions-1] NE 3) THEN MESSAGE,'Error- value must be Nx3 array (try /TRANSPOSE)' ;# ,/INFORM
    
  self.dimensions= 0
  IF (siz.n_dimensions LE 1) THEN self.dimensions[0]= 1 ELSE self.dimensions= siz.dimensions[0:siz.n_dimensions-2] 
  *vec= REFORM(*vec,siz.n_elements/3L,3,/OVERWRITE)  ;# store internally as Nx3

  ;IF KEYWORD_SET(SPHERICAL) THEN BEGIN  ;# [r,theta,phi] to [x,y,z]
  IF (spherical NE !NULL) && (spherical NE 0) THEN BEGIN  ;# [r,theta,phi] to [x,y,z]
    IF KEYWORD_SET(DEGREES) THEN dtor=!dpi/180.0 ELSE dtor=1.0d0
    theta= (*vec)[*,1] * dtor   &   phi= (*vec)[*,2] * dtor  &  r= (*vec)[*,0]
    IF ((spherical AND 2) NE 0) THEN theta= !dpi/2 - theta  ;# co-latitude to latitude
    IF ((spherical AND 4) NE 0) THEN phi= !dpi/2 - phi  ;# co-azimuth to azimuth
    ctheta= COS(theta) & stheta= SIN(theta) 
    x= r * stheta * COS(phi)
    y= r * stheta * SIN(phi)
    z= r * ctheta 
    *vec= [[x],[y],[z]]
  ENDIF ELSE IF KEYWORD_SET(DEGREES) THEN MESSAGE,'Warning- keyword SPHERICAL not set',/INFORM
  
  self.vector= vec  &  error_flag=0  
  RETURN   
END ;#----------------------------------------------------------------------------



FUNCTION SKYMAP_VECTOR::COPY  ;#,scalar_value,OVERWRITE=overwrite 
  ;RETURN,SKYMAP_VECTOR(self->skymap_vector::get())  ;# 0.164 seconds for 1x3 999 times
  result= REFORM(*self.vector,[self.dimensions(/NO_ZERO),3])
  RETURN,SKYMAP_VECTOR(result)  ;# 0.03 seconds, worth optimizing(?)
END ;#----------------------------------------------------------------------------


PRO SKYMAP_VECTOR::SCALAR_MULTIPLY,scalar_value,ERROR_FLAG=error_flag
  error_flag=1
  ns= N_ELEMENTS(scalar_value)  &  nv= self.n_elements
  
  IF NOT ((ns EQ nv) OR (ns EQ 1) OR (nv EQ 1)) THEN BEGIN
    MESSAGE,'Error- vector sizes must be identical, or one must have only a single element',/INFORM
    RETURN
  END  
  
  dim= self.dimensions 
  IF (ns EQ 1) THEN *self.vector *= scalar_value[0] ELSE $
  IF (nv EQ 1) THEN BEGIN
    vec= *self.vector   &  result= DBLARR(ns,3)
    FOR indx=0,2 DO result[0,indx]= (vec[*,indx])[0] * scalar_value
    *self.vector= result  &  self.n_elements= ns>nv
    self.reform,SIZE(scalar_value,/DIMENSIONS) 
  ENDIF ELSE IF (ns EQ nv) THEN BEGIN
    vec= *self.vector
    FOR indx=0,2 DO vec[0,indx]= vec[*,indx] * scalar_value
    *self.vector= vec
  ENDIF ELSE MESSAGE,'Error- input scalar value must be of length 1 or N' 
  
  error_flag=0
  RETURN
END ;#----------------------------------------------------------------------------


PRO SKYMAP_VECTOR::MATRIX_MULTIPLY,matrix3x3,ERROR_FLAG=error_flag
  ON_ERROR,2  &  error_flag=1
  CATCH,status  &  IF (self->skymap_object::catch(status)) THEN RETURN
  ;IF (matrix3x3 EQ !NULL) THEN MESSAGE,'Error: input is empty or undefined'
  ;siz= SIZE(matrix3x3,/STRUCTURE)  ;# could also check that det=1 or orthogonal
  ;IF TOTAL(siz.dimensions NE [3,3,0,0,0,0,0,0]) THEN MESSAGE,'Error: input must be 3x3 rotation matrix'
  *self.vector = matrix3x3 ## TEMPORARY(*self.vector)   ;# "temporary" has no effect on execution time ?!
  error_flag=0
RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_VECTOR::DOT_PRODUCT,value,ANGLE=angle,DEGREES=degrees

  vector= SKYMAP_VECTOR(value)
  IF NOT OBJ_VALID(vector) THEN BEGIN
    MESSAGE,'Error- input value must be valid vector (Nx3)',/INFORM
    RETURN,!values.d_nan
  ENDIF  
  
  n1= self.n_elements  &  n2= vector->n_elements() ;N_ELEMENTS(tmp)
  IF NOT ((n1 EQ n2) OR (n1 EQ 1) OR (n2 EQ 1)) THEN BEGIN
    MESSAGE,'Error- vector sizes must be identical, or one must have only a single element',/INFORM
    RETURN,!values.d_nan
  END
  
  vec1= *self.vector  &  vec2= *(vector->get(/POINTER)) 
;  dot= 0.0d0  &  FOR indx=0,2 DO dot += vec1[*,indx] * vec2[*,indx]  ;# array[n] * array[1] = array[1] ?!!
  IF (n1 EQ n2) THEN BEGIN
    dot= TOTAL(vec1*vec2,2) 
  ENDIF ELSE IF (n2 EQ 1) THEN BEGIN
    dot= 0.0d0  &  FOR indx=0,2 DO dot += vec1[*,indx] * (vec2[0,indx])[0]
  ENDIF ELSE IF (n1 EQ 1) THEN BEGIN
    dot= 0.0d0  &  FOR indx=0,2 DO dot += (vec1[0,indx])[0] * vec2[*,indx]
  ENDIF ELSE MESSAGE,'This should never happen'
  
  IF KEYWORD_SET(ANGLE) THEN BEGIN
    dot= dot / (self.magnitude()*vector.magnitude())  ;# requres that magnitude() casts array[1] to scalars  
    dot= ACOS((-1.0d0)>dot<(+1.0d0))
    IF KEYWORD_SET(DEGREES) THEN dot *= (180.0d0/!dpi)
  ENDIF

  RETURN,dot 
END ;#----------------------------------------------------------------------------


PRO SKYMAP_VECTOR::CROSS_PRODUCT,vector,ERROR_FLAG=error_flag
  ON_ERROR,3  &  error_flag=1

  tmp= SKYMAP_VECTOR(vector)
  IF NOT OBJ_VALID(tmp) THEN BEGIN
    MESSAGE,'Error- input value must be valid vector (Nx3)',/INFORM
    RETURN
  ENDIF  
  
  n1= self.n_elements  &  n2= tmp->n_elements() ;N_ELEMENTS(tmp)
  IF NOT ((n1 EQ n2) OR (n1 EQ 1) OR (n2 EQ 1)) THEN BEGIN
    MESSAGE,'Error- vector sizes must be identical, or one must have only a single element',/INFORM
    RETURN
  END
  
  vec1= *self.vector  &  vec2= *(tmp->get(/POINTER))  &  vec3= DBLARR(n1>n2,3)

  IF (n1 EQ n2) THEN BEGIN
    vec3[0,0]=  vec1[*,1]*vec2[*,2] - vec1[*,2]*vec2[*,1]
    vec3[0,1]= -vec1[*,0]*vec2[*,2] + vec1[*,2]*vec2[*,0]
    vec3[0,2]=  vec1[*,0]*vec2[*,1] - vec1[*,1]*vec2[*,0] 
    dimensions= self.dimensions(/NO_ZERO) 
  ENDIF ELSE IF (n2 EQ 1) THEN BEGIN
    vec3[0,0]=  vec1[*,1]*vec2[2] - vec1[*,2]*vec2[1]
    vec3[0,1]= -vec1[*,0]*vec2[2] + vec1[*,2]*vec2[0]
    vec3[0,2]=  vec1[*,0]*vec2[1] - vec1[*,1]*vec2[0]   
    dimensions= self.dimensions(/NO_ZERO)
  ENDIF ELSE IF (n1 EQ 1) THEN BEGIN
    vec3[0,0]=  vec1[1]*vec2[*,2] - vec1[2]*vec2[*,1]
    vec3[0,1]= -vec1[0]*vec2[*,2] + vec1[2]*vec2[*,0]
    vec3[0,2]=  vec1[0]*vec2[*,1] - vec1[1]*vec2[*,0]   
    dimensions= tmp.dimensions(/NOZERO)
  ENDIF

  *self.vector= vec3
  self.n_elements= n1>n2
  self.reform,dimensions
  
  error_flag=0
  RETURN 
END ;#----------------------------------------------------------------------------


;# Rotation around an arbitrary axis passing through the origin.
;# There is an alternative approach using a (fairly complicated) 3x3 rotation matrix,
;# but this is relatively straightforward and also exercises several utility functions.
PRO SKYMAP_VECTOR::ROTATE,rotation_angle,axis_vector,DEGREES=degrees,ERROR_FLAG=error_flag
  ON_ERROR,3  &  error_flag=1
  
  IF (N_PARAMS() LT 2) THEN MESSAGE,'Error- two input values required: rotation_angle and axis_vector"
  theta= rotation_angle  &  IF KEYWORD_SET(DEGREES) THEN theta= theta * !dtor  ;# !! check: non-empty, numeric, scalar?

  n= SKYMAP_VECTOR(axis_vector,/COPY)  &  IF NOT OBJ_VALID(n) THEN MESSAGE,'Error- input axis_vector must be valid vector'
  IF (n.n_elements() NE 1) THEN MESSAGE,'Error- input axis_vector must be a single vector'
  n.scalar_multiply,1.0/n.magnitude()   ;# normalize to unit vector

  u= self.copy()
  u.cross_product,n              ;# gives vXn, want nXv ...
  u.scalar_multiply,-SIN(theta)  ;# ...so flip sign

  dot= self.dot_product(n)      ;# v.n
  n.scalar_multiply,dot         ;# now really v_parallel
 
  v= self.get(/NO_REFORM) - n.get(/NO_REFORM)  ;# v_perpendicular
  v= SKYMAP_VECTOR(v)
  v.scalar_multiply,COS(theta)-1.0
  
  *self.vector= *self.vector + v.get() + u.get()

  error_flag=0
  RETURN
END ;#----------------------------------------------------------------------------


;angle is rotation angle in radians 
;axis is rotation axis, a string of 'X','Y' or 'Z'
FUNCTION SKYMAP_VECTOR::ROTATION_MATRIX,angle,axis_name,DEGREES=degrees
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

  zeta= DOUBLE(angle)  &  IF KEYWORD_SET(DEGREES) THEN zeta= zeta * !dpi/180.0d0
  sin_zeta= SIN(zeta[0])  &  cos_zeta= COS(zeta[0])
  fill= [1.0d0,cos_zeta,cos_zeta,-sin_zeta,sin_zeta]
  rmatrix= DBLARR(3,3)
  CASE STRUPCASE(axis_name[0]) OF
  'X': rmatrix([0,4,8,7,5])= fill
  'Y': rmatrix([4,0,8,6,2])= fill
  'Z': rmatrix([8,0,4,3,1])= fill
  ELSE: BEGIN
         MESSAGE,'Input AXIS_NAME parameter must be string of X, Y, or Z',/INFORM
         rmatrix([0,4,8])= 1.0d0  ;# identity matrix
        END
  ENDCASE

  ;# allow vector input eg. print,x.rotation_matrix(replicate(1,90),'z',/degrees)
  nz= N_ELEMENTS(zeta)-1  &  na= N_ELEMENTS(axis_name)-1
  FOR indx=1,(nz>na) DO rmatrix= self.rotation_matrix(zeta[indx<nz],axis_name[indx<na]) ## rmatrix

  RETURN,rmatrix
END ;#----------------------------------------------------------------------------


;# !! TEST ME !!
FUNCTION SKYMAP_VECTOR::EULER_MATRIX,alpha_angle,beta_angle,gamma_angle,DEGREES=degrees
  RETURN,self.rotation_matrix([alpha_angle,beta_angle,gamma_angle],['z','x','z'],DEGREES=degrees)
END ;#----------------------------------------------------------------------------


;# y.REGULAR_GRID([15,10],[90,70],[5.0,10,20],/degrees) ;5 degrees clockwise, 10 towards north pole, 20 to positive longitude
;#
FUNCTION SKYMAP_VECTOR::REGULAR_GRID,angles,steps,rotations,DEGREES=degrees
  lat= (FINDGEN(steps[0])/(steps[0]-1.0) - 0.5 ) * angles[0] & lat= lat # REPLICATE(1,steps[1])
  lon= (FINDGEN(steps[1])/(steps[1]-1.0) - 0.5 ) * angles[1] & lon= REPLICATE(1,steps[0]) # lon
  one= lat*0 + 1
  vec= SKYMAP_VECTOR([[[one]],[[lat]],[[lon]]],SPHERICAL=2,DEGREES=degrees)
  rot= vec.rotation_matrix(-rotations,['X','Y','Z'],DEGREES=degrees)
  vec.matrix_multiply,rot
  RETURN,vec
END ;#----------------------------------------------------------------------------



;# Goal is to test each method under a range of inputs.
;# !!RUN THIS AFTER ANY CODE CHANGE!
FUNCTION SKYMAP_VECTOR::SELFTEST
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  flag= ['[PASS]','[FAIL]']+' '  &  ntest=0  &  nfail=0

  note= 'Create vector from empty (valid)'
  x= SKYMAP_VECTOR()  &  fail= ~OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Create vector from scalar (invalid)'
  x= SKYMAP_VECTOR(9.0)  &  fail= OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1  
  
  note= 'Create vector from [0.0,1.0,2.0] (valid)'
  x= SKYMAP_VECTOR([0.0,1.0,2.0])  &  fail= ~OBJ_VALID(x) 
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Create vector from fltarr(1,3) (valid)'
  x= SKYMAP_VECTOR(FLTARR(1,3))  &  fail= ~OBJ_VALID(x) 
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Create vector from fltarr(3,1) (acceptable)'
  x= SKYMAP_VECTOR(FLTARR(3,1))  &  fail= ~OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Create vector from fltarr(3,2) (invalid)'
  x= SKYMAP_VECTOR(FLTARR(3,2))  &  fail= OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
  
  note= 'Create vector from fltarr(3,2) with /TRANSPOSE (valid)'
  x= SKYMAP_VECTOR(FLTARR(3,2),/TRANSPOSE)  &  fail= ~OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Test pass-through (valid)'
  y= SKYMAP_VECTOR(x)  &  fail= ~OBJ_VALID(y)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
  
  v= skymap_vector([0,0,0]) & u= skymap_vector([1,2,3])
  x= skymap_vector([1,0,0]) & y= skymap_vector([0,1,0])  &  z= skymap_vector([0,0,1]) 
  a= skymap_vector([[0,0,0],[1,0,0],[0,1,0],[0,0,1]],/TRANSPOSE)
  b= skymap_vector([[0,0,0],[1,0,0],[0,2,0],[0,3,4]],/TRANSPOSE)


  note= 'scalar multiply'  
  u.scalar_multiply,0.5  &  fail= ~u.equal([0.5,1.0,1.5],EPS=1.0d-9)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'magnitude'  
  fail= TOTAL(b.magnitude() EQ [0,1,2,5]) NE 4 
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= '->equal() (valid)'  
  fail= ~x.equal(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
  
  note= '->equal() (invalid)'  
  fail= x.equal(y)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1  

  note= '->equal() array'  
  fail= TOTAL(x.equal(a) EQ [0,1,0,0]) NE 4
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1 

  note= 'Test dot product'  
  c= x.dot_product(a)  &  fail= TOTAL(c EQ [0.0, 1.0, 0.0, 0.0]) NE 4
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1 
  
  note= 'Test cross product (valid)'  
  x.cross_product,y  &  fail= ~x.equal(z)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1   

  RETURN,FIX([nfail,ntest])  ;# ideally, nfail=0
END ;#----------------------------------------------------------------------------
 
;FUNCTION SKYMAP_VECTOR::_overloadAsterisk, Left, Right
;  result=0
;
;  IF OBJ_ISA(Left,OBJ_CLASS(self)) AND OBJ_ISA(Right,OBJ_CLASS(self)) THEN $
;    RETURN,Left->dot_product(Right) 
;
;  IF OBJ_ISA(Left,OBJ_CLASS(self)) THEN BEGIN
;    IF (N_ELEMENTS(Right) EQ 1) THEN RETURN,Left->scalar_multiply(Right) ELSE BEGIN
;      MESSAGE,'Error- right-hand value in vector multiply must be a scalar'  &  HELP,/STR,Right  &  RETURN,0
;    ENDELSE
;  ENDIF 
;  
;  RETURN,0
;END ;#----------------------------------------------------------------------------

PRO SKYMAP_VECTOR__DEFINE
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  
  RESOLVE_ROUTINE,'SKYMAP_OBJECT__DEFINE',COMPILE_FULL_FILE=0,/EITHER,/NO_RECOMPILE  ;# shouldn't be necessary
  
  void= {SKYMAP_VECTOR                 $
        ,n_elements:0UL                $
        ,dimensions:ULONARR(8)         $
        ,vector:PTR_NEW()              $ ;# N x 3
        ,inherits Skymap_Object        $ ;# for operator overloading etc.
        }
RETURN
END ;#----------------------------------------------------------------------------



y= skymap_vector()
print,y->selftest()

;x= skymap_vector([1,0,2])
;x.rotate,90,/degrees,[0,0,1] & print,x

;
;stop
; a= skymap_vector([[1,0,0],[0,1,0],[0,0,1]])
; b= skymap_vector([0,1,0])
; c= a.cross_product(b)
; HELP,c
;
;stop
;
; PROFILER,/CLEAR  &  PROFILER,/RESET  &  PROFILER  &   PROFILER,/SYSTEM
; vec= OBJ_NEW('skymap_vector',[[1,2,3],[1,2,2]],/TRANSPOSE)
; dot= vec.dot_product([1,2,3],/angle,/degrees)
; PROFILER,/REPORT
;
; PROFILER,/CLEAR  &  PROFILER,/RESET  &  PROFILER  &   PROFILER,/SYSTEM
; vec= SKYMAP_VECTOR(DBLARR(9999,3))
; dot= vec.dot_product([1,2,3])
; PROFILER,/REPORT
; 
END