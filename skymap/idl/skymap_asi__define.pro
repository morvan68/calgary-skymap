;#  Subversion $Id: skymap_asi__define.pro 190 2010-12-12 02:20:57Z bjackel $

;+
; IDL object code for dealing with all-sky image (ASI) instruments.  Requires other SKYMAP objects (ie. geo_aim).  
;
; Examples:
;   skymap_object__define  &  skymap_asi__define 
;  
;   a= skymap_asi()                  ;# create an empty object
;   a.set_location,[1143,64,128.0]   ;# set geographic location to 64N 128E 1143m above sea level 
;    
;   optics= {SKYMAP_ASI_OPTICS}  &  optics.projection[1,0]= 140.0  ;# 140 pixels per radian
;   a.set_optics,optics  &  help,/str,a.get_optics()  &  a.plot_optics
;    
;   ccd= {SKYMAP_ASI_CCD}  &  ccd.size=[512,512]  &  ccd.aspect=1.0  &  ccd.center=[256,256]
;   a.set_ccd,ccd  &  help,/str,a.get_ccd()  &  a.plot_ccd
;-

; Storing pixel locations (x,y) could be done in many different ways
; 1) FLTARR(2,n,m,...)
; 2) FLTARR(n,m,...,2)
; 3) {SKYMAP_PIXEL, x:0.0, y:0.0}
; 4) SKYMAP_PIXEL__DEFINE object    <--- overkill?
; 5) COMPLEXARR(n,m,...)            <--- too sneaky? 


;# IDL defines this implicitly, but we want to also "pass through" ie. if input is valid object, just return it 
FUNCTION SKYMAP_ASI,location,optics,ccd,frame,ERROR_FLAG=error_flag,COPY=copy,_EXTRA=_extra
  classname= 'SKYMAP_ASI'  &  siz= SIZE(value,/STRUCT)
  IF (siz.type EQ 11) && (OBJ_CLASS(location) EQ classname) THEN RETURN,location
 ; IF (siz.type_name EQ 'OBJREF') THEN IF (OBJ_CLASS(value) EQ classname) THEN BEGIN
 ;   IF KEYWORD_SET(COPY) THEN result= value.copy() ELSE result= value
 ;   RETURN,result
 ; ENDIF  
  result= OBJ_NEW(classname,location,optics,ccd,frame,_EXTRA=_extra)  &  error_flag= ~OBJ_VALID(result)
RETURN,result
END

;# called only by OBJ_NEW.  We allow empty input, screen so ->set doesn't complain.
FUNCTION SKYMAP_ASI::INIT,location,optics,ccd,frame,_EXTRA=_extra
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  error_flag1=0  &  error_flag2=0
 ; self.location= SKYMAP_GEOGRAPHIC()  ;# empty input, shouldn't complain
  np= N_PARAMS() ; &  IF (np EQ 0) THEN RETURN,1   ;# empty input is valid
  ;IF (np GE 1) AND (N_ELEMENTS(location) GT 0) THEN self->set_location,location,ERROR_FLAG=error_flag1,_EXTRA=_extra
  error_flag1= ~self->skymap_geo_aim::INIT(location,time,_EXTRA=_extra)
  IF (np GE 3) AND (N_ELEMENTS(optics) GT 0) THEN self->set_optics,optics,ERROR_FLAG=error_flag2,_EXTRA=_extra
  RETURN, (error_flag1 OR error_flag2) EQ 0 ; AND success1 AND success2 
END ;#----------------------------------------------------------------------------


;# called only by OBJ_DESTROY  
PRO SKYMAP_ASI::CLEANUP
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  self->skymap_geo_aim::cleanup   ;# unecessary?
;  IF (HEAP_REFCOUNT(self.aim) EQ 1) THEN OBJ_DESTROY,self.aim    ;# only if object not in use elsewhere
RETURN
END ;#----------------------------------------------------------------------------


;PRO SKYMAP_ASI::SET_LOCATION,value,ERROR_FLAG=error_flag,_EXTRA=_extra  ;# only allow a single location?
;  ON_ERROR,2  &  error_flag=1
;  CATCH,status  &  IF (self.skymap_object::catch(status)) THEN RETURN
;;  loc= SKYMAP_GEOGRAPHIC(value,_EXTRA=_extra)
;;  IF NOT OBJ_VALID(loc) THEN MESSAGE,'Error- input location must be valid SKYMAP_GEOGRAPHIC'
;;  IF (loc.n_elements NE 1) THEN MESSAGE,'Error- more than one input location not allowed'
;   self.location->set,value,ERROR_FLAG=error_flag  &  RETURN
;END ;#----------------------------------------------------------------------------
;
;
;FUNCTION SKYMAP_ASI::GET_LOCATION,COPY=copy
;  RETURN,self.location
;END ;#----------------------------------------------------------------------------


;PRO SKYMAP_ASI::SET_ORIENTATION,yaw,pitch,tilt,ERROR_FLAG=error_flag,_EXTRA=_extra  ;# only allow a single location?
;  ON_ERROR,2  &  error_flag=1
;  CATCH,status  &  IF (self.skymap_object::catch(status)) THEN RETURN
;  
;  vec= SKYMAP_VECTOR()
;  self.utility.local2optics= vec.EULER_MATRIX(yaw, pitch, tilt, /DEGREES)
;  self.optics.orientation= [yaw,pitch,tilt] 
;
;  error_flag=0  &  RETURN
;END ;#----------------------------------------------------------------------------



PRO SKYMAP_ASI::SET_OPTICS,value,ERROR_FLAG=error_flag,_EXTRA=_extra  ;# only allow a single location?
  ON_ERROR,2  &  error_flag=1
  CATCH,status  &  IF (self.skymap_object::catch(status)) THEN RETURN
  
  sname= TAG_NAMES(self.optics,/STRUCTURE_NAME)
  IF (SIZE(value,/SNAME) NE sname) THEN MESSAGE,'Error- input value must be structure of type '+sname 

 ;# Input coefficients for rational polynomial give displacement (pixels) in terms of off-axis angle (degrees).
 ;# Require that a0=0 (numerator) and b0=1 (denominator).
 ;# Simplest case is linear mapping with all other terms equal to zero except a1 (pixels/degree) 
 ;# Direct calculation of r(theta) is easy, but faster to pre-calculate and interpolate.
 ;# Inversion to get theta(r) is hard, so use pre-calculated values for reverse interpolation.

 ; zangle= !dpi/2.0d0 * DINDGEN(183)/180.0d0  ;# 0 to 91 degrees in radians 
  zangle= DINDGEN(183)/2.0d0 *!dtor ;# 0 to 91 degrees in 1/2 degree steps, in radians

  c= value.projection  &  nc= N_ELEMENTS(c[*,0])
;  FOR indx=1,nc-1 DO c[indx,*]= c[indx,*] / !dtor^indx  ;# convert from pixels/degree to pixels/radian 
  c[0,0]= 0  &  c[0,1]= 1  &  value.projection= c  ;by definition

  numer= 0.0d0  &  denom= 0.0d0
  FOR indx=nc-1,0,-1 DO numer= ABS(zangle)*numer + c[indx,0]
  FOR indx=nc-1,0,-1 DO denom= ABS(zangle)*denom + c[indx,1]
  rpixel= numer/denom           ;# rational polynomial 
  
  self.utility.zangle= zangle 
  self.utility.rpixel= rpixel
 
  self.optics= value  &  error_flag=0  &  RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_ASI::GET_OPTICS ;,_EXTRA=_extra
  RETURN,self.optics
END ;#----------------------------------------------------------------------------


PRO SKYMAP_ASI::SET_CCD,value,ERROR_FLAG=error_flag,_EXTRA=_extra  ;# only allow a single location?
  ON_ERROR,2  &  error_flag=1
  CATCH,status  &  IF (self.skymap_object::catch(status)) THEN RETURN

  sname= TAG_NAMES(self.ccd,/STRUCTURE_NAME)
  IF (SIZE(value,/SNAME) NE sname) THEN MESSAGE,'Error- input value must be structure of type '+sname 

  IF (value.size[0] LE 0) THEN MESSAGE,'Error- CCD x-dimension must be greater than 0'
  IF (value.size[1] LE 0) THEN MESSAGE,'Error- CCD y-dimension must be greater than 0'
  IF (value.aspect  LE 0.0) THEN MESSAGE,'Error- CCD aspect ratio must be greater than 0'  
  IF (value.center[0] LT 0) THEN MESSAGE,'Error- CCD x-center must be greater than 0'
  IF (value.center[0] GT (value.size[0]-1)) THEN MESSAGE,'Error- CCD x-center must be less than CCD size'
  IF (value.center[1] LT 0) THEN MESSAGE,'Error- CCD y-center must be greater than 0'
  IF (value.center[1] GT (value.size[1]-1)) THEN MESSAGE,'Error- CCD y-center must be less than CCD size'
  
  self.ccd= value  &  error_flag=0  &  RETURN
END ;#----------------------------------------------------------------------------

FUNCTION SKYMAP_ASI::GET_CCD ;,_EXTRA=_extra
  RETURN,self.ccd
END ;#----------------------------------------------------------------------------


;FUNCTION SKYMAP_ASI::CCD_PIXELS,CORNERS=corners
;  dim= self.ccd.size
;  IF KEYWORD_SET(CORNERS) THEN BEGIN
;    nx= dim[0]+1 & ny=dim[1]+1
;    x= LINDGEN(nx) # REPLICATE(1L,ny)
;    y= REPLICATE(1L,nx) # LINDGEN(ny)
;  ENDIF ELSE BEGIN ;# centers
;    nx= dim[0] & ny=dim[1]
;    x= LINDGEN(nx) # REPLICATE(1L,ny) + 0.5
;    y= REPLICATE(1L,nx) # LINDGEN(ny) + 0.5
;  ENDELSE
;
;;RETURN,TRANSPOSE([[[x]],[[y]]],[2,0,1]) ;# 2,nx,ny = 2, ncolumns, nrows
;RETURN,[[[x]],[[y]]] ;# nx,ny,2 = ncolumns, nrows, 2
;END ;#----------------------------------------------------------------------------




;# GET/SET FRAME
;# GET/SET TIME
;
;FUNCTION SKYMAP_ASI::GET_UTILITY
;  COMPILE_OPT HIDDEN    ;# should not be seen by most users
;  RETURN,self.utility
;END ;#----------------------------------------------------------------------------


;FUNCTION SKYMAP_ASI::CCD2OPTICS,ccd_pixels
;;  COMPILE_OPT HIDDEN    ;# should not be seen by most users
;
;  IF (N_PARAMS() EQ 0) THEN xy= self->get_.pixels(/CCD) ELSE xy= ccd_pixels
;;# check for out of range pixels?
;
;  siz= SIZE(xy,/STRUCTURE)
;  m= siz.dimensions[siz.n_dimensions-1]  &  IF (m NE 2) THEN MESSAGE,'Boom'
;  n= siz.n_elements/2                    &  IF (n EQ 0) THEN MESSAGE,''
;  tmp= REFORM(xy,n,2)  &  dims= siz.dimensions[0:siz.n_dimensions-2] 
;   
;  vec= DBLARR(n,3)  &  vec[*,0]= 1.0d0  ;# dummy radius
;  x= tmp[*,0] - self.ccd.center[0]
;  y= tmp[*,1] - self.ccd.center[1]  &  y /= self.ccd.aspect
;  r= SQRT(x^2 + y^2)
;  
;  u= self.utility
;  vec[0,1]= INTERPOL(u.zangle,u.rpixel,r,/QUADRATIC)     ;# zenith angle
;  bad= vec[*,1] GE (!dpi/2.0)*1.1
;
;  aangle= ATAN(x,-y) & IF (self.optics.reflection) THEN aangle= aangle - !dpi
;  vec[0,2]= aangle  ;  & surface,reform(bad,512,512)
;
;  wbad= WHERE(bad, nbad) &  IF (nbad GT 0) THEN vec[wbad,0]= !values.d_nan 
;  vec= REFORM(vec,[dims,3])
;  result= SKYMAP_VECTOR(vec,SPHERICAL=4,DEGREES=0)  ;# r, za, az in radians
;  RETURN,result
;END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_ASI::CCD2OPTICS,ccd_pixels
  COMPILE_OPT HIDDEN    ;# don't clutter up namespace

  IF (N_PARAMS() EQ 0) THEN pix= self->get_pixels(/CCD) ELSE pix= ccd_pixels ;!!TODO!! check input {skymap_asi_pixel}
;# check for out of range pixels?

  siz= SIZE(pix,/STRUCTURE)  &  dims= siz.dimensions[0:siz.n_dimensions-1]
  pix= REFORM(pix,siz.n_elements) 
      
  x= pix.x - self.ccd.center[0]
  y= pix.y - self.ccd.center[1]  &  y /= self.ccd.aspect
  rpixel= SQRT(x^2 + y^2)

  vec= DBLARR(siz.n_elements,3)  &  vec[*,0]= 1.0d0   ;# dummy radius

  ;# Do reverse interpolation to find angle (degrees) from radius (pixels).
  ;# ?might be faster to do linear interpolation with denser look-up table? 
  u= self.utility  &  dtor= !dpi/180.0d0
  vec[0,1]= INTERPOL(u.zangle,u.rpixel,rpixel,QUADRATIC=1)     ;# off-axis angle in radians
  bad= vec[*,1] GE (!dpi/2.0)*1.1

  aangle= ATAN(y,x) & IF (self.optics.reflection) THEN aangle= aangle - !dpi
  vec[0,2]= aangle  ;#  azimuth (zonal) angle

  wbad= WHERE(bad, nbad) &  IF (nbad GT 0) THEN vec[wbad,0]= !values.f_nan 
  vec= REFORM(vec,[dims,3])
  result= SKYMAP_VECTOR(vec,SPHERICAL=1,DEGREES=0)  ;# r, za, az in radians
  RETURN,result
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_ASI::OPTICS2CCD,aim_vector
  vec= SKYMAP_VECTOR(aim_vector)  ;# input must either be vector or convertable to one
  IF NOT OBJ_VALID(vec) THEN MESSAGE,'Error- input aim_vector must be valid object'
  nvec= vec.n_elements()  &  dim= vec.dimensions(/NO_ZERO) 
  vec= vec.get(SPHERICAL=1,DEGREES=0,/NO_REFORM)   ;# radians, Nx3     

  u= self.utility  &  dtor= !dpi/180.0
  zangle= REFORM(vec[*,1])    ;# off-axis angle (radians)
  rpixel= INTERPOL(u.rpixel,u.zangle,zangle,/QUADRATIC) ;# reverse lookup: radians to pixels
  aangle= REFORM(vec[*,2]) & IF (self.optics.reflection) THEN aangle= aangle + !dpi

  result= REPLICATE({SKYMAP_ASI_PIXEL},nvec)
  result.x= self.ccd.center[0] + COS(aangle)*rpixel
  result.y= self.ccd.center[1] + SIN(aangle)*rpixel*self.ccd.aspect
  
  bad= (result.x LT 0.0) OR (result.x GE self.ccd.size[0]) OR $
       (result.y LT 0.0) OR (result.y GE self.ccd.size[1]) OR $
       (FINITE(result.x) NE 1) OR (zangle GT MAX(u.zangle))
  where_bad= WHERE(bad,nbad)
  IF (nbad GT 0) THEN result[where_bad].x= !values.f_nan

RETURN,REFORM(result,dim)
END ;#----------------------------------------------------------------------------




;#===========================================================================
;# The image frame is obtained by selecting a rectangular subregion of the CCD, 
;# possibly applying binning, and possibly flipping x and/or y on readout:
;#
;#  offset= x0,y0        pixels relative to CCD origin (0,0)
;#  size= nx,ny          number of ccd pixels in exposure
;#  binning= bx,by       ratio of CCD to image pixels
;#  img_flip=[ 0 or 1, 0 or 1]
;#
;# to produce an image frame of dimensions [nx/bx, ny/by]

PRO SKYMAP_ASI::SET_FRAME,value,ERROR_FLAG=error_flag,_EXTRA=_extra  ;# only allow a single location?
  ON_ERROR,2  &  error_flag=1
  CATCH,status  &  IF (self.skymap_object::catch(status)) THEN RETURN

  sname= TAG_NAMES(self.frame,/STRUCTURE_NAME) ;& stop
  IF (SIZE(value,/SNAME) NE sname) THEN MESSAGE,'Error- input value must be structure of type '+sname 

  IF (value.size[0] LE 0) THEN MESSAGE,'Error- Frame x-dimension must be greater than 0'
  IF (value.size[1] LE 0) THEN MESSAGE,'Error- Frame y-dimension must be greater than 0'  

  IF (value.offset[0] LT 0) THEN MESSAGE,'Error- Frame x-offset must be greater than 0'
  IF (value.offset[1] LT 0) THEN MESSAGE,'Error- Frame y-offset must be greater than 0'

  IF (value.binning[0] LE 0) THEN MESSAGE,'Error- Frame x-binning must be greater than 0'
  IF (value.binning[1] LE 0) THEN MESSAGE,'Error- Frame y-binning must be greater than 0'
; also check if binning too large...

  xy= value.offset + value.size
  IF (xy[0] GT self.ccd.size[0]) THEN MESSAGE,'Error- Frame x-dimension greater than CCD size'
  IF (xy[1] GT self.ccd.size[1]) THEN MESSAGE,'Error- Frame y-dimension greater than CCD size'
 
  self.frame= value  &  error_flag=0  &  RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_ASI::GET_FRAME ;,_EXTRA=_extra
  RETURN,self.frame
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_ASI::FRAME2CCD,frame_pixels

  IF (N_PARAMS() EQ 0) THEN pix= self->pixel_grid() ELSE pix= frame_pixels 

  siz= SIZE(pix,/STRUCTURE)  ;&  dims= siz.dimensions[0:siz.n_dimensions-1]
  IF (siz.type_name NE 'STRUCT') || (siz.structure_name NE 'SKYMAP_ASI_PIXEL') THEN $
    MESSAGE,'Error- input must be of type {SKYMAP_ASI_PIXEL}'  
 
  ;bad= (pix.x LT 0) OR (pix.y LT 0)  ;# check for out of range pixels?  Expensive...

  IF (self.frame.flip[0]) THEN pix.x= (self.frame.size[0]-1) - pix.x
  IF (self.frame.flip[1]) THEN pix.y= (self.frame.size[1]-1) - pix.y

  ; convert from image pixels to CCD device pixels
  pix.x= pix.x * self.frame.binning[0] + self.frame.offset[0]
  pix.y= pix.y * self.frame.binning[1] + self.frame.offset[1]

  ;where_bad= WHERE(bad,nbad)
  ;IF (nbad GT 0) THEN pix[wbad,*]= !values.d_nan
  
RETURN,pix
END ;#----------------------------------------------------------------------------

;# convert from CCD device pixels to image frame pixels (shift, bin, flip) 
FUNCTION SKYMAP_ASI::CCD2FRAME,ccd_pixels
  IF (N_PARAMS() EQ 0) THEN pix= self->get_pixels(/CCD) ELSE pix= ccd_pixels 

  siz= SIZE(pix,/STRUCTURE)  ;&  dims= siz.dimensions[0:siz.n_dimensions-1]
  IF (siz.type_name NE 'STRUCT') OR (siz.structure_name NE 'SKYMAP_ASI_PIXEL') THEN $
    MESSAGE,'Error- input must be of type {SKYMAP_ASI_PIXEL}' 

  ;# ignore errors from operations on NaN pixels (out of FOV)
  ;# "Program caused arithmetic error: Floating illegal operand"
  ;void= CHECK_MATH(PRINT=0) & _except=!except & !except=0

  ;bad_input= (pix.x LT 0) OR (pix.y LT 0)  ;# don't bother checking input, only output

  pix.x= FLOAT(pix.x - self.frame.offset[0] ) / self.frame.binning[0] 
  pix.y= FLOAT(pix.y - self.frame.offset[1] ) / self.frame.binning[1]

  IF (self.frame.flip[0]) THEN pix.x= (self.frame.size[0]-1) - pix.x
  IF (self.frame.flip[1]) THEN pix.y= (self.frame.size[1]-1) - pix.y

  bad_output= ~FINITE(pix.x) OR $
              (pix.x LT 0.0) OR (pix.x GT self.frame.size[0]) OR $
              (pix.y LT 0.0) OR (pix.y GT self.frame.size[1])

  wbad= WHERE(bad_output,nbad)
  IF (nbad GT 0) THEN pix[wbad].x = !values.f_nan

  ;void= CHECK_MATH(PRINT=0) & !except= _except
RETURN,pix
END ;#----------------------------------------------------------------------------



;# frame pixel locations -> ccd pixels -> internal device orientation
PRO SKYMAP_ASI::SET_PIXEL,frame_pixels,ERROR_FLAG=error_flag
  pix= self->frame2ccd(frame_pixels)
  vec= self->ccd2optics(pix) ; &  stop
  self->skymap_geo_aim::set_aim,vec,/VECTOR,/DEVICE,ERROR_FLAG=error_flag
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_ASI::GET_PIXEL ;,ERROR_FLAG=error_flag
  vec= self->skymap_geo_aim::get_aim(/VECTOR,/DEVICE)
  ccd= self->optics2ccd(vec)
  pix= self->ccd2frame(ccd)
  RETURN,pix
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_ASI::PIXEL_GRID,CORNERS=corners,CCD=ccd

  IF KEYWORD_SET(CCD) THEN dim= self.ccd.size ELSE dim= self.frame.size

  IF KEYWORD_SET(CORNERS) THEN BEGIN
    nx= dim[0]+1 & ny=dim[1]+1
    x= LINDGEN(nx) # REPLICATE(1L,ny)
    y= REPLICATE(1L,nx) # LINDGEN(ny)
  ENDIF ELSE BEGIN ;# centers
    nx= dim[0] & ny=dim[1]
    x= LINDGEN(nx) # REPLICATE(1L,ny) + 0.5
    y= REPLICATE(1L,nx) # LINDGEN(ny) + 0.5
  ENDELSE

  result= REPLICATE({SKYMAP_ASI_PIXEL},nx,ny)
  result.x=x  &  result.y=y

RETURN,result
END ;#----------------------------------------------------------------------------


;# Simple figure of mapping from off-axis angle to pixel distance from center
PRO SKYMAP_ASI::PLOT_OPTICS,_EXTRA=_extra
  COMPILE_OPT HIDDEN    ;# should not be seen by most users

  PLOT,self.utility.zangle/!dtor,self.utility.rpixel,_EXTRA=_extra $
    ,XTITLE='Angle (degrees)',XRANGE=[0,90],XSTYLE=1,XTICKLEN=1,XGRIDSTYLE=1 $
    ,YTITLE='Displacement (pixels)',YTICKLEN=1,YGRIDSTYLE=1
   
  c= self.optics.projection  &  nc= N_ELEMENTS(c[*,0])
  POLYFILL,[0.59,0.59,0.92,0.92],[0.15,0.5,0.5,0.15],COLOR=!p.background,/NORMAL  
  FOR indx=0,nc-1 DO BEGIN
    yy= 0.45-(0.3*indx)/nc
    XYOUTS,0.6,yy,c[indx,0],/NORMAL,ALIGN=0
    XYOUTS,0.75,yy,c[indx,1],/NORMAL,ALIGN=0
  ENDFOR  
    
RETURN
END ;#----------------------------------------------------------------------------


PRO SKYMAP_ASI::PLOT_CCD,_EXTRA=_extra
  COMPILE_OPT HIDDEN    ;# should not be seen by most users
 
  dim= self.ccd.size

  PLOT,[0,dim[0]],[0,dim[1]],/NODATA,_EXTRA=_extra $
    ,XTITLE='CCD x-pixel',XRANGE=[0,dim[0]],XSTYLE=1 $
    ,YTITLE='CCD y-pixel',YRANGE=[0,dim[1]],YSTYLE=1
   
  PLOTS,!x.crange,self.ccd.center[0]*[1,1],THICK=2,LINESTYLE=1
  PLOTS,self.ccd.center[1]*[1,1],!y.crange,THICK=2,LINESTYLE=1
  
  pix= self->pixel_grid(/ccd)  &   q= self.ccd2optics(pix) 
  v= q.get(spherical=1,/degrees) &  zenith_angle=v[*,*,1] 
  contour,zenith_angle,levels=[30,45,60,75,90],/OVERPLOT,/FOLLOW  
  
  vec= SKYMAP_VECTOR([1.0,25.0,0.0],SPHERICAL=1,/DEGREES)  &  pix= self.optics2ccd(vec)
  arrow,self.ccd.center[0],self.ccd.center[1],pix.x,pix.y,/DATA,/SOLID,THICK=4  ;# North?

  vec= SKYMAP_VECTOR([1.0,25.0,90.0],SPHERICAL=1,/DEGREES)  &  pix= self.optics2ccd(vec)
  arrow,self.ccd.center[0],self.ccd.center[1],pix.x,pix.y,/DATA,THICK=2  ;# East?

RETURN
END ;#----------------------------------------------------------------------------



FUNCTION SKYMAP_ASI::SELFTEST
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  flag= ['[PASS]','[FAIL]']+' '  &  ntest=0  &  nfail=0

  note= 'Create empty object'
  a= SKYMAP_ASI()  &  fail= ~OBJ_VALID(a)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
 
  note= 'Set location'
  a.set_location,[1143,64,128.0],ERROR_FLAG=error_flag  &  fail= error_flag NE 0
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
 
  note= 'Set optics'
  optics= {SKYMAP_ASI_OPTICS}  &  optics.projection[1,0]= 140.0  & optics.projection[1,1]= 1.1
  a.set_optics,optics,ERROR_FLAG=error_flag  &  fail= error_flag NE 0
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  help,/str,a.get_optics()
  a.plot_optics

  optics= {SKYMAP_ASI_OPTICS}  &  optics.projection[*]= 0.0  &  optics.projection[1,0]= 140.0
  a.set_optics,optics

  note= 'Set CCD'
  ccd= {SKYMAP_ASI_CCD}  &  ccd.size=[512,512]  &  ccd.aspect=1.0  &  ccd.center=[256,256]
  a.set_ccd,ccd,ERROR_FLAG=error_flag  &  fail= error_flag NE 0
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
  
  help,/str,a.get_ccd()

  note= 'optics -> CCD -> optics (zenith)'
  vec= SKYMAP_VECTOR([1.0,25.0,0.0],SPHERICAL=4,/DEGREES)  &  pix= a.optics2ccd(vec)
  tmp= a.ccd2optics(pix)  &  fail= ~vec.equal(tmp,EPS=1d-6)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'optics -> CCD -> optics (mixed)'
  vec= SKYMAP_VECTOR([1.0,33.0,13.0],SPHERICAL=4,/DEGREES)  &  pix= a.optics2ccd(vec)
  tmp= a.ccd2optics(pix)  &  fail= ~vec.equal(tmp,EPS=1d-6)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1 
  
  
  note= 'Set frame'
  frame= {SKYMAP_ASI_FRAME}  &  frame.size=[256,256]  &  frame.offset=[0,0]  &  frame.binning=[2,2]
  a.set_frame,frame,ERROR_FLAG=error_flag  &  fail= error_flag NE 0
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
  
  note= 'frame -> CCD -> frame'
  pix= {SKYMAP_ASI_PIXEL} & pix.x=9  &  pix.y=131
  tmp1= a.FRAME2CCD(pix)  &  tmp2= a.CCD2FRAME(tmp1)
  fail= (pix.x NE tmp2.x) OR (pix.y NE tmp2.y)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'pixel -> aim -> pixel'
  pix= {SKYMAP_ASI_PIXEL} & pix.x=127  &  pix.y=171
  a.set_pixel,pix  &  tmp= a.get_pixel()
  fail= (pix.x NE tmp.x) OR (pix.y NE tmp.y)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  
  RETURN,FIX([nfail,ntest])  ;# ideally, nfail=0
END ;#----------------------------------------------------------------------------
  

PRO SKYMAP_ASI__DEFINE
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace

  RESOLVE_ROUTINE,'SKYMAP_GEOGRAPHIC__DEFINE',COMPILE_FULL_FILE=0,NO_RECOMPILE=1
  RESOLVE_ROUTINE,'SKYMAP_GEOSPACE__DEFINE',COMPILE_FULL_FILE=0,NO_RECOMPILE=1
  RESOLVE_ROUTINE,'SKYMAP_GEO_AIM__DEFINE',COMPILE_FULL_FILE=0,NO_RECOMPILE=1
 
  
  void= {SKYMAP_ASI_OPTICS         $
 ;       ,orientation:DBLARR(3)     $  ;# Euler angles
        ,projection:DBLARR(9,2)    $  ;# {SKYMAP_RPOLY}
        ,reflection:0b             $
        }
  
  void= {SKYMAP_ASI_PIXEL, x:0.0, y:0.0}
  
  void= {SKYMAP_ASI_CCD         $
        ,size:UINTARR(2)        $  
        ,aspect:0.0d0           $
        ,center:DBLARR(2)       $  ;# change to pixel struct?  
        }

  void= {SKYMAP_ASI_FRAME         $
        ,size:UINTARR(2)          $  
        ,offset:UINTARR(2)        $
        ,binning:UINTARR(2)       $
        ,flip:BYTARR(2)           $  
        }

  void= {SKYMAP_ASI_UTILITY        $
 ;       ,local2optics:DBLARR(3,3)      $  ;# rotate local to optics  <-- now in geo_aim?
        ,zangle:DBLARR(183)        $
        ,rpixel:DBLARR(183)        $
        }
  
  void= {SKYMAP_ASI                    $
 ;       ,location:OBJ_NEW()            $  ;# {SKYMAP_GEOGRAPHIC}
        ,optics:{SKYMAP_ASI_OPTICS}    $
        ,ccd:{SKYMAP_ASI_CCD}          $
        ,frame:{SKYMAP_ASI_FRAME}      $
        ,utility:{SKYMAP_ASI_UTILITY}  $
;        ,direction:OBJ_NEW()          $  ;# {SKYMAP_GEOSPACE}
        ,inherits SKYMAP_GEO_AIM       $                
        }
        
RETURN
END ;#----------------------------------------------------------------------------



a= SKYMAP_ASI()
PRINT,a.selftest()

STOP
a.set_location,[1143,64,128.0]

optics= {SKYMAP_ASI_OPTICS}  &  optics.projection[*]= 0.0  &  optics.projection[1,0]= 140.0
a.set_optics,optics

ccd= {SKYMAP_ASI_CCD}  &  ccd.size=[512,512]  &  ccd.aspect=1.0  &  ccd.center=[256,256]
a.set_ccd,ccd  ;&  help,/str,a.get_ccd()  &  a.plot_ccd

frame= {SKYMAP_ASI_FRAME}  &  frame.size=[256,256]  &  frame.offset=[0,0]  &  frame.binning=[2,2]
a.set_frame,frame

pix= {SKYMAP_ASI_PIXEL} & pix.x=127  &  pix.y=171
a.set_pixel,pix  &  tmp= a.get_pixel()

pix= REPLICATE({SKYMAP_ASI_PIXEL},2) & pix.x=[127,128]  &  pix.y=[171,172]
a.set_pixel,pix  &  tmp= a.get_pixel()

pix= REPLICATE({SKYMAP_ASI_PIXEL},2,3) & pix.x=131  &  pix.y=165
a.set_pixel,pix  &  tmp= a.get_pixel()

a->skymap_geo_aim::set_time,'20120618T180000',/ISO
a->skymap_geo_aim::set_aim,[10.0,90.0],/DEGREES,/ASTRO
a->skymap_geo_aim::set_aim,[(2/24.0 + 31/60.0/24.0)*360,89+15/60.0],/DEGREES,/ASTRO
tmp= a.get_pixel()  &  print,tmp

;# good benchmark: time to transform all image pixels to RaDec (currently 0.06 seconds for 256x256)
;# profiler shows no hot-spots: a few at 10% (6 ATAN, 1 INTERPOL, 1 FRAME2CCD) and a bunch of 5%
pix= a->pixel_grid()
st= systime(1)
;profiler & profiler,/reset & profiler,/system
a.set_pixel,pix  &  tmp= a.get_aim(/astro,/degree)
;profiler,/report
print,systime(1)-st


stop
xy= a.ccd_pixels()
q= a.ccd2optics(xy)
v= q.get(spherical=4,/degrees) & help,v
za=v[*,*,1]  &  contour,za,levels=[30,60,90]

END