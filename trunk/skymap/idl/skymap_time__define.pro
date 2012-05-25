;#  Subversion $Id: skymap_time__define.pro 190 2010-12-12 02:20:57Z bjackel $
;+
; Store and convert time value(s).  Gather together all(?) IDL time conversion tools.
; 
;
; Examples:
;  
;   x= skymap_time(0,/UNIX)           ;# seconds since 00:00:00 January 01, 1970
;    
;   print,x.get(/ISO8601)
;   
;   x.set,'1970-01-01T00:00:00',ISO8601=2
;-

;# -use double precision everywhere
;# to do: add CDF to selftest
;#        selftest multiple dimensions
;#        selftest overloads

;# IDL defines this implicitly, but we want to also "pass through": if input is valid object, just return it. 
FUNCTION SKYMAP_TIME,value,ERROR_FLAG=error_flag,COPY=copy,_EXTRA=_extra
  classname= 'SKYMAP_TIME'  &  siz= SIZE(value,/STRUCT)
;  IF (siz.type EQ 11) && (OBJ_CLASS(value) EQ classname) THEN RETURN,value
  IF (siz.type_name EQ 'OBJREF') THEN IF (OBJ_CLASS(value) EQ classname) THEN BEGIN
    IF KEYWORD_SET(COPY) THEN result= value.copy() ELSE result= value
    RETURN,result
  ENDIF  
  result= OBJ_NEW(classname,value,_EXTRA=_extra)  &  error_flag= ~OBJ_VALID(result)
RETURN,result
END


;# called only by OBJ_NEW
FUNCTION SKYMAP_TIME::INIT,value,_EXTRA=_extra
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (N_PARAMS() EQ 0) OR (SIZE(value,/TYPE) EQ 0) THEN error_flag=0  $ 
    ELSE self->set,value,ERROR_FLAG=error_flag,_EXTRA=_extra 
  RETURN,~error_flag
END ;#----------------------------------------------------------------------------


;# called only by OBJ_DESTROY
PRO SKYMAP_TIME::CLEANUP
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (HEAP_REFCOUNT(self.time) EQ 1) THEN PTR_FREE,self.time  
  self.idl_object::cleanup
RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_TIME::N_ELEMENTS
  RETURN,self.n_elements
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_TIME::DIMENSIONS,NO_ZERO=no_zero
  result=self.dimensions 
  IF KEYWORD_SET(NO_ZERO) THEN result=result[WHERE(result NE 0)]
  RETURN,result
END ;#----------------------------------------------------------------------------


;# called only by SIZE() and N_ELEMENTS()
FUNCTION SKYMAP_TIME::_overloadSize
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  RETURN,self.dimensions(/NO_ZERO)
END ;#----------------------------------------------------------------------------


;# was overloadHelp, but that complicates debugging
FUNCTION SKYMAP_TIME::_overloadPrint
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (self.n_elements GT 1) THEN BEGIN
    d= self.dimensions  &  d= d[WHERE(d NE 0)]
    tmp= STRING(d,FORMAT='("[",8(I,:,","))')+']'
    tmp= STRCOMPRESS(tmp,/REMOVE_ALL)
  ENDIF ELSE BEGIN
    tmp= self.get(iso8601=2) ; &  IF (tmp EQ !NULL) THEN tmp= "NULL" 
    tmp= '   '+STRING(tmp)
  ENDELSE
  
  RETURN,'OBJREF = '+OBJ_CLASS(self)+tmp
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_TIME::_overloadEQ,left,right,EPS=eps
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (SIZE(left,/TYPE) NE 11) OR (SIZE(right,/TYPE) NE 11) THEN RETURN,0  
  IF NOT OBJ_ISA(left,OBJ_CLASS(self)) THEN RETURN,0
  IF NOT OBJ_ISA(right,OBJ_CLASS(self)) THEN RETURN,0
  IF NOT KEYWORD_SET(EPS) THEN eps= 1d-3  ;# 1 millisecond resolution
  result= ABS(left.y2k() - right.y2k()) LE eps
  RETURN,result
END ;#----------------------------------------------------------------------------


;#=====!!DO NOT CALL DIRECTLY (use get/set methods) SUBJECT TO CHANGE!!=========
;# Utility routines that either 
;# 1) convert one type of input to internal format, resurn 1 for success
;# 2) return internal state in particular format

;# seconds since 00:00:00 January 01 2000 currently used for internal storage
;#  e2000= 63113904000000.0D0 ;# CDF_EPOCH,E2000,2000,1,1,0,0,0,/COMPUTE  &  PRINT,E2000,FORMAT='(F25.8)'
FUNCTION SKYMAP_TIME::Y2K,value,NO_COPY=no_copy
  COMPILE_OPT HIDDEN        ;# private utility for internal use only
  IF (N_PARAMS() EQ 0) THEN RETURN,*self.time  ;# no conversion required

  ;# convert input to internal and store
  tmp= PTR_NEW(DOUBLE(value),NO_COPY=no_copy) ;# need to check type of input
  self.time= tmp  & success=1
  RETURN,success
END ;#----------------------------------------------------------------------------



FUNCTION SKYMAP_TIME::CDF,value,NO_COPY=no_copy
  COMPILE_OPT HIDDEN        ;# private utility for internal use only
  e2000= 63113904000000.0D0 ;# CDF_EPOCH,E2000,2000,1,1,0,0,0,/COMPUTE  &  PRINT,E2000,FORMAT='(F25.8)'

  IF (N_PARAMS() EQ 0) THEN BEGIN
    result= (*self.time)*1000 + e2000     ;# shift epoch and convert from seconds to milliseconds
    RETURN,result 
  ENDIF

  ;# convert input to internal and store
  tmp= PTR_NEW(DOUBLE(value),NO_COPY=no_copy) ;# need to check type of input
  *tmp= (*tmp-e2000)/1000     ;# shift epoch and convert from milliseconds to seconds
  self.time= tmp  & success=1
  RETURN,success
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_TIME::JULIAN,value,NO_COPY=no_copy,MODIFIED=modified
  COMPILE_OPT HIDDEN        ;# private utility for internal use only
  e2000= 2451544.5          ;# PRINT,JULDAY(1,1,2000,0,0,0)
  mjdate= 2400000.5         ;# 00:00 UT on 17 November 1858

  IF (N_PARAMS() EQ 0) THEN BEGIN
     result= (*self.time)/86400 + e2000     ;# convert from seconds to days and shift epoch
     IF KEYWORD_SET(MODIFIED) THEN result= result - mjdate
     RETURN,result 
  ENDIF

  ;# convert input to internal and store
  tmp= PTR_NEW(DOUBLE(value),NO_COPY=no_copy) ;# need to check type of input
  IF KEYWORD_SET(MODIFIED) THEN *tmp= *tmp + mjdate
  *tmp= (*tmp-e2000)*86400     ;# shift epoch and convert from seconds to days
  self.time= tmp  & success=1
  RETURN,success
END ;#----------------------------------------------------------------------------



FUNCTION SKYMAP_TIME::YMDHMS,value,NO_COPY=no_copy
  COMPILE_OPT HIDDEN        ;# private utility for internal use only
  
  IF (N_PARAMS() EQ 0) THEN BEGIN
    cdf_epoch= self.cdf()
    result= UINTARR(6,self.n_elements)
    FOR indx=0L,self.n_elements-1L DO BEGIN
      CDF_EPOCH,cdf_epoch[indx],year,month,day,hour,minute,second,/BREAKDOWN
      result[0,indx]= [year,month,day,hour,minute,second]
    ENDFOR
    RETURN,result  
  ENDIF

  ;# convert input to CDF
  tmp= DBLARR(self.n_elements)
  FOR indx=0L,self.n_elements-1L DO BEGIN
    v= value[*,indx]
    CDF_EPOCH,cdf_epoch,v[0],v[1],v[2],v[3],v[4],v[5],/COMPUTE
    tmp[indx]= cdf_epoch
  ENDFOR
  success= self.cdf(tmp)  ;# convert CDF to internal

  RETURN,success
END ;#----------------------------------------------------------------------------



FUNCTION SKYMAP_TIME::ISO8601,value,EXTENDED_FORMAT=extended_format
  COMPILE_OPT HIDDEN        ;# private utility for internal use only

  IF (N_PARAMS() EQ 0) THEN BEGIN
     result= STRARR(self.n_elements)
     ymdhms= self.ymdhms()
     IF KEYWORD_SET(EXTENDED_FORMAT) THEN format='(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' $
        ELSE format='(I4.4,I2.2,I2.2,"T",I2.2,I2.2,I2.2)'
     FOR indx=0L,self.n_elements-1L DO BEGIN
       result[indx]= STRING(ymdhms,FORMAT=format)
     ENDFOR
     RETURN,result
  ENDIF

 ; IF KEYWORD_SET(EXTENDED_FORMAT) THEN format='(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)' $
 ;       ELSE format='(I4.4,I2.2,I2.2,"T",I2.2,I2.2,I2.2)'
 ; READS,value[indx],ymdhms,FORMAT=format  ;# TO DO: add ON_IOERROR

  ;# convert input to YMDHMS using slow but robust regular expressions
  tmp= DBLARR(6,self.n_elements)  &  ymdhms= FLTARR(6)
  regexp= '([0-9]{4})\-?([0-9]{2})\-?([0-9]{2})T([0-9]{2}):?([0-9]{2}):?([0-9]{2})'
  FOR indx=0L,self.n_elements-1L DO BEGIN
    ymdhms= STREGEX(value[indx],regexp,/SUBEXPR,/EXTRACT) 
    IF (ymdhms[0] NE '') THEN tmp[0,indx]= ymdhms[1:6] ;ELSE COMPLAIN !!FIXME!!
  ENDFOR
  success= self.ymdhms(tmp)   ;# convert YMDHMS to internal

  RETURN,success
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_TIME::UNIX,value,NO_COPY=no_copy,STRING=string
  COMPILE_OPT HIDDEN        ;# private utility for internal use only
  e2000= 946684800.000 ;# t= skymap2_time([1970,1,1,0,0,0],/ymdhms) & PRINT,-t.y2k(),FORMAT='(F15.3)'

  IF (N_PARAMS() EQ 0) THEN BEGIN
     result= (*self.time) + e2000     ;# shift epoch 
     IF KEYWORD_SET(STRING) THEN BEGIN
       tmp= result &  result= STRING(tmp)
       FOR indx=0,self.n_elements-1 DO result[indx]= SYSTIME(0,tmp[indx],/UTC)
     ENDIF
     RETURN,result 
  ENDIF

  ;# convert input to internal and store
  tmp= PTR_NEW(DOUBLE(value),NO_COPY=no_copy) ;# need to check type of input
  *tmp= (*tmp-e2000)     ;# shift epoch 
  self.time= tmp  & success=1
  RETURN,success
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_TIME::ISA_LEAPYEAR,year
  IF (N_PARAMS() GT 0) THEN tmp=year ELSE BEGIN
    tmp= self.ymdhms()  &  tmp= tmp[0,*]
  ENDELSE
  result= ((tmp MOD 4) EQ 0) AND ( ((tmp MOD 100) NE 0) OR ((tmp MOD 400) EQ 0) ) 
;  result= ((tmp MOD 4) EQ 0) && ( ((tmp MOD 100) NE 0) || ((tmp MOD 400) EQ 0) ) ;# lazy evaluation fails for arrays
  RETURN,result 
END ;#----------------------------------------------------------------------------


;# Day of week: 0=Monday to 6=Sunday
FUNCTION SKYMAP_TIME::DAY_OF_WEEK,NAME=name
  e2000= 5 ;# t= skymap2_time([2000,1,1,0,0,0],/ymdhms) & PRINT,t.unix(/string) ;# Saturday
  result= (*self.time)/86400 + e2000     ;# seconds to days and shift epoch
  result= (FIX(result MOD 7) + 7) MOD 7
  IF KEYWORD_SET(NAME) THEN BEGIN
    day_name=['Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday']
    result= day_name[result]
  ENDIF
  RETURN,result 
END ;#----------------------------------------------------------------------------

;# Day of year: 1= January 1st
FUNCTION SKYMAP_TIME::DAY_OF_YEAR
  tmp= self.ymdhms() 
  tmp[1,*]=1 & tmp[2,*]=1 & tmp[3,*]=0 & tmp[4,*]=0 & tmp[5,*]=0
  tmp= SKYMAP_TIME(tmp,/YMDHMS) ;# January 1
  diff= self.y2k() - tmp.y2k()  ;# should be GE 0
  OBJ_DESTROY,tmp    ;# should happen automatically
  doy= diff / 86400
  RETURN,FIX(doy)+1 
END ;#----------------------------------------------------------------------------




PRO SKYMAP_TIME::SET,value,ERROR_FLAG=error_flag,NO_COPY=no_copy  $
       ,CDF_MILLISECONDS=cdf_milliseconds,Y2K_SECONDS=y2k_seconds,JULIAN_DAY=julian_day  $
       ,ISO8601_STRING=iso8601_string,YMDHMS_ARRAY=ymdhms_array,UNIX_SECONDS=unix_seconds

  ON_ERROR,2  &  error_flag=0
  CATCH,status  &  IF (self.catch(status)) THEN RETURN  

  siz= SIZE(value,/STRUCTURE)
  IF (siz.type EQ 0) THEN MESSAGE,'Error- input value is undefined'
  
  IF (siz.type EQ 11) THEN BEGIN
    IF OBJ_ISA(value,OBJ_CLASS(self)) THEN BEGIN ;# 'OBJREF' , copy (overwrite) self with value
       self.set,value.get(/Y2K),/Y2K,ERROR_FLAG=error_flag & RETURN
    ENDIF ELSE MESSAGE,'Error- input object must be of class '+OBJ_CLASS(self)+', was '+OBJ_CLASS(value)
  ENDIF
  
  IF TOTAL(siz.type EQ [6,9]) THEN MESSAGE,'Error- input value is complex'

  IF (siz.n_elements LT 1) THEN MESSAGE,'Error- value must have at least 1 element'
  self.n_elements= siz.n_elements

  self.dimensions= 0
;  IF (siz.n_dimensions LE 1) THEN self.dimensions[0]=1 ELSE self.dimensions= siz.dimensions[0:siz.n_dimensions-2] 
  IF (siz.n_dimensions LT 1) THEN self.dimensions[0]=1 ELSE self.dimensions= siz.dimensions[0:siz.n_dimensions-1]

  IF  KEYWORD_SET(Y2K_SECONDS) THEN BEGIN  
    tmp= REFORM(value,self.n_elements)  
    error_flag= ~self.y2k(tmp)
    RETURN
  ENDIF

  IF KEYWORD_SET(CDF_MILLISECONDS) THEN BEGIN
     IF NOT (siz.type EQ 5) THEN MESSAGE,'Error- input CDF must be double precision, was: '+siz.type_name
     tmp= REFORM(value,self.n_elements)  
     error_flag= ~self.cdf(tmp,/NO_COPY)
     RETURN
  ENDIF

  IF KEYWORD_SET(JULIAN_DAY) THEN BEGIN
     IF NOT TOTAL(siz.type EQ [4,5]) THEN MESSAGE,'Error- input JULIAN must be floating point, was: '+siz.type_name
     tmp= REFORM(value,self.n_elements)  
     error_flag= ~self.julian(tmp,MODIFIED=(julian_day GT 1))
     RETURN
  ENDIF

  IF KEYWORD_SET(YMDHMS_ARRAY) THEN BEGIN
     ;IF NOT TOTAL(siz.type EQ [4,5]) THEN MESSAGE,'Error- input JULIAN must be floating point, was: '+siz.type_name
     IF (siz.dimensions[0] NE 6) THEN MESSAGE,'Error- input YMDHMS must have 6 elements'
     self.n_elements= siz.n_elements / 6
     IF (self.n_elements EQ 1) THEN self.dimensions[0]=1 ELSE self.dimensions= self.dimensions[1:*]     
     tmp= REFORM(value,6,self.n_elements)  
     error_flag= ~self.ymdhms(tmp)
     RETURN
  ENDIF

  IF KEYWORD_SET(UNIX_SECONDS) THEN BEGIN
  ;   IF NOT TOTAL(siz.type EQ [4,5]) THEN MESSAGE,'Error- input JULIAN must be floating point, was: '+siz.type_name
     tmp= REFORM(value,self.n_elements)  
     error_flag= ~self.unix(tmp)
     RETURN
  ENDIF

  IF KEYWORD_SET(ISO8601_STRING) THEN BEGIN
     IF NOT TOTAL(siz.type EQ 7) THEN MESSAGE,'Error- input ISO8601_STRING must be type string, was: '+siz.type_name
     tmp= REFORM(value,self.n_elements)
     tmp= STRCOMPRESS(tmp,/REMOVE_ALL) 
     error_flag= ~self.iso8601(tmp)
     RETURN
  ENDIF

  ;# default to Y2K_SECONDS
  IF NOT KEYWORD_SET(Y2K_SECONDS) THEN MESSAGE,'Warning: no keyword set, defaulting to internal format (Y2K)',/INFORM  
  tmp= REFORM(value,self.n_elements)  
  error_flag= ~self.y2k(tmp)
  
  RETURN
END ;#----------------------------------------------------------------------------


FUNCTION SKYMAP_TIME::GET,index,POINTER=pointer,NO_REFORM=no_reform  $
          ,CDF_MILLISECONDS=cdf_milliseconds,Y2K_SECONDS=y2k_seconds,JULIAN_DAY=julian_day  $
          ,ISO8601_STRING=iso8601_string,YMDHMS_ARRAY=ymdhms_array,UNIX_SECONDS=unix_seconds
                     
  IF (self.time EQ !NULL) THEN BEGIN
    MESSAGE,'Warning- time is empty or undefined',/INFORMATIONAL
    RETURN,!NULL
  ENDIF
    
  IF KEYWORD_SET(POINTER) THEN RETURN,self.time  ;# pointer to the N vector
  
  IF KEYWORD_SET(CDF_MILLISECONDS) THEN BEGIN
     result= self.cdf()
     IF NOT KEYWORD_SET(NO_REFORM) THEN result= REFORM(result,self.dimensions(/NO_ZERO))
     RETURN,result
  ENDIF

  IF KEYWORD_SET(JULIAN_DAY) THEN BEGIN
     result= self.julian(MODIFIED=(julian_day GT 1))
     IF NOT KEYWORD_SET(NO_REFORM) THEN result= REFORM(result,self.dimensions(/NO_ZERO))
     RETURN,result
  ENDIF

  IF KEYWORD_SET(UNIX_SECONDS) THEN BEGIN
     result= self.unix()
     IF NOT KEYWORD_SET(NO_REFORM) THEN result= REFORM(result,self.dimensions(/NO_ZERO))
     RETURN,result
  ENDIF

  IF KEYWORD_SET(YMDHMS_ARRAY) THEN BEGIN
     result= self.ymdhms()
     IF NOT KEYWORD_SET(NO_REFORM) THEN result= REFORM(result,[6,self.dimensions(/NO_ZERO)],/OVERWRITE)
     RETURN,result
  ENDIF

  IF KEYWORD_SET(ISO8601_STRING) THEN BEGIN
     result= self.iso8601(EXTENDED_FORMAT=(iso8601_string GT 1))
     IF NOT KEYWORD_SET(NO_REFORM) THEN result= REFORM(result,self.dimensions(/NO_ZERO),/OVERWRITE)
     RETURN,result
  ENDIF
  
  IF NOT KEYWORD_SET(Y2K_SECONDS) THEN BEGIN
    MESSAGE,'Warning: no keyword set, defaulting to internal format (Y2K)',/INFORM
    default=1
  ENDIF ELSE default=0
    
  IF KEYWORD_SET(Y2K_SECONDS) OR (default) THEN BEGIN    
    result= self.y2k()
    IF NOT KEYWORD_SET(NO_REFORM) THEN result= REFORM(result,self.dimensions(/NO_ZERO),/OVERWRITE)  
    RETURN,result
  ENDIF
  
  MESSAGE,'Error- this should never happen'
END ;#----------------------------------------------------------------------------
 
 
FUNCTION SKYMAP_TIME::COPY  ;#,scalar_value,OVERWRITE=overwrite 
  RETURN,SKYMAP_TIME(self.get(/Y2K),/Y2K)
END ;#----------------------------------------------------------------------------


;# Goal is to test each method under a range of inputs.
;# !!RUN THIS AFTER ANY CODE CHANGE!!  
FUNCTION SKYMAP_TIME::SELFTEST
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  flag= ['[PASS]','[FAIL]']+' '  &  ntest=0  &  nfail=0

  note= 'Create object from empty (valid)'
  x= SKYMAP_TIME()  &  fail= ~OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Unix time input (valid)'  &  val= 123456789.0
  x= SKYMAP_TIME(val,/UNIX_SECONDS)  &  fail= ~OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1 
  
  note= 'Unix time reversal'  &  val= 123456789.0
  x= SKYMAP_TIME(val,/UNIX_SECONDS)  &  fail= x.get(/UNIX) NE val
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Julian time reversal'  &  val= 123456789.0
  x= SKYMAP_TIME(val,/JULIAN_DAY)  &  fail= x.get(/JULIAN) NE val
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
  
  note= 'YMDHMS time reversal'  &  val= [2000, 01, 02, 03, 04, 05]
  x= SKYMAP_TIME(val,/YMDHMS_ARRAY)  &  fail= TOTAL(x.get(/YMDHMS) NE val)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1 

  note= 'ISO8601 time reversal'  &  val= '2000-01-02T03:04:05'
  x= SKYMAP_TIME(val,/ISO8601_STRING)  &  fail= x.get(ISO8601=2) NE val
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'ISO8601 to Unix epoch'  &  val= '1970-01-01T00:00:00'
  x= SKYMAP_TIME(val,/ISO8601_STRING)  &  fail= x.get(/UNIX) NE 0.0
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

;http://aa.usno.navy.mil/cgi-bin/aa_jdconv.pl
  note= 'ISO8601 to Julian'  &  val= '2012-02-14T00:00:00'
  x= SKYMAP_TIME(val,/ISO8601_STRING)  &  fail= x.get(/JULIAN) NE 2455971.500000
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Julian to ISO8601'  &  val= 2455971.500000
  x= SKYMAP_TIME(val,/JULIAN_DAY)  &  fail= x.get(/ISO) NE '20120214T000000' 
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

;IDL> print,systime(0,987654321,/utc)
;Thu Apr 19 04:25:21 2001
  note= 'Unix to ISO8601'  &  val= 987654321.0d0
  x= SKYMAP_TIME(val,/UNIX_SECONDS)  &  fail= x.get(ISO=2) NE '2001-04-19T04:25:21'
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Unix (single precision) to ISO8601'  &  val= 987654321.0
  x= SKYMAP_TIME(val,/UNIX_SECONDS)  &  fail= x.get(ISO=2) NE '2001-04-19T04:25:36'
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

;http://disc.gsfc.nasa.gov/julian_calendar.shtml
  note= 'ISO8601 to day of year'  &  val= '20011201T000001'
  x= SKYMAP_TIME(val,/ISO8601)  &  fail= x.day_of_year() NE 335
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Leap years (valid)'  &  val= [1600, 2000, 2400, 1996, 2004, 2008] 
  fail= TOTAL(x.isa_leapyear(val)) NE 6
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Leap years (invalid)'  &  val= [1800, 1900, 2100, 1998, 1999, 2001] 
  fail= TOTAL(x.isa_leapyear(val)) NE 0
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'JULDAY comparison'  &  val= [2001, 02, 03, 04, 05, 06]
  x= SKYMAP_TIME(val,/YMDHMS)  &  y= JULDAY(02,03,2001,04,05,06)
  fail= ABS(x.get(/JULIAN_DAY) - y) GE 1d-9
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1
  
  RETURN,FIX([nfail,ntest])  ;# ideally, nfail=0
END ;#----------------------------------------------------------------------------


PRO SKYMAP_TIME__DEFINE
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  void= {SKYMAP_TIME                $
        ,n_elements:0UL                 $
        ,dimensions:ULONARR(8)         $
        ,time:PTR_NEW()              $ ;# DBLARR(n_elements)
        ,inherits Skymap_Object           $ ;# for operator overloading
        }
RETURN
END ;#----------------------------------------------------------------------------
 
 
t= skymap_time()
print,t.selftest()


;IDL> print,systime(0,0)
;Wed Dec 31 17:00:00 1969
;IDL> print,systime(0,0,/utc)
;Thu Jan 01 00:00:00 1970
;IDL> print,systime(0,123456789,/utc)
;Thu Nov 29 21:33:09 1973
;IDL> print,systime(0,987654321,/utc)
;Thu Apr 19 04:25:21 2001
 
 
;;# CDF_EPOCH is 40x faster than JULDAY 
;st= SYSTIME(1)
;FOR i=0L,999999L DO CDF_EPOCH,tmp,2000,1,1,0,0,0,/COMPUTE
;PRINT,SYSTIME(1)-st  ;# 0.54499984
;
;st= SYSTIME(1)
;FOR i=0L,999999L DO tmp=JULDAY(2000,1,1,0,0,0)
;PRINT,SYSTIME(1)-st  ;# 21.880000
  
END