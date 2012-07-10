;+
; This IDL object translates [height,latitude,longitude] to and from
; an internal Cartesian representation using the SKYMAP_VECTOR object. 
;
; It also provides local direction vectors (ie. north, east, down)
; 
; The Earth is not a perfect sphere, but can be approximated by an oblate ellipsoid.
; Geographic coordinates use a reference geoid (ie. WGS-84).
;-


;# IDL defines this implicitly, but we want to also "pass through" ie. if input is valid object, just return it 
FUNCTION SKYMAP_STAR_CATALOG,value,_EXTRA=_extra
  classname= 'SKYMAP_STAR_CATALOG'  &  siz= SIZE(value,/STRUCT)
  IF (siz.type EQ 11) && (OBJ_CLASS(value) EQ classname) THEN RETURN,value
;  IF (N_PARAMS() EQ 0) THEN result= OBJ_NEW(classname) ELSE result= OBJ_NEW(classname,value,_EXTRA=_extra) 
  result= OBJ_NEW(classname,value,_EXTRA=_extra)
RETURN,result
END

;# called only by OBJ_CREATE
FUNCTION SKYMAP_STAR_CATALOG::INIT,value,_EXTRA=_extra
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  self->read_binary,value,ERROR_FLAG=error_flag
  ;IF (N_PARAMS() GT 0) THEN self->set,value,SUCCESS=success,_EXTRA=_extra
;  stop
  RETURN,~error_flag
END ;#----------------------------------------------------------------------------


;# called only by OBJ_DESTROY
PRO SKYMAP_STAR_CATALOG::CLEANUP
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
 ; IF (HEAP_REFCOUNT(self.ras) EQ 1) THEN PTR_FREE,self.ras
  self.skymap_vector::cleanup
RETURN
END ;#----------------------------------------------------------------------------

FUNCTION SKYMAP_STAR_CATALOG::_overloadPrint 
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  IF (self.n_elements GT 1) THEN BEGIN
    d= self.dimensions  &  d= d[WHERE(d NE 0)]
    tmp= STRING(d,FORMAT='("[",8(I,:,","))')+']'
    tmp= STRCOMPRESS(tmp,/REMOVE_ALL)
  ENDIF ELSE tmp= '   '+ STRING(self.get(),FORMAT='(3(G,X))')
  RETURN,'OBJREF  =  '+OBJ_CLASS(self)+tmp
END ;#----------------------------------------------------------------------------


PRO SKYMAP_STAR_CATALOG::READ_BINARY,filename,ERROR_FLAG=error_flag
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  error_flag=1
  ON_IOERROR,fail

  rinfo= ROUTINE_INFO(OBJ_CLASS(self),/FUNCTION,/SOURCE)
  filename= FILE_DIRNAME(FILE_DIRNAME(rinfo.path),/MARK_DIRECTORY) + 'data' + PATH_SEP() + 'BSC5'
  fs= FILE_SEARCH(filename,COUNT=nf)
  IF (nf EQ 0) THEN MESSAGE,'Error- file not found: '+filename
  OPENR,lun,filename,/GET_LUN

  header= {SKYMAP_YBSC5_BINARY_HEADER}
  READU,lun,header  ;# validate !!
  
  fs= FSTAT(lun)  &  nrec= (fs.size-fs.cur_ptr) / header.nbent
  IF (nrec NE ABS(header.starN)) THEN MESSAGE,'Boom'     
  records= REPLICATE( {SKYMAP_YBSC5_BINARY_RECORD} , nrec) 
  READU,lun,records
  
  vec= DBLARR(nrec,3)
  vec[*,0]= 1.0  &  vec[*,1]= records.dec  &  vec[*,2]= records.ras
  self.skymap_vector::set,vec,SPHERICAL=4
  
  self.binary_data= PTR_NEW(records,/NO_COPY)
  error_flag=0
  
  fail: RETURN
END ;#----------------------------------------------------------------------------

FUNCTION SKYMAP_STAR_CATALOG::DMS,angle,DEGREES=degrees,STRING=string
  IF KEYWORD_SET(DEGREES) then deg= angle ELSE deg= angle/!dtor
  
  d= FIX(deg)    &  deg= deg - d
  m= FIX(deg*60) &  deg= deg - m
  s= deg*60.0
stop
  IF KEYWORD_SET(STRING) THEN BEGIN
    result= STRING(d,m,s,FORMAT='(I+3.2,":",I2.2,":",F05.2)')
  ENDIF ELSE result= [[d],[m],[s]]

RETURN,result
END







FUNCTION SKYMAP_STAR_CATALOG::SELFTEST
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
  x.set,g0,SUCCESS=success  &  fail= ~success
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Get = dblarr[3]  (valid)'
  tmp= x.get()  &  fail= TOTAL((g0-tmp)^2) GE 1d-6
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  g0= [1234.5, 12.3, 123.4]  &  x.set,g0,SUCCESS=success
  note= 'Compare get after set'
  tmp= x.get()  &  fail= TOTAL((g0-tmp)^2) GE 1d-6
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

stop

  RETURN,FIX([nfail,ntest])  ;# ideally, nfail=0
END ;#----------------------------------------------------------------------------

PRO SKYMAP_STAR_CATALOG__DEFINE
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace

  RESOLVE_ROUTINE,'SKYMAP_VECTOR__DEFINE',COMPILE_FULL_FILE=0,/NO_RECOMPILE

  void= {SKYMAP_YBSC5_BINARY_HEADER  $
         , star0:0L, star1:0L, starN:0L, stnum:0L, mprop:0L, nmag:-1L, nbent:32L}
 
  void= {SKYMAP_YBSC5_BINARY_RECORD  $
         , number:0.0, ras:0.0d0, dec:0.0d0, type:[0b,0b], magnitude:0, dras:0.0, ddec:0.0} 
  
  void= {SKYMAP_STAR_CATALOG          $
        ,binary_data:PTR_NEW()        $
        ,inherits SKYMAP_VECTOR       $                
        }
RETURN
END ;#----------------------------------------------------------------------------

 

test= SKYMAP_STAR_CATALOG()
star= *(test.get_internal()).binary_data
stop
print,star[423] ;# polaris
w= where((star.dec ge -30*!dtor) AND (star.magnitude le 210)) & help,w & radec=[[star[w].ras],[star[w].dec]]

print,a.selftest()

stop
b=a.target_range(0.0,0.0,500.0)

c= a.aim_target(b)
print,c

END

;http://heasarc.gsfc.nasa.gov/W3Browse/star-catalog/bsc5p.html

;http://tdc-www.harvard.edu/catalogs/bsc5.html
;
;Yale Bright Star Catalog 5 (BSC5) Catalog Binary Header Format
;
;The first 28 bytes of both BSC5 and BSC5ra contain the following information:
;Integer*4 STAR0=0 Subtract from star number to get sequence number
;Integer*4 STAR1=1 First star number in file
;Integer*4 STARN=9110    Number of stars in file
;Integer*4 STNUM=1 0 if no star i.d. numbers are present
;      1 if star i.d. numbers are in catalog file
;      2 if star i.d. numbers are  in file
;Logical*4 MPROP=1 1 if proper motion is included
;      0 if no proper motion is included
;Integer*4 NMAG=-1 Number of magnitudes present (-1=J2000 instead of B1950)
;Integer*4 NBENT=32  Number of bytes per star entry
;
;Yale Bright Star Catalog 5 (BSC5) Binary Entry Format
;
;Each catalog entry in BSC5 and BSC5ra contains 32 bytes with the following information:
;Real*4 XNO    Catalog number of star
;Real*8 SRA0   B1950 Right Ascension (radians)
;Real*8 SDEC0    B1950 Declination (radians)
;Character*2 IS    Spectral type (2 characters)
;Integer*2 MAG   V Magnitude * 100
;Real*4 XRPM   R.A. proper motion (radians per year)
;Real*4 XDPM   Dec. proper motion (radians per year)
;Sample Entries
;
;The first and last entries in the Yale Bright Star (BSC5) catalog file are:
;
;sbsc5 WCSTools 3.8.1, 30 April 2010, Doug Mink SAO
;BSC_number  RA2000       Dec2000       Mag Type
;  000001  00:05:09.900 +45:13:45.00   6.70  A1
;  009110  00:05:06.200 +61:18:51.00   5.80  B8
;The first and last entries in the RA-sorted BSC5ra catalog file are:
;
;BSC_num   RA2000       Dec2000     Mag Type
;009077 00:00:19.200 -44:17:26.00  6.29  G3
;009076 23:59:55.000 -65:34:38.00  4.50  B9
;
;
;

