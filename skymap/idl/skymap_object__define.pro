;#  Subversion $Id: skymap_object__define.pro 190 2010-12-12 02:20:57Z bjackel $

;+
; Superclass that inherits IDL_OBJECT for operator overloading; also provides some utility methods.  
; Can also be used to factor out shared code from SKYMAP_TIME and SKYMAP_VECTOR superclasses.
;
; Examples:
;  
;   x= skymap_object()           ;# create an empty object
;    
;   y= skymap_object(x)
;   IF NOT OBJ_VALID(y) THEN MESSAGE,'Error- input was not a valid SKYMAP_OBJECT'
;    
;   help,/struct,x.get_internal()   ;# sneak peak at internal state
;
;   y= x.copy()                  ;# cloning (of simple objects only)
;-

; To do:
;   - think about adding "assert" method to simplify self-tests

;# IDL defines this implicitly, but we want to also "pass through": if input is valid object, just return it. 
FUNCTION SKYMAP_OBJECT,value,NO_COPY=no_copy,_EXTRA=_extra
  classname= 'SKYMAP_OBJECT'  &  siz= SIZE(value,/STRUCT)
  IF (siz.type_name EQ 'OBJREF') && (OBJ_CLASS(value) EQ classname) THEN $
    IF KEYWORD_SET(NO_COPY) THEN RETURN,value ELSE RETURN,value.copy()
  RETURN,OBJ_NEW(classname,value,_EXTRA=_extra)
END ;#----------------------------------------------------------------------------


;# called only by OBJ_NEW
FUNCTION SKYMAP_OBJECT::INIT,value,_EXTRA=_extra
  COMPILE_OPT HIDDEN              ;# don't clutter user visible namespace
;  self.set,value,SUCCESS=success,_EXTRA=_extra    ;# IF hash then try to set value
;  self.version=1
  RETURN,1
END ;#----------------------------------------------------------------------------


;# called only by OBJ_DESTROY
PRO SKYMAP_OBJECT::CLEANUP
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
RETURN
END ;#----------------------------------------------------------------------------


;# was overloadHelp, but that complicates debugging
FUNCTION SKYMAP_OBJECT::_overloadPrint,_EXTRA=_extra
  COMPILE_OPT IDL2,HIDDEN        ;# don't clutter user visible namespace
  result= 'OBJREF  =  '+OBJ_CLASS(self)
  FOREACH value,self.get_internal(/HASH),name DO $
    IF (name NE '') THEN result= [result, STRING(10b)+name+':'+STRING(value)] 
  RETURN,result
END ;#----------------------------------------------------------------------------


;# Violate object principles by allowing access to internal state... 
;#
FUNCTION SKYMAP_OBJECT::GET_INTERNAL,HASHTYPE=hashtype,LISTTYPE=listtype
  COMPILE_OPT IDL2,HIDDEN        ;# don't clutter user visible namespace

  nt= N_TAGS(self)  &  tname= TAG_NAMES(self)  &  sname= TAG_NAMES(self,/STRUCTURE_NAME) 
    
  IF KEYWORD_SET(HASHTYPE) THEN BEGIN  ;# HASH(self) doesn't work  
    result= HASH('',sname) 
    FOR indx=0,nt-1 DO result[tname[indx]]= self.(indx)  ;# skip idl_* ?  
  ENDIF ELSE IF KEYWORD_SET(LISTTYPE) THEN BEGIN
    result= LIST(sname) 
    FOR indx=0,nt-1 DO result += LIST(tname[indx], self.(indx))  ;# skip idl_* ?
  ENDIF ELSE BEGIN  ;result= CREATE_STRUCT(self)  ;# doesn't work
    result= CREATE_STRUCT(NAME=sname)
    STRUCT_ASSIGN,self,result
  ENDELSE
  
  RETURN,result
END ;#----------------------------------------------------------------------------


;# Violate object principles by allowing access to internal state... 
;#
PRO SKYMAP_OBJECT::SET_INTERNAL,value,ERROR_FLAG=error_flag,_EXTRA=_extra
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  siz= SIZE(value,/STRUCTURE)  &  error_flag=1
  IF (siz.type EQ 0) THEN BEGIN & error_flag=0 & RETURN & ENDIF
  IF (siz.type_name EQ 'STRUCT') THEN BEGIN & STRUCT_ASSIGN,value,self,/NOZERO & error_flag=0 & RETURN & ENDIF
  IF (siz.type_name NE 'OBJREF') THEN MESSAGE,'Error- input must be type OBJREF, was: '+siz.type_name
  IF (OBJ_CLASS(value) EQ 'HASH') THEN BEGIN
    IF (OBJ_CLASS(self) NE value['']) THEN MESSAGE,'Error- input class must be'+OBJ_CLASS(self)+', was: '+value['']  
    STRUCT_ASSIGN,value->toStruct(_EXTRA=_extra),self,/NOZERO  &  error_flag=0
  ENDIF ELSE IF (OBJ_CLASS(value) EQ 'LIST') THEN BEGIN
    stop  ;# !!FIXME!!
  ENDIF ELSE MESSAGE,'Error- input must be class HASH or LIST, was: '+OBJ_CLASS(value)

  RETURN
END ;#----------------------------------------------------------------------------


; A reasonably general copy method that dereferences pointers and attempts to copy sub-objects.
; 
FUNCTION SKYMAP_OBJECT::COPY,object,SKIP=skip
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace

  IF NOT KEYWORD_SET(SKIP) THEN skip=['','IDL_OBJECT_TOP','__OBJ__','IDL_OBJECT_BOTTOM']

  IF (object EQ !NULL) THEN object=self
  IF NOT OBJ_HASMETHOD(object,'get_internal') THEN BEGIN
    MESSAGE,'Error- object does not support "get_internal" method'
    RETURN,!NULL
  ENDIF
  
  state= object.get_internal(/HASH)
  FOREACH value,state,name DO BEGIN
    siz= SIZE(value,/STRUCTURE)
    CASE (siz.type_name) OF
    'POINTER': state[name]= PTR_NEW(*value)  ;# dereference pointers
    'OBJREF':IF OBJ_HASMETHOD(object,'copy') THEN state[name]= value.copy() ELSE $
             MESSAGE,'Warning, could not create new copy of object:'+name,/INFORM
    ELSE: void=0
    ENDCASE
  ENDFOREACH
  
  result= OBJ_NEW(OBJ_CLASS(object))   ;# requires that init accept zero arguments
  result.set_internal,state,SKIPPED=skip,ERROR_FLAG=error_flag

  RETURN,result
END ;#----------------------------------------------------------------------------


;# Use this to control debugging and error reporting
;#
FUNCTION SKYMAP_OBJECT::VERBOSE,level
  IF (level NE !NULL) THEN self.verbose=level
  RETURN,self.verbose
END ;#----------------------------------------------------------------------------


;# for use in "catch" blocks
FUNCTION SKYMAP_OBJECT::CATCH,flag,NO_CANCEL=no_cancel
  IF (flag EQ 0) THEN RETURN,0  ;# no problem
  IF NOT KEYWORD_SET(NO_CANCEL) THEN CATCH,/CANCEL
  PRINT,!ERROR_STATE.MSG  
  stack= SCOPE_TRACEBACK(/STRUCTURE)
  ;PRINT,stack[-1].routine+'  '+ STRJOIN((STRSPLIT(!ERROR_STATE.MSG,/EXTRACT))[1:*],' ')
  IF (self.verbose GT 0) THEN FOR indx=N_ELEMENTS(stack)-1,0,-1 DO $
    PRINT,'-'+stack[indx].routine+STRCOMPRESS(' (line '+STRING(stack[indx].line,FORMAT='(I,")")'))
  RETURN,1
END ;#----------------------------------------------------------------------------



;# if IDL source files are in /path/foo/bar/skymap/idl, then the skymap root directory is /path/foo/bar/skymap
FUNCTION SKYMAP_OBJECT::PATH,subdir,CWD=cwd
  stack= SCOPE_TRACEBACK(/STRUCTURE)
  caller= stack[-2]  &  fname= caller.filename
  path= FILE_DIRNAME(FILE_DIRNAME(fname,/MARK_DIRECTORY))
  IF (N_PARAMS() GT 0) THEN FOREACH part,subdir DO path= path + PATH_SEP() + part 
  RETURN,path
END ;#----------------------------------------------------------------------------


;# Goal is to test each method under a range of inputs.
;# !!RUN THIS AFTER ANY CODE CHANGE!!
FUNCTION SKYMAP_OBJECT::SELFTEST
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  flag= ['[PASS]','[FAIL]']+' '  &  ntest=0  &  nfail=0

  note= 'Create vector from empty (valid)'
  x= SKYMAP_OBJECT()  &  fail= ~OBJ_VALID(x)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Get internal state as HASH'
  x= self->get_internal(/HASH)  &  fail= x[''] NE TAG_NAMES(self,/STRUCTURE_NAME)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Set internal state as HASH'  
  self.set_internal,x,SKIPPED=['','IDL_OBJECT_TOP','__OBJ__','IDL_OBJECT_BOTTOM'],ERROR=err  &  fail= err
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Get internal state as STRUCTURE'
  x= self->get_internal()  &  fail= TAG_NAMES(x,/STRUCTURE_NAME) NE TAG_NAMES(self,/STRUCTURE_NAME)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= 'Set internal state as STRUCTURE'  
  self.set_internal,{verbose:3},ERROR=err  &  fail= err OR (self.verbose NE 3)
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  self.verbose=0
  note= '->catch(0) method'
  x= self->catch(0)  &  fail= x NE 0
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= '->catch(1) method'
  x= self->catch(1)  &  fail= x EQ 0
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  note= '->path method'
  path= self.path('idl')  &  fail= FILE_SEARCH(path,'skymap_object__define.pro') EQ ''
  PRINT,flag[fail] + note  &  nfail += fail  &  ntest += 1

  RETURN,FIX([nfail,ntest])  ;# ideally, nfail=0
END ;#----------------------------------------------------------------------------


;# Define the core structure (keep as light-weight as possible) 
;# +hack to make sure !path system variable is set
;#
PRO SKYMAP_OBJECT__DEFINE
  COMPILE_OPT HIDDEN        ;# don't clutter user visible namespace
  void= {SKYMAP_OBJECT                $
;        ,version:0                   $
        ,verbose:0                    $
        ,inherits IDL_Object          $ ;# for operator overloading
        }

  ;# source file subdir should be in !PATH, if not add and complain
  stack= SCOPE_TRACEBACK(/STRUCTURE)  &  caller= stack[-2]
  path= FILE_DIRNAME(caller.filename,MARK_DIRECTORY=0)  &  pos= STRPOS(!PATH,path)
  IF (pos EQ -1) THEN BEGIN
    MESSAGE,'Warning- skymap subdir not in !PATH, appending: '+path,/INFORM
    !PATH= !PATH + PATH_SEP(/SEARCH_PATH) + path
    PATH_CACHE,CLEAR=1,REBUILD=0  ;# lazy rebuild
  ENDIF

RETURN
END ;#----------------------------------------------------------------------------
 

;To Do -add more self tests
a= SKYMAP_OBJECT()
void= a.verbose(0)  ;&  print,void
PRINT,a.selftest()
END