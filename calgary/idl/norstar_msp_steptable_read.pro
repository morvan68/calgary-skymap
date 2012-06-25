;# Subversion $Id$

;+
; This IDL program reads mirror step tables for the NORSTAR meridian scanning photometer (MSP)
; 
; Example:
;  step= NORSTAR_MSP_STEPTABLE_READ(17)
;
;-


FUNCTION NORSTAR_MSP_STEPTABLE_READ,version_number,DATA_DIRECTORY=datadir,FILENAME=filename

  IF NOT KEYWORD_SET(datadir) THEN BEGIN  ;#  deafault: calgary/idl/../dat
    thisdir= FILE_DIRNAME(((SCOPE_TRACEBACK(/STRUCTURE))[-1]).filename,/MARK_DIRECTORY)
    datadir= STRJOIN([thisdir,PATH_SEP(/PARENT_DIRECTORY),'dat'],PATH_SEP())
  ENDIF
  IF NOT FILE_TEST(datadir,/DIRECTORY) THEN MESSAGE,'Error- directory not found: '+datadir  
    
  filelist= FILE_SEARCH(datadir,'*.stp',COUNT=nfiles)
  IF (nfiles EQ 0) THEN MESSAGE,'Error- no .stp files found in directory: '+datadir

  vnum= STRMID(filelist,8,5,/REVERSE_OFFSET)
  w= WHERE(vnum EQ version_number,nw)
  IF (nw EQ 0) THEN MESSAGE,'Error- version number not found: '+version_number
  filename= filelist[w]

  step= LONARR(9999) -1L

  ON_IOERROR,done
  OPENR,lun,filename,/GET_LUN
  READF,lun,step
  done: fs= FSTAT(lun) 
  FREE_LUN,lun
  
 ; w= WHERE(step NE -1,nw)  
 ; IF (nw NE fs.transfer_count) THEN MESSAGE,'Warning: step count mismatch: '+nw,/INFORMATIONAL
 
  RETURN,step[0:fs.transfer_count-1]
END


; Make useful step-table plots 
;
PRO NORSTAR_MSP_STEPTABLE_PLOT_FIGURES,version_number,HARDCOPY=hardcopy

  ; create a multi-page postscript file in ../fig subdir
  IF KEYWORD_SET(HARDCOPY) THEN BEGIN
    _d= !d  &  SET_PLOT,'PS'  & !p.font=0
    thisdir= FILE_DIRNAME(((SCOPE_TRACEBACK(/STRUCTURE))[-1]).filename,/MARK_DIRECTORY)
    filename= STRJOIN([thisdir,PATH_SEP(/PARENT_DIRECTORY),'fig','norstar_msp_steptable_figures.ps'],PATH_SEP())
    DEVICE,FILENAME=filename,PREVIEW=0,/LANDSCAPE
  ENDIF
 
  step= NORSTAR_MSP_STEPTABLE_READ(version_number,FILENAME=fname)
 
  xr= [0,N_ELEMENTS(step)]  &  yr= [0,2000]
  PLOT,step,PSYM=-4,SYMSIZE=0.4,TITLE=FILE_BASENAME(fname)      $
    ,XRANGE=xr,XSTYLE=1,XTICKLEN=1,XTITLE='step table index',XMARGIN=[8,6]   $
    ,YRANGE=yr,YSTYLE=1,YTICKLEN=1,YTITLE='mirror step number'
  
  ytickv= [0,500,1000,1500,2000]
  AXIS,xr[1],yr[0],YAXIS=1,YTITLE='mirror step angle [degrees]' $
    ,YTICKV=ytickv,YTICKNAME=STRING(ytickv*(360.0/4000),FORMAT='(-I3)')
  
  ;# look at the distance for targets at 110km
  re= 6371.2 ; earth radius in kilometres  
  alpha= !dpi/2 + step*(360.0/4000)*!dtor 
  gamma= asin( re/(re+110.0) * sin(alpha) ) 
  beta= !pi - alpha - gamma 
  l= beta*re  
  
  yr= [-600,+600]
  PLOT,l,PSYM=-4,SYMSIZE=0.4,TITLE=FILE_BASENAME(fname)      $
    ,XRANGE=xr,XSTYLE=1,XTICKLEN=1,XTITLE='step table index',XMARGIN=[8,3]   $
    ,YRANGE=yr,YSTYLE=1,YTICKLEN=1,YTITLE='distance to location below target at 110km'
  
  
  IF KEYWORD_SET(HARDCOPY) THEN BEGIN
    DEVICE,/CLOSE_FILE  &  SET_PLOT,_d.name  &  !p.font=-1
  ENDIF
RETURN
END


;NORSTAR_MSP_STEPTABLE_PLOT_FIGURES,16  ; horizon 30-second mode
;NORSTAR_MSP_STEPTABLE_PLOT_FIGURES,17  ; standard 30-second mode
;NORSTAR_MSP_STEPTABLE_PLOT_FIGURES,18  ; fast 15-second mode
;NORSTAR_MSP_STEPTABLE_PLOT_FIGURES,19  ; zenith 30-second mode

NORSTAR_MSP_STEPTABLE_PLOT_FIGURES,17,/HARDCOPY

END