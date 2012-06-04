;# Subversion $Id$
;# 10 Aug 2010: Brian Jackel wrote based on info provided by Greg Baker
;# 28 May 2012: Bjj updated with iv4 format, made more robust

;+
; This IDL program reads data files produced by the NORSTAR 
; meridian scanning photometer (MSP) UDP packet stream.
; 
; Example:
;  datadir= 'c:/data/norstar/msp/2012/05/01/'  &  pattern='*_GILL_iv[234].dat' 
;  filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)
;  data= NORSTAR_MSP_DAT_READFILE(filelist[0])
;  plot,data[9999:9999+1999].counts[0]
;
;  data= NORSTAR_MSP_DAT_READFILE(filelist)
;  dat= NORSTAR_MSP_DAT_RESHAPE(data,UNIX_SECONDS=time)
;  dat= TRANSPOSE(SQRT(dat.counts[1]))
;  SHADE_SURF,dat,SHADE=BYTSCL(dat),ax=90,az=0
;
;-


;index_6300=0 ; red line
;index_4800=1 ; blue background
;index_4861A=2 ; H-beta
;index_4861B=3 ; H-beta
;index_4950=4 ; blue background
;index_4709=5 ; Nitrogen blue line
;index_5577=6 ;  green line
;index_6250=7 ; red line background

; The instrument produces one data record (packet) for every full rotation of the filter wheel.
; There have been several different revisions of the record structure; so far all are 64-bytes long (with variable amounts of padding).
; Version 1 was only used for a short time during the early refurbishment process.
; Versions 2-4 all start with the same 2-bytes ("UC"=0x5543=21827) and end with "CU"=0x4355=17237
PRO NORSTAR_MSP_DAT_IV2__DEFINE
  tmp= {NORSTAR_MSP_DAT_IV2     $ 
       ,header:BYTARR(2)        $  ;# "UC" = 0x5534
       ,sitecode:BYTARR(4)      $  ;# 4 characters eg. "GILL"
       ,version:0U              $  ;# 0x0002
       ,nsteps:0U               $  ;# mirror steps per scan (544 for 30s scans)
       ,device_id:0U            $  ;# 0x0001 for prototype?
       ,packet_size:0U          $  ;# 0x0064
       ,mode:0U                 $  ;# 0x0101 nominal
       ,step_table_version:0U   $
       ,seconds:0L              $
       ,microseconds:0L         $
       ,mirror_step:0U          $  ;# step table index (0-?)
       ,start_of_data:0U        $  ;# 0xff00 
       ,counts:UINTARR(8)       $  ;# 8 filters x 16 bits
       ,end_of_data:0U          $  ;# 0xff00       
       ,padding:BYTARR(14)      $  ;# 
       ,trailer:BYTARR(2)       $  ;# "CU" = 0x3455
  }
RETURN
END


;# Version 3: added "north_step"
PRO NORSTAR_MSP_DAT_IV3__DEFINE
  tmp= {NORSTAR_MSP_DAT_IV3     $ 
       ,header:BYTARR(2)        $  ;# "UC" = 0x5534
       ,sitecode:BYTARR(4)      $  ;# 4 characters eg. "GILL"
       ,version:0U              $  ;# packet version (0x0003)
       ,nsteps:0U               $  ;# mirror steps per scan (544 for 30s scans)
       ,north_step:0U           $  ;# 0x0000
       ,device_id:0U            $  ;# 0x0001 for prototype
       ,packet_size:0U          $  ;# 0x0064
       ,mode:0U                 $  ;# 0x0101 nominal
       ,steptable_version:0U    $  ;# 0x0010
       ,seconds:0L              $
       ,microseconds:0L         $
       ,mirror_step:0U          $  ;# step table index (0-?)
       ,start_of_data:0U        $  ;# 0x00ff 
       ,counts:UINTARR(8)       $  ;# 8 filters x 16 bits
       ,end_of_data:0U          $  ;# 0xff00       
       ,padding:BYTARR(12)      $  ;# 0xff00
       ,trailer:BYTARR(2)       $  ;# "CU" = 0x3455
  }
RETURN
END

;# Version 4: added "n_calibration", two status register bytes
PRO NORSTAR_MSP_DAT_IV4__DEFINE
  tmp= {NORSTAR_MSP_DAT_IV4     $ 
       ,header:BYTARR(2)        $  ;# "UC" = 0x5534
       ,sitecode:BYTARR(4)      $  ;# 4 characters eg. "GILL"
       ,version:0U              $  ;# packet version (0x0003)
       ,nsteps:0U               $  ;# mirror steps per scan (544 for 30s scans)
       ,n_calibration:0U        $  ;# approximate(?) # of calibration records (15-25)
       ,north_step:0U           $  ;# 0x0000
       ,status_register_a:0b    $  ;# system config bits
       ,status_register_b:0b    $  ;# system config bits
       ,device_id:0U            $  ;# 0x0001 for prototype
       ,packet_size:0U          $  ;# 0x0064
       ,mode:0U                 $  ;# 0x0101 nominal
       ,step_table_version:0U   $  ;# 0x0010
       ,seconds:0L              $
       ,microseconds:0L         $
       ,mirror_step:0U          $  ;# step table index (0-?)
       ,start_of_data:0U        $  ;# 0x00ff 
       ,counts:UINTARR(8)       $  ;# 8 filters x 16 bits
       ,end_of_data:0U          $  ;# 0xff00       
       ,padding:BYTARR(8)       $  ;# 0xff00
       ,trailer:BYTARR(2)       $  ;# "CU" = 0x3455
  }
RETURN
END


FUNCTION NORSTAR_MSP_DAT_VALID,record
  
  
  ;# need to check if structure(s)
  
  sname= TAG_NAMES(record[0],/STRUCTURE_NAME)
  CASE sname OF
  'NORSTAR_MSP_DAT_IV2':version=2
  'NORSTAR_MSP_DAT_IV3':version=3
  'NORSTAR_MSP_DAT_IV4':version=4
  ELSE: BEGIN
    MESSAGE,'Error- unrecognized structure:'+sname
    RETURN,!NULL
    END
  ENDCASE
  
  good_headtail= (STRING(record.header) EQ 'UC') && (STRING(record.trailer) EQ 'CU')
  good_version= record.version EQ version
  good_length= (record.packet_size EQ 64) && (N_TAGS(record,/DATA_LENGTH) EQ 64)
  
  wgood= WHERE(good_headtail && good_version && good_length, ngood)
RETURN,wgood
END


; Ideally we could get a 2-D array with:  data= REFORM(data,564,120)
; but partial hours and dropped records make things more complicated.
;
FUNCTION NORSTAR_MSP_DAT_RESHAPE,record,UNIX_SECONDS=unix_seconds
  nrec= N_ELEMENTS(record)
  cal_step= MAX(record.nsteps)  ; usually 544  
  
  ; determine scan & step for each record
  scan= LONARR(nrec) &  step= LONARR(nrec)
  scandx=0  &  stepdx=0
  FOR indx=0,nrec-1 DO BEGIN
    rec= record[indx]  &  _step= rec.mirror_step 
    IF (_step EQ cal_step) THEN stepdx= (stepdx+1)>cal_step ELSE BEGIN
      IF (_step LT stepdx) THEN scandx= scandx+1  ; or large time gap?
      stepdx= _step
    ENDELSE ;# &  print,indx,scandx,stepdx,step
    scan[indx]= scandx  &  step[indx]= stepdx
  ENDFOR
  
  ; create a 2-D array and insert records
  nscan= MAX(scan)+1  &  nstep= MAX(step)+1
  sname= TAG_NAMES(record[0],/STRUCTURE_NAME)
  status= EXECUTE('struct={'+sname+'}')
  result= REPLICATE(struct,nstep,nscan) 
 ; FOR indx=0,nrec-1 DO result[step[indx],scan[indx]]= record[indx]
  result[step+scan*nstep]= record  ; faster than loop, but cryptic
  
  unix_seconds= MEDIAN(result.seconds,/DOUBLE,DIMENSION=1)  ; zenith time for each scan
  
;  hist= HISTOGRAM(record.mirror_step,BINSIZE=1,MIN=0,MAX=cal_step)    ; # of records for each step index
;  nscan= MAX(hist[0:cal_step-1])  &  ncal= hist[cal_step]/nscan +1  
;  dstep= LONG(record[1:*].mirror_step) - LONG(record.mirror_step)
;  wjump= WHERE(dstep LT 0, njump)
;  IF (njump+1 NE nscan) THEN BEGIN
;    MESSAGE,'Warning- jump and scan difference',/INFORM
;    nscan= nscan > (njump+1)
;  ENDIF

;  result= REPLICATE(record[0],cal_step+ncal,nscan)
;  scandx=0  &  stepdx=0
;  FOR indx=0,nrec-1 DO BEGIN
;    rec= record[indx]  &  step= rec.mirror_step 
;    IF (step EQ cal_step) THEN stepdx= (stepdx+1)>cal_step ELSE BEGIN
;      IF (step LT stepdx) THEN scandx= scandx+1  ; or large time gap?
;      stepdx= step
;    ENDELSE ;# &  print,indx,scandx,stepdx,step
;    result[stepdx,scandx]= rec
;  ENDFOR

RETURN,result
END



FUNCTION NORSTAR_MSP_DAT_READFILE,filename,COUNT=n_records

  n_records= 0  &  record_length= 64
  
  IF (filename EQ '') THEN BEGIN
    MESSAGE,'Error- input filename is blank',/INFORMATIONAL
    RETURN,!null
  ENDIF
  
  filelist= FILE_SEARCH(filename,COUNT=nfiles)
  IF (nfiles EQ 0) THEN BEGIN
    MESSAGE,'Error- no files found',/INFORMATIONAL
    RETURN,!null
  ENDIF
  fname= filelist[0]

  OPENR,lun,fname,/GET_LUN,/SWAP_IF_BIG_ENDIAN
  filestat= FSTAT(lun)
  IF (filestat.size LT record_length) THEN BEGIN
    MESSAGE,'Error- file too small: '+fname+STRING(filestat.size),/INFORMATIONAL
    FREE_LUN,lun
    RETURN,!null    
  ENDIF

  IF ((filestat.size MOD record_length) NE 0) THEN $
    MESSAGE,'Warning- excess bytes in file: '+fname,/INFORMATIONAL

;# grab enough bytes for first record and do some sanity checks
  b= BYTARR(record_length)  &  READU,lun,b

  version= FIX(b[6:7],0) &  sitecode= STRING(b[2:5])
  CASE version OF
  2:record= {NORSTAR_MSP_DAT_IV2}
  3:record= {NORSTAR_MSP_DAT_IV3}
  4:record= {NORSTAR_MSP_DAT_IV4}
  ELSE: BEGIN
    MESSAGE,'Error- unrecognized record version in file: '+fname,/INFORMATIONAL
    RETURN,!NULL
    END
  ENDCASE
  
  IF (N_TAGS(record,/DATA_LENGTH) NE record_length) THEN MESSAGE,'Error- record_length mismatch'
  
  result= STREGEX(fname,'.*_ut.._(.*)_iv([2-4])\.dat$',/EXTRACT,/SUBEXPR)
  IF (result[0] NE fname) THEN MESSAGE,'Warning- unexpected file name: '+fname,/INFORM
  IF (result[1] NE sitecode) THEN MESSAGE,'Warning- file name sitecode mismatch: '+fname,/INFORM
;  IF (result[2] NE version) THEN MESSAGE,'Warning- file name version mismatch: '+fname,/INFORM
;# -this happens for all FSMI files in 2009 
  
;# re-read first record and do more validity testing
  POINT_LUN,lun,0  &  READU,lun,record
  IF (NORSTAR_MSP_DAT_VALID(record) NE 0) THEN BEGIN
    MESSAGE,'Error- invalid initial record in file: '+fname,/INFORMATIONAL
    FREE_LUN,lun
    RETURN,record    
  ENDIF

;# read all records in file
  n_records= filestat.size / record.packet_size
  data= REPLICATE(record,n_records)
  POINT_LUN,lun,0  &  READU,lun,data  &  FREE_LUN,lun
  
  FOR indx=1,nfiles-1 DO $
    data= [data,NORSTAR_MSP_DAT_READFILE(filelist[indx])] 

  n_records= N_ELEMENTS(data)
RETURN,data
END

;# some useful plots
PRO NORSTAR_MSP_DAT_PLOT_FIGURES,HARDCOPY=hardcopy

  ; create a multi-page postscript file in ./fig subdir
  IF KEYWORD_SET(HARDCOPY) THEN BEGIN
    _d= !d
    SET_PLOT,'PS'
    stack= SCOPE_TRACEBACK(/STRUCTURE)
    pwd= FILE_DIRNAME(stack[-1].filename)
    filename= STRJOIN([pwd,'fig','norstar_msp_dat_figures.ps'],PATH_SEP())
    DEVICE,FILENAME=filename,PREVIEW=0,/LANDSCAPE
    !p.font=0
  ENDIF

  datadir= 'c:/data/norstar/msp/2011/02/01/'  &  pattern= '*_GILL_iv[234].dat'
  filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)
  data= NORSTAR_MSP_DAT_READFILE(filelist)
  sitecode= STRING(data[0].sitecode)

  time= data.seconds + data.microseconds*1d-6
  title0= sitecode+'  '+SYSTIME(0,time[0],/UTC) 
  dtime= time[1:*] - time
  
  blank= REPLICATE(' ',30)

  ; two scans (60-seconds) showing the red channels (signal & background) and the mirror step index
  t0= 25000  &  t= time-time[0]-t0  
  title= sitecode+'  '+SYSTIME(0,time[0]+t0,/UTC)
  PLOT,t,data.counts[7],PSYM=4,SYMSIZE=0.4,XRANGE=[0,60],YRANGE=[0,566] $
     ,XTITLE='time [seconds]',YTITLE='red channel=0,7 [counts]',TITLE=title
  OPLOT,t,data.counts[0],PSYM=1,SYMSIZE=0.4
  OPLOT,t,data.mirror_step,THICK=2

  ; all 8 channels at zenith(ish) over 24-hours
  w= where(data.mirror_step EQ 272,nw)
  d= data[w]  &  t= time[w]
  PLOT,d.counts[1]>1,/YLOG,YRANGE=[3,65535],YSTYLE=1,/NODATA $
     ,XTITLE='record number',YTITLE='zenith bin=272 channel=0-7 [counts]',TITLE=title0   
  FOR indx=0,7 DO OPLOT,SMOOTH(d.counts[indx],3)

  ; time difference between sucessive records over 1-hour to emphasize 0.5ms jitter
  PLOT,time-time[0],dtime*1d3,YRANGE=50 + 1.0*[-1,+1],XRANGE=[0,3600] $
     ,XTITLE='time [seconds]',YTITLE='delta-T [milliseconds]',TITLE=title0
  
  ; time difference between sucessive records over 1-minute to emphasize 0.005ms oscillation
  PLOT,time-time[0],dtime*1d3,YRANGE=50 + 0.05*[-1,+1],XRANGE=[0,60],YSTYLE=1 $
     ,XTITLE='time [seconds]',YTITLE='delta-T [milliseconds]',TITLE=title0

  ; histogram of time differences (logarithmic with inset linear) 
  hist= HISTOGRAM(dtime,BINSIZE=1d-6,MIN=0,MAX=60d-3)
  PLOT,FINDGEN(60000)/1d3,hist>1,/YLOG,XRANGE=[49.3,50.7],XSTYLE=1  $
     ,XTITLE='delta-T [milliseconds]',YTITLE='number of records',TITLE=title0     
  PLOT,FINDGEN(60000)/1d3,hist/TOTAL(hist),XRANGE=[49.98,50.02],XSTYLE=1,POSITION=[0.6,0.6,0.925,0.9],/NOERASE,YTICKNAME=blank,PSYM=10

  IF KEYWORD_SET(HARDCOPY) THEN BEGIN
    DEVICE,/CLOSE_FILE
    SET_PLOT,_d.name
    !p.font=-1
  ENDIF

  RETURN
END



;;# Generic read routine.  Currently just a wrapper
;;#
;FUNCTION NORSTAR_MSP_READFILE,filename,COUNT=n_records
;  result= NORSTAR_MSP_DAT_READFILE(filename,COUNT=n_records)
;RETURN,result
;END


;# Dummy structure(less) definition.  Call this to force compilation of readfile et al.
;#
PRO NORSTAR_MSP_DAT__DEFINE
  RETURN
END

;# Test readfile by running it on *all* data files.  Slow but effective.
PRO NORSTAR_MSP_DAT_TEST_READ_ALL,datadir
  datadir= 'c:\data\norstar\msp\'
  yearlist= FILE_SEARCH(datadir+'????',/TEST_DIRECTORY,/MARK_DIRECTORY,COUNT=ndirs)  &  PRINT,ndirs
  FOREACH yeardir,yearlist DO BEGIN
;    daylist= FILE_SEARCH(yeardir+'02\09',/TEST_DIRECTORY,/MARK_DIRECTORY,COUNT=ndirs)
    daylist= FILE_SEARCH(yeardir+'??\??',/TEST_DIRECTORY,/MARK_DIRECTORY,COUNT=ndirs)
    PRINT,yeardir,ndirs,FORMAT='(A,2X,I)'
    FOREACH daydir,daylist DO BEGIN
;      filelist= FILE_SEARCH(daydir+'*_ut??_*_iv[2].dat',/TEST_READ,COUNT=nfiles)  
      filelist= FILE_SEARCH(daydir+'*_ut??_*_iv[234].dat',/TEST_READ,COUNT=nfiles)
      PRINT,daydir,nfiles,FORMAT='(A,2X,I)' 
      IF (nfiles EQ 0) THEN CONTINUE  ;!! the foreach loop will call once with an empty string !!
      FOREACH filename,filelist DO data= NORSTAR_MSP_DAT_READFILE(filename,COUNT=nrec) 
    ENDFOREACH
  ENDFOREACH
  RETURN
END

;C:\data\norstar\msp\2009\01\01\20090101_ut00_FSMI_iv2.dat
;** Structure NORSTAR_MSP_DAT_IV2, 16 tags, length=68, data length=64:
;   HEADER          BYTE      Array[2]  UC
;   SITECODE        BYTE      Array[4]  FSMI
;   VERSION         UINT             3  !!!
;   NSTEPS          UINT           544
;   DEVICE_ID       UINT             0
;   PACKET_SIZE     UINT             1  !!!
;   MODE            UINT            64  !!!
;   STEP_TABLE_VERSION
;                   UINT             0
;   SECONDS         LONG         125829137
;   MICROSECONDS    LONG       -1930540708
;   MIRROR_STEP     UINT             0
;   START_OF_DATA   UINT             0
;   COUNTS          UINT      Array[8]  255     268     413    3329    3332    3514    3793    4436
;   END_OF_DATA     UINT           193
;   PADDING         BYTE      Array[14]  0 255   0 255   0 255   0 255   0 255   0 255   0 255
;   TRAILER         BYTE      Array[2]   CU

; C:\data\norstar\msp\2011\02\16\20110216_ut19_GILL_iv3.dat
;** Structure NORSTAR_MSP_DAT_IV3, 17 tags, length=64, data length=64:
;   HEADER          BYTE      Array[2] "te"
;   SITECODE        BYTE      Array[4] "st\nt"
;   VERSION         UINT         29541
;   NSTEPS          UINT          2676
;   NORTH_STEP      UINT         25972
;   DEVICE_ID       UINT         29811
;   PACKET_SIZE     UINT         29706
;   MODE            UINT         29541
;   STEP_TABLE_VERSION
;                   UINT          2676
;   SECONDS         LONG        1953719668
;   MICROSECONDS    LONG        1936028682
;   MIRROR_STEP     UINT          2676
;   START_OF_DATA   UINT         25972
;   COUNTS          UINT      Array[8]
;   END_OF_DATA     UINT         16384
;   PADDING         BYTE      Array[12]
;   TRAILER         BYTE      Array[2]

PRO TEST
 rec= {NORSTAR_MSP_DAT_IV2}
 datadir= 'c:/data/norstar/msp/2009/01/01/'
 filelist= FILE_SEARCH(datadir,'*FSMI_iv2.dat',COUNT=nfiles)
 OPENR,lun,filelist[0],/GET_LUN,/SWAP_IF_BIG_ENDIAN
 filestat= FSTAT(lun)
 READU,lun,rec
 HELP,/STR,rec
 nrec= filestat.size / rec.packet_size
 dat= REPLICATE(rec,nrec)
 POINT_LUN,lun,0
 READU,lun,dat
 FREE_LUN,lun
END

 datadir= 'c:\data\norstar\msp\'
; daylist= FILE_SEARCH(datadir+'????/??/??',/TEST_DIRECTORY,/MARK_DIRECTORY,COUNT=ndirs)
 pattern= '*_ATHA_iv[234].dat'
 FOREACH daydir,daylist DO BEGIN 
   f= FILE_SEARCH(daydir,pattern,COUNT=nfiles)
   IF (nfiles EQ 0) THEN CONTINUE
   d= NORSTAR_MSP_DAT_READFILE(f[0])
   PRINT,f[0],d[0].step_table_version
 ENDFOREACH
 
 stop

 datadir= 'c:/data/norstar/msp/2011/02/01/'  &  pattern= '*_GILL_iv[234].dat'
 filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)
 data= NORSTAR_MSP_DAT_READFILE(filelist[0])
 plot,data[9999:9999+1999].counts[0]

 data= NORSTAR_MSP_DAT_READFILE(filelist)
 dat= NORSTAR_MSP_DAT_RESHAPE(data,UNIX_SECONDS=time)
 dat= TRANSPOSE(SQRT(dat.counts[1]))
 SHADE_SURF,dat,SHADE=BYTSCL(dat),ax=90,az=0
 hour= ((time-time[0])/3600.0) > 0
 SHADE_SURF,dat,hour,INDGEN(564),SHADE=BYTSCL(dat),ax=90,az=0,YRANGE=[563,0]
 SHADE_SURF,dat[100:-200,0:543],hour[100:-200],INDGEN(544),SHADE=BYTSCL(dat[100:-200,0:543]),ax=90,az=0,YRANGE=[544,0],xrange=[3,3.5]
 
 
 datadir= 'c:\data\norstar\msp\'
 daylist= FILE_SEARCH(datadir+'????/??/??',/TEST_DIRECTORY,/MARK_DIRECTORY,COUNT=ndirs)
 pattern= '*_GILL_iv[234].dat'
 FOREACH daydir,daylist DO BEGIN 
   f= FILE_SEARCH(datadir,pattern,COUNT=nfiles)
   d= NORSTAR_MSP_DAT_READFILE(f[0])
 ENDFOREACH
 
END