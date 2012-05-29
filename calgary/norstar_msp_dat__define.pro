;# Subversion $Id$
;# 10 Aug 2010: Brian Jackel wrote based on info provided by Greg Baker
;# 28 May 2012: Bjj updated

;+
; This IDL program reads data files produced by the NORSTAR 
; meridian scanning photometer (MSP) UDP packet stream.
; 
; Example:
;  datadir= 'c:/data/norstar/msp/2012/05/01/'
;  filelist= FILE_SEARCH(datadir,'*_GILL_iv[23].dat',COUNT=nfiles)
;  data= NORSTAR_MSP_DAT_READFILE(filelist[0])
;  plot,data[9999:9999+1999].counts[0]
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
       ,step_table_version:0U   $  ;# 0x0010
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
  nscan= MAX(scan)  &  nstep= MAX(step)
  result= REPLICATE(record[0],nstep,nscan) 
  ;FOR indx=0,nrec-1 DO result[step[indx],scan[indx]]= record[indx]
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

  record= {NORSTAR_MSP_DAT_IV3}  &  n_records= 0
  filelist= FILE_SEARCH(filename,COUNT=nfiles)
  IF (nfiles EQ 0) THEN BEGIN
    MESSAGE,'Error- no files found',/INFORMATIONAL
    RETURN,record
  ENDIF
  fname= filelist[0]

  CASE 1 OF
  STRMATCH(fname,'*_ut??_*_iv2.dat'):record= {NORSTAR_MSP_DAT_IV2}
  STRMATCH(fname,'*_ut??_*_iv3.dat'):record= {NORSTAR_MSP_DAT_IV3}
  STRMATCH(fname,'*_ut??_*_iv4.dat'):record= {NORSTAR_MSP_DAT_IV4}
  ELSE: BEGIN
    MESSAGE,'Error- unrecognized file name: '+fname
    RETURN,!NULL
    END
  ENDCASE

  OPENR,lun,fname,/GET_LUN,/SWAP_IF_BIG_ENDIAN
  filestat= FSTAT(lun)
  IF (filestat.size LT N_TAGS(record,/DATA_LENGTH)) THEN BEGIN
    MESSAGE,'Error- file too small: '+fname,/INFORMATIONAL
    RETURN,record    
  ENDIF 

  READU,lun,record
  IF (NORSTAR_MSP_DAT_VALID(record) NE 0) THEN BEGIN
    MESSAGE,'Error- invalid initial record in file: '+fname,/INFORMATIONAL
    RETURN,record    
  ENDIF
  
  nrecords= filestat.size / record.packet_size
  data= REPLICATE(record,nrecords)
  POINT_LUN,lun,0
  READU,lun,data
  FREE_LUN,lun
  
  FOR indx=1,nfiles-1 DO $
    data= [data,NORSTAR_MSP_DAT_READFILE(filelist[indx])] 

  n_records= N_ELEMENTS(data)
RETURN,data
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

 datadir= 'c:/data/norstar/msp/2011/02/01/'
 filelist= FILE_SEARCH(datadir,'*_GILL_iv[23].dat',COUNT=nfiles)
 data= NORSTAR_MSP_DAT_READFILE(filelist[0])
 plot,data[9999:9999+1999].counts[0]
 dat= NORSTAR_MSP_DAT_RESHAPE(data,UNIX_SECONDS=time)
 dat= TRANSPOSE(SQRT(dat.counts[1]))
 SHADE_SURF,dat,SHADE=BYTSCL(dat),ax=90,az=0
 ;SHADE_SURF,dat,(time-MIN(time))/3600.0,INDGEN(563),SHADE=BYTSCL(dat),ax=90,az=0,YRANGE=[563,0]
 
END