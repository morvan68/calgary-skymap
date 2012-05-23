;# Subversion $Id$

;# This IDL program reads data files produced by the
;# NORSTAR meridian scanning photometer UDP packet stream

;# 10 Aug 2010: written by Brian Jackel


;index_6300=0 ; red line
;index_4800=1 ; blue background
;index_4861A=2 ; H-beta
;index_4861B=3 ; H-beta
;index_4950=4 ; blue background
;index_4709=5 ; Nitrogen blue line
;index_5577=6 ;  green line
;index_6250=7 ; red line background


PRO NORSTAR_MSP_DAT_IV2__DEFINE
  tmp= {NORSTAR_MSP_DAT_IV2     $ 
       ,header:BYTARR(2)        $  ;# should be "UC" = 0x5534
       ,sitecode:BYTARR(4)      $  ;# 4 characters eg. "GILL"
       ,version:0U              $
       ,nsteps:0U               $
       ,north_step:0U           $  ;# 0x0000
       ,device_id:0U            $  ;# 0x0001 for prototype?
       ,packet_size:0U          $  ;# 0x0064
       ,mode:0U                 $  ;# 0x0101 nominal
       ,step_table_version:0U   $
       ,seconds:0L              $
       ,microseconds:0L         $
       ,mirror_step:0U          $
       ,start_of_data:0U        $  ;# should be 0xff00 
       ,counts:UINTARR(8)       $
       ,end_of_data:0U          $  ;# should be 0xff00       
       ,padding:BYTARR(12)      $  ;# 
       ,trailer:BYTARR(2)       $  ;# should be "CU" = 0x3455
  }
RETURN
END

PRO NORSTAR_MSP_DAT_IV3__DEFINE
  tmp= {NORSTAR_MSP_DAT_IV3     $ 
       ,header:BYTARR(2)        $  ;# should be "UC" = 0x5534
       ,sitecode:BYTARR(4)      $  ;# 4 characters eg. "GILL"
       ,version:0U              $  ;# packet version (0x0003)
       ,nsteps:0U               $  ;# number of mirror steps per scan
       ,north_step:0U           $  ;# 0x0000
       ,device_id:0U            $  ;# 0x0001 for prototype
       ,packet_size:0U          $  ;# 0x0064
       ,mode:0U                 $  ;# 0x0101 nominal
       ,step_table_version:0U   $  ;# 0x0010
       ,seconds:0L              $
       ,microseconds:0L         $
       ,mirror_step:0U          $
       ,start_of_data:0U        $  ;# should be 0x00ff 
       ,counts:UINTARR(8)       $  ;# 8 filters x 16 bits
       ,end_of_data:0U          $  ;# should be 0xff00       
       ,padding:BYTARR(12)      $  ;# 0xff00
       ,trailer:BYTARR(2)       $  ;# should be "CU" = 0x3455
  }
RETURN
END


FUNCTION NORSTAR_MSP_DAT_VALID,record
  result= 1
  
  IF (STRING(record.header) NE 'UC') THEN BEGIN
    mesg= 'Warning- unexpected header: '+STRING(record.header)
    MESSAGE,mesg,/INFORMATIONAL  &  result= 0
  ENDIF 

  IF (record.packet_size NE 64) THEN BEGIN
    mesg= 'Warning- unexpected packet size: '+STRING(record.packet_size)
    MESSAGE,mesg,/INFORMATIONAL  &  result= 0
  ENDIF 

  IF (STRING(record.trailer) NE 'CU') THEN BEGIN
    mesg= 'Warning- unexpected trailer: '+STRING(record.trailer)
    MESSAGE,mesg,/INFORMATIONAL  &  result= 0
  ENDIF

RETURN,result
END


FUNCTION NORSTAR_MSP_DAT_READFILE,filename,COUNT=n_records

  record= {NORSTAR_MSP_DAT_IV3}  &  n_records= 0
  filelist= FILE_SEARCH(filename,COUNT=nfiles)
  IF (nfiles EQ 0) THEN BEGIN
    MESSAGE,'Error- no files found',/INFORMATIONAL
    RETURN,record
  ENDIF

  OPENR,lun,filelist[0],/GET_LUN,/SWAP_IF_BIG_ENDIAN
  filestat= FSTAT(lun)
  IF (filestat.size LT 64) THEN BEGIN
    MESSAGE,'Error- file too small: '+filelist[0],/INFORMATIONAL
    RETURN,record    
  ENDIF 

  READU,lun,record
  IF NOT NORSTAR_MSP_DAT_VALID(record) THEN BEGIN
    MESSAGE,'Error- invalid record',/INFORMATIONAL
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


;# Generic read routine.  Currently just a wrapper
;#
FUNCTION NORSTAR_MSP_READFILE,filename,COUNT=n_records
  result= NORSTAR_MSP_DAT_READFILE(filename,COUNT=n_records)
RETURN,result
END


PRO TEST
 rec= {NORSTAR_MSP_DAT}
 datadir= 'c:/data/norstar/msp/2009/01/01/'
 filelist= FILE_SEARCH(datadir,'*_iv[23].dat',COUNT=nfiles)
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

 datadir= 'c:/data/norstar/msp/2012/05/01/'
 filelist= FILE_SEARCH(datadir,'*_GILL_iv[23].dat',COUNT=nfiles)
 data= NORSTAR_MSP_READFILE(filelist[0])
 plot,data[9999:9999+1999].counts[0]
END