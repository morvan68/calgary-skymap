

;# Themis all-sky imager data with 256x256 pixels at a 3-second cadence are located in "stream0".
;# A variety of useful data products are located in "stream2".  These include keograms that can 
;# be used to find interesting intervals eg. 
;#  c:\data\themis\imager\stream2\2011\01\01\gill_themis19\20110101__gill_themis19_full-keogram.pgm.jpg

;# 1-minute averages (in 1-hour files) are also located in stream2.  These can be read quickly and
;# have increased signal-to-noise that is useful for star detection.

datadir= 'c:\data\themis\imager\stream2\2011\01\01\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

; read from ut00 to ut13 (skip ut23 which is sunset from next day) should be 817 frames
THEMIS_IMAGER_READFILE,filelist[0:14],data,metadata,COUNT=nframes  &  PRINT,nframes

minval= MIN(data)  &  maxval= 100  ; crop range to see stars 

; show as movie using square-root scaling to enhance contrast & small wait on fast machines
FOR indx=0,nframes-1 DO BEGIN & TVSCL,SQRT(data[*,*,indx]-minval)<maxval & WAIT,0.01 & ENDFOR

; stack two hours without aurora to bring out star tracks
TVSCL,SQRT(TOTAL(data[*,*,100:219]-minval,3)/120)<60

END