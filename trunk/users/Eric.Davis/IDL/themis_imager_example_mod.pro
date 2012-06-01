

;# Themis all-sky imager data with 256x256 pixels at a 3-second cadence are located in "stream0".
;# A variety of useful data products are located in "stream2".  These include keograms that can 
;# be used to find interesting intervals eg. 
;#  c:\data\themis\imager\stream2\2011\01\01\gill_themis19\20110101__gill_themis19_full-keogram.pgm.jpg

;# 1-minute averages (in 1-hour files) are also located in stream2.  These can be read quickly and
;# have increased signal-to-noise that is useful for star detection.

datadir1= 'c:\data\themis\imager\stream2\2011\01\01\gill_themis19\'  ; Gillam January 1, 2011
pattern1= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist1= FILE_SEARCH(datadir1,pattern1,COUNT=nfiles)  

datadir2= 'c:\data\themis\imager\stream2\2011\01\03\gill_themis19\'  ; Gillam January 1, 2011
pattern2= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist2= FILE_SEARCH(datadir2,pattern2,COUNT=nfiles)  


n1=(size(filelist1))(1)-1
n2=(size(filelist2))(1)-1

THEMIS_IMAGER_READFILE,filelist1[0:n1],data1,metadata1,COUNT=nframes 
THEMIS_IMAGER_READFILE,filelist2[0:n2],data2,metadata2,COUNT=nframes



slice1=data1[128,*,*]
colnum1=(size(slice1))(3)
keo1=reform(slice1,256,colnum1)
keofinal1=rotate(keo1,3)

slice2=data2[128,*,*]
colnum2=(size(slice2))(3)
keo2=reform(slice2,256,colnum2)
keofinal2=rotate(keo2,3)

tvscl,alog(keofinal1)
tvscl,alog(keofinal2),0,256

psf1=keofinal1(329,45:55)
psf=reform(psf1,11)
x=indgen(11)-5


END