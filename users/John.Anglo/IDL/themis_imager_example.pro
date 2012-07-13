

;# Themis all-sky imager data with 256x256 pixels at a 3-second cadence are located in "stream0".
;# A variety of useful data products are located in "stream2".  These include keograms that can 
;# be used to find interesting intervals eg. 
;#  c:\data\themis\imager\stream2\2011\01\01\gill_themis19\20110101__gill_themis19_full-keogram.pgm.jpg

;# 1-minute averages (in 1-hour files) are also located in stream2.  These can be read quickly and
;# have increased signal-to-noise that is useful for star detection.

datadir= 'c:\data\themis\imager\stream2\2011\01\01\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

; read from ut00 to ut12 (skip ut23 which is sunset from next day) should be 817 frames
THEMIS_IMAGER_READFILE,filelist[0:12],data,metadata,COUNT=nframes  &  PRINT,nframes

minval= MIN(data)  &  maxval= 100  ; crop range to see stars 

; show as movie using square-root scaling to enhance contrast & small wait on fast machines
FOR indx=0,nframes-1 DO BEGIN & TVSCL,rebin(SQRT(data[*,*,indx]-minval),512,512,/sample)<maxval & WAIT,0.01 & ENDFOR

; stack two hours without aurora to bring out star tracks
TVSCL,rebin(SQRT(TOTAL(data[*,*,60:299]-minval,3)/240)<60,512,512,/sample)
var=variance(data[*,*,60:299]-minval,dimension=3)
ave=total((data[*,*,60:299]-minval)^2d,3)/240
;shade_surf,var<(max(var)/800),ax=90,az=0
var = var<(max(var)/800)>min(var)
ave = ave<(max(ave)/800)>min(ave)
;tvscl,ave * var

;test_data=ulonarr(256,256)
;for i = 1,10 do test_data=test_data + raster_circle(i*10, 128,128, 256,256) 

kernelSize = [3, 3]  
kernel = REPLICATE(-1./9., kernelSize[0], kernelSize[1])  
kernel[1, 1] = 1.  
filteredImage = CONVOL(FLOAT(total((data[*,*,60:299]-minval),3)), kernel, /CENTER, /EDGE_TRUNCATE)

filter=raster_circle(72,131,17,256,120,/fill)-raster_circle(64,131,17,256,120,/fill)

FOR indx=0,619 DO BEGIN & tvscl,rebin((SQRT(data[*,60:179,indx]-minval))*(1+filter),512,240,/sample)<maxval & WAIT,0.01 & ENDFOR

maxct = ulonarr(700)
radius = 1
for indx = 20,469 do begin & temp = max(data[0:219,80:179,indx]*filter,coord) & coord = array_indices([220,100], coord, /dimensions) & tfil = raster_circle(10,coord[0],coord[1],220,100) & frame = (data[0:219,80:179,indx]*tfil) & maxct[indx-20] = total(frame)/total(frame ne 0) & tvscl,rebin(frame+2*data[0:219,80:179,indx], 440,200,/sample)& wait,0.01 & endfor
for indx = 0,469 do begin & temp = max(data[0:179,80:255,indx]*filter,coord) & coord = array_indices([180,196], coord, /dimensions) & tfil = intarr(180,196) & tfil[coord[0]-radius:coord[0]+radius,coord[1]-radius:coord[1]+radius]=1 & frame = (data[0:179,80:255,indx]*tfil) & maxct[indx] = total(frame[(sort(frame))[-4:*]])/4.0 & tvscl,rebin(frame+2*data[0:179,80:255,indx], 360,352,/sample)& wait,0.01 & endfor
plot,maxct
for indx = 0,239 do maxct[indx]=[max(data[0:179,80:255,indx]*filter)]

for indx = 0,500 do tvscl,raster_circle(indx,500,500,1000,1000)
for indx = 0,500 do tvscl,raster_circle(indx,500,500,1000,1000,/fill)

maxct = ulonarr(620)
maxctf = fltarr(620)
bgct = fltarr(620)
radius = 1
for indx = 0,619 do begin
  temp = max(data[*,60:179,indx]*filter,coord)
  coord = array_indices([256,120],coord,/dimensions)
  tfil = raster_circle(3,coord[0],coord[1],256,120,/fill)
  tvscl,tfil*data[*,60:179,indx]+data[*,60:179,indx]
  bgct[indx] = total(raster_circle(5,coord[0],coord[1],256,120)*data[*,60:179,indx])/total(raster_circle(5,coord[0],coord[1],256,120))
  maxct[indx] = total(((data[*,60:179,indx]-bgct(indx))>0)*tfil)
endfor 
END