datadir= 'c:\data\themis\imager\stream2\2011\01\01\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles

THEMIS_IMAGER_READFILE,filelist[0:14],data,metadata,COUNT=nframes  &  PRINT,nframes
minval= MIN(data)  &  maxval= MAX(data)
;rot=rotate(reform(data[(size(data))[1]/2,*,*],256,(size(data))[3]),3)
;tvscl,sqrt(rot-minval)<60

;x=[]
;init=metadata[0].exposure_start_cdf
;for i=1,(size(metadata))[1]-1 do begin & x=[x, metadata[i].exposure_start_cdf-init] & endfor

slice=reform(data[(size(data))[1]/2,*,*],256,(size(data))[3])
plotdata=slice[*,0]
plottime=metadata[0].exposure_start_string
plotcdf=metadata[0].exposure_start_cdf
for i=1,(size(metadata))[1]-1 do begin
  fillwidth=round((metadata[i].exposure_start_cdf-metadata[i-1].exposure_start_cdf)/60000)-1
  if fillwidth gt 0 then begin
    fill=uintarr(256,fillwidth)
    strfill=strarr(1,fillwidth)
    cdffill=dblarr(1,fillwidth)
    plotdata=[[plotdata],[fill]]
    plottime=[[plottime],[strfill]]
    plotcdf=[[plotcdf],[cdffill]]
  endif
  plotdata=[[plotdata],[slice[*,i]]]
  plottime=[[plottime],[metadata[i].exposure_start_string]]
  plotcdf=[[plotcdf],[metadata[i].exposure_start_cdf]]
endfor
tvscl,rotate(plotdata-minval,3)<1400
end

;plotdata=slice[*,0] & for i=1,(size(metadata))[1]-1 do begin & fillwidth=round((metadata[i].exposure_start_cdf-metadata[i-1].exposure_start_cdf)/60000)-1 & if fillwidth gt 0 then begin & fill=uintarr(256,fillwidth) & plotdata=[[plotdata],[fill]] & endif & plotdata=[[plotdata],[slice[*,i]]] & endfor &