;procedure for drawing bitmask image for star pointing
;object coords in RA/dec
;location in degrees, lat/long
;time in ymdhms
;optional arguments:
;pole: angle of celestial pole to 12:00 on image, default is 0, enter in degrees
;zenith: x-y coords of zenith, default is 127,127
;frame: x-y dimensions of frame, default is 256,256
;ratio: dec to pixel radius ratio, default is 81.49
;bound: largest zenith angle to draw for in degrees

function pointing_mask, object, location, time, pole = pole, zenith = zenith, frame = frame, ratio = ratio, bound=bound, z_angle=z_angle
  if ~keyword_set(pole) then pole = 0
  if ~keyword_set(zenith) then zenith  =[128,128]
  if ~keyword_set(frame) then frame = [256,256]
  if ~keyword_set(ratio) then ratio = 256d/!dpi
  if ~keyword_set(bound) then begin & bound = !dpi/2
  endif else bound = bound*!dpi/180d
  p = pole*!dpi/180d
  
  
  location=  [100.0,[location]]
  skymap_geographic__define
  site= skymap_geographic(location)
  skymap_geo_aim__define
  gaim= skymap_geo_aim(site)
  
  gaim.set_time,time,/ymdhms
  gaim.set_aim,object,/astro,/degrees
  
  coords = reform(gaim.get_aim())
  if arg_present(z_angle) then z_angle = coords[1]
  if coords[1] gt bound then return, ulonarr(256,256)
  
  point = round(zenith + rotate(cv_coord(from_polar=(coords*[1,ratio]+[!dpi/2+p,0]),/to_rect),4))
  rad = 5
  
  return, raster_circle(rad,point[0],point[1],frame[0],frame[1],/fill)
end

pro manual_refine_params, data, metadata, pole, zenith, ratio, rotate = rotate
  length = (size(data))[3]
  rdata=ulonarr(256,256,length)
  if keyword_set(rotate) then begin & for i = 0, length-1 do rdata[*,*,i] = rotate(data[*,*,i],7)
  endif else rdata = data
  
  min_val = min(rdata)
  i=0
  c = 'u'
  ratio = 256d/!dpi
  pole = 0
  zenith  =[128,128]
  time = (strsplit(metadata[i].exposure_start_string,":- ",/extract))[0:5]
  frame=rdata[*,*,i]*(1+pointing_mask([79.35,46.01],[56.35,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
    +pointing_mask([88.8,7.41],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
    +pointing_mask([101.4,-16.72],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
    +pointing_mask([115,5.2],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
    +pointing_mask([116.5,28],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
    +pointing_mask([41.2,89.31],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio))
    print,'wasd to move zenith, q/e to change pole angle, o/l to raise/lower ratio, y to finalize parameters'
    tvscl,rebin(sqrt(frame-min_val),512,512,/sample)
    while c ne 'y' do begin
      c = get_kbrd(1,/escape)
      case c of
      'w':  zenith[1]++
      'a':  zenith[0]--
      's':  zenith[1]--
      'd':  zenith[0]++
      'q':  pole++
      'e':  pole--
      'm':  begin
        i++
        i = i mod length
      end
      'n': begin
        i--
        i = i mod length
      end
      'o':  ratio+=0.5
      'l':  ratio-=0.5
      else:
      endcase
    time = (strsplit(metadata[i].exposure_start_string,":- ",/extract))[0:5]
    frame=rdata[*,*,i]*(1+pointing_mask([79.35,46.01],[56.35,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
      +pointing_mask([88.8,7.41],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
      +pointing_mask([101.4,-16.72],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
      +pointing_mask([115,5.2],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
      +pointing_mask([116.5,28],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
      +pointing_mask([41.2,89.31],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio))
    tvscl,rebin(sqrt(frame-min_val),512,512,/sample)
    endwhile
end

pro get_count, data, metadata, index, object, location, pole, zenith, ratio, result, z_angle = z_angle, refine_dec = dec
  min_val = min(data)
  time = (strsplit(metadata[index].exposure_start_string,":- ",/extract))[0:5]
  tmp = 0d
  filter1 = pointing_mask(object, location, time, pole = pole, zenith = zenith, ratio = ratio, z_angle = tmp_z)
  
  if total(filter1) eq 0 then begin & result = 0
  endif else begin
    tmp = max(data[*,*,index]*filter1, coords)
    coords = array_indices([256,256],coords,/dimensions)
    filter2 = raster_circle(3, coords[0],coords[1], 256,256,/fill)
    bgfilter = raster_circle(4,coords[0],coords[1], 256,256)
    bg = total(data[*,*,index]*bgfilter)/total(bgfilter)
    result = total((data[*,*,index]-bg)*filter2)>0
    tvscl,rebin(sqrt((data[*,*,index]-min_val)*(1+filter2)),512,512,/sample)
  endelse
  if arg_present(z_angle) then z_angle = tmp_z
end

;manual_refine_params, rdata, metadata, pole, zenith, ratio
;for i = 0, length-1 do begin
;  time = (strsplit(metadata[i].exposure_start_string,":- ",/extract))[0:5]
;  fdata[*,*,i]=rdata[*,*,i]*(1+pointing_mask([79.35,46.01],[56.35,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
;  +pointing_mask([88.8,7.41],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
;  +pointing_mask([101.4,-16.72],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
;  +pointing_mask([115,5.2],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
;  +pointing_mask([116.5,28],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio)$
;  +pointing_mask([41.2,89.31],[56.3,265.3],time,pole=pole,zenith=zenith,ratio=ratio))
;endfor
;play_data,fdata

pro data_init, data,metadata, day,site
  datadir = 'c:\data\themis\imager\stream2\2011\01\'+string(day,format='(i2.2)')+'\'+site+'\'
  pattern= 'ut??\*_full-average.pgm.gz'
  filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles
  
  THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
end

print, pole, zenith, ratio
ra = 79.35
dec = 46.01

tsize = (size(data))[3]
rdata = ulonarr(256,256,tsize)
z_angle = dblarr(tsize)
for i = 0, tsize-1 do rdata[*,*,i] = rotate(data[*,*,i],7)
result=ulonarr(tsize)
for i = 0,tsize-1 do begin
  tmp2 = 0
  get_count, rdata, metadata, i, [ra, dec], [56.3,265.3], pole,zenith, ratio, tmp, z_angle=z_angle[i]
  ;z_angle[i] = tmp2
  result[i] = tmp
endfor
end