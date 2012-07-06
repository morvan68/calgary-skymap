pro region_select, data, keo, region, start_x, end_x
  tsize = (size(keo))[1]
  tvscl,rebin(sqrt(keo<max(keo)-min(keo)),tsize,512,/sample)
  c = 'x'
  start_x = 0
  print, "start frame: c to use cursor, l/r to move forward/backward, n to enter frame number, y to finalize selection"
  while c ne 'y' do begin
    c = get_kbrd(1,/escape)
    case c of
      'c': begin
        print, "select start frame using cursor:"
        cursor,start_x,y,3,/device
        end
      'l': begin
        start_x--
        start_x = start_x mod tsize
        end
      'r': begin 
        start_x++
        start_x = start_x mod tsize
        end
      'n': begin
        print, "enter frame number:"
        read, tmp
        if tmp lt tsize or tmp ge 0 then begin
          start_x = tmp
        endif else print, "number is out of bounds"
        end
      else:
    endcase
    tvscl, sqrt(data[*,*,start_x]-min(data)), 0, 512
  endwhile
  c = 'x'
  end_x = start_x
  print, "end frame: c to use cursor, l/r to move forward/backward, n to enter frame number, y to finalize selection"
  while c ne 'y' do begin
    c = get_kbrd(1,/escape)
    case c of
      'c': begin
        print, "select end frame using cursor:"
        cursor,end_x,y,3,/device
        end
      'l': begin
        end_x--
        end_x = end_x mod tsize
        end
      'r': begin 
        end_x++
        end_x = end_x mod tsize
         end
      'n': begin
        print, "enter frame number:"
        read, tmp
        if tmp lt tsize or tmp ge 0 then begin
          end_x = tmp
        endif else print, "number is out of bounds"
        end
      else:
    endcase
    tvscl, sqrt(data[*,*,end_x]-min(data)), 0, 512
  endwhile
  print,start_x, end_x
  region = data[*,*,start_x:end_x]
end

pro zenith_select, data, z_x, z_y
  t = 0
  c = 'x'
  print, "y to select from frame, l/r to change frame"
  tvscl, sqrt(data[*,*,t]-min(data))
  while c ne 'y' do begin
    c = get_kbrd(1,/escape)
    if c eq 'l' then t--
    if c eq 'r' then t++
    if t gt (size(data))[3] then t = 0
    tvscl, sqrt(data[*,*,t]-min(data))
  endwhile
  cursor,z_x,z_y,3,/device
  filter = raster_circle(3, z_x, z_y, 256,256, /fill)
  tmp = max(filter*data[*,*,t],coords)
  coords = array_indices([256,256],coords,/dimensions)
  z_x = coords[0] & z_y = coords[1]
  print,z_x, z_y
end

pro pointing, data, z_x, z_y, r_min, r_max
  minval = min(data)
  t = 0
  c = '0'
  print, "enter initial radius"
  read, r
  r_min = r-3
  r_max = r+3
  filter = raster_circle(r_max, z_x,z_y, 256,256, /fill)-raster_circle(r_min, z_x,z_y, 256,256, /fill)
  tvscl, rebin((filter+1)*sqrt(data[*,*,t]-min(data)),512,512)
  print, "l/r to change frame, u to change max, d to change min, n to choose new radius, , f to enter frame number, y to finalize"
  while c ne 'y' do begin
    c = get_kbrd(1,/escape)
    case c of
      'u': begin
        print, r_max, " enter new max:"
        read,r_max
        filter = raster_circle(r_max, z_x,z_y, 256,256, /fill)-raster_circle(r_min, z_x,z_y, 256,256, /fill)
        end
      'd': begin
        print, r_min, " enter new min:"
        read,r_min
        filter = raster_circle(r_max, z_x,z_y, 256,256, /fill)-raster_circle(r_min, z_x,z_y, 256,256, /fill)
        end
      'l': t--
      'r': t++
      'n': begin
        print, r, " enter new radius:"
        read, r
        r_min = r-3
        r_max = r+3
        filter = raster_circle(r_max, z_x,z_y, 256,256, /fill)-raster_circle(r_min, z_x,z_y, 256,256, /fill)
        end
      'f': begin
        print, "enter frame number:"
        read, t
        end
      else:
    endcase
    t = t mod ((size(data))[3])
    tvscl, rebin((filter+1)*sqrt(data[*,*,t]-minval), 512,512)
  endwhile
end



pro time_plot, data, z_x, z_y, r_min, r_max, result
  minval = min(data)
  xsize = (size(data))[1]
  ysize = (size(data))[2]
  tsize = (size(data))[3]
  result = ulonarr(tsize)
  filter = raster_circle(r_max, z_x,z_y, xsize,ysize, /fill)-raster_circle(r_min, z_x,z_y, xsize,ysize, /fill)
  for indx = 0, tsize-1 do begin
    tmp = max(data[*,*,indx]*filter, coords)
    coords = array_indices([xsize,ysize],coords,/dimensions)
    tfil = raster_circle(3,coords[0],coords[1],xsize,ysize,/fill)
    bgfil = raster_circle(5,coords[0],coords[1],xsize,ysize)
    bg = total(data[*,*,indx]*bgfil)/total(bgfil)
    result[indx] = total(abs((data[*,*,indx]-bg)*tfil))
    tvscl, rebin((tfil+1)*sqrt(data[*,*,indx]-minval),xsize*2,ysize*2)
  endfor
end

pro play_data, data, rot=rot, keyboard=keyboard
  end_frame = (size(data))[3]
  minval = min(data)
  frame = 0
  if keyword_set(keyboard) then begin
    c = 'y'
    print, "l/r to change frame, n to display frame number, x to stop on frame, c to play remaining frames"
    repeat begin
      tvscl, rebin(sqrt(data[*,*,frame]-minval), 512,512,/sample)
      c=get_kbrd(1,/escape)
      case c of
      'l': frame--
      'r': frame++
      'n': print,frame
      else:
      endcase
      frame = (frame + end_frame) mod end_frame
    endrep until c eq 'x' or c eq 'c'
    if c eq 'x' then return
  endif
  if keyword_set(rot) then begin
    for indx = frame, end_frame-1 do tvscl, rebin(sqrt(rotate(data[*,*,indx],7)-minval), 512,512,/sample)
  endif else for indx = frame, end_frame-1 do tvscl, rebin(sqrt(data[*,*,indx]-minval), 512,512,/sample)
end

pro fill_gaps, data, metadata, fdata, fkeo
  minval = min(data)
  fdata = data[*,*,0]
  slice=reform(data[132,*,*],256,(size(data))[3])
  fmdata=metadata[0]
  fkeo = slice[*,0]
  for i=1,(size(metadata))[1]-1 do begin
    fillwidth=round((metadata[i].exposure_start_cdf-metadata[i-1].exposure_start_cdf)/60000)-1
    if fillwidth gt 0 then begin
      frame_fill = uintarr(256,256*fillwidth)+minval
      slice_fill = uintarr(256,fillwidth)+minval
      fdata = [[fdata],[frame_fill]]
      fkeo = [[fkeo],[slice_fill]]
    endif
    fdata = [[fdata],[data[*,*,i]]]
    fkeo = [[fkeo],[slice[*,i]]]
  endfor
  fdata = reform(fdata,256,256,(size(fdata))[2]/256)
  fkeo = rotate(fkeo,3)
  if (size(fdata))[3] ne 1440 then print, "Warning: result is of unexpected size"
end

pro compile_plots, day, site, plot, px, py, r1, r2, x0, zenith=zenith, point=point
  datadir = 'c:\data\themis\imager\stream2\2011\01\'+string(day,format='(i2.2)')+'\'+site+'\'
  pattern= 'ut??\*_full-average.pgm.gz'
  filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles
  
  THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
  fill_gaps,data,metadata,fdata,keog
  region_select,fdata,keog,treg,x0,x1
  
  if keyword_set(point) then zenith_select,treg, px, py
  if keyword_set(point) then pointing, treg, px,py,r1, r2
  
  time_plot, treg, px, py, r1, r2, plot
end

hashstarts = hash()

datadir= 'c:\data\themis\imager\stream2\2011\01\26\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
fill_gaps,data,metadata,fdata,keog
region_select,fdata,keog,treg,x0,x1
time_plot, treg, 130, 82, 64, 70, tmpplot
hashplots[0126] = tmpplot
hashstarts[0126] = x0

datadir= 'c:\data\themis\imager\stream2\2011\01\01\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
fill_gaps,data,metadata,fdata,keog,fmdata
region_select,fdata,keog,treg, x0,x1
time_plot, treg, 130,82, 64, 70, tmpplot
hashplots[0101] = tmpplot
hashstarts[0101] = x0

datadir= 'c:\data\themis\imager\stream2\2011\01\03\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
fill_gaps,data,metadata,fdata,keog
treg = data[x0:x1]
time_plot, treg, 130,82, 64, 70, tmpplot
hashplots[0103] = tmpplot
hashstarts[0103] = x0

datadir= 'c:\data\themis\imager\stream2\2011\01\06\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
fill_gaps,data,metadata,fdata,keog
region_select, fdata, keog, treg, x0, x1
time_plot, treg, 130,82, 64, 70, tmpplot
hashplots[0106] = tmpplot
hashstarts[0106] = x0

datadir= 'c:\data\themis\imager\stream2\2011\01\27\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
fill_gaps,data,metadata,fdata,keog
region_select, fdata, keog, treg, x0, x1
time_plot, treg, 130,82, r1, r2, tmpplot
hashplots[0127] = tmpplot
hashstarts[0127] = x0

datadir= 'c:\data\themis\imager\stream2\2011\01\28\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
fill_gaps,data,metadata,fdata,keog
region_select, fdata, keog, treg, x0, x1
time_plot, treg, 130,82, r1, r2, tmpplot
hashplots[0128] = tmpplot
hashstarts[0128] = x0

datadir= 'c:\data\themis\imager\stream2\2011\01\29\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
fill_gaps,data,metadata,fdata,keog
region_select, fdata, keog, treg, x0, x1
time_plot, treg, 130,82, r1, r2, tmpplot
hashplots[0129] = tmpplot
hashstarts[0129] = x0

datadir= 'c:\data\themis\imager\stream2\2011\01\30\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
fill_gaps,data,metadata,fdata,keog
region_select, fdata, keog, treg, x0, x1
time_plot, treg, 130,82, r1, r2, tmpplot
hashplots[0130] = tmpplot
hashstarts[0130] = x0

datadir= 'c:\data\themis\imager\stream2\2011\01\31\gill_themis19\'  ; Gillam January 1, 2011
pattern= 'ut??\*_full-average.pgm.gz'                               ; only certain file types
filelist= FILE_SEARCH(datadir,pattern,COUNT=nfiles)  &  PRINT,nfiles   ; should be 15

THEMIS_IMAGER_READFILE,filelist,data,metadata,COUNT=nframes  &  PRINT,nframes
fill_gaps,data,metadata,fdata,keog
region_select, fdata, keog, treg, x0, x1
time_plot, treg, 130,82, r1, r2, tmpplot
hashplots[0131] = tmpplot
hashstarts[0131] = x0

hashplots8=hash()
hashstarts8=hash()
compile_plots,3,'tpas_themis08',tmpplot,px,py,r1,r2,nstart,/zenith,/point
hashplots8[0103]=tmpplot
hashstarts8[0103]=nstart

compile_plots,4,'tpas_themis08',tmpplot,px,py,r1,r2,nstart
hashplots8[0104]=tmpplot
hashstarts8[0104]=nstart

compile_plots,5,'tpas_themis08',tmpplot,px,py,r1,r2,nstart
hashplots8[0105]=tmpplot
hashstarts8[0105]=nstart

compile_plots,2,'tpas_themis08',tmpplot,px,py,r1,r2,nstart
hashplots8[0102]=tmpplot
hashstarts8[0102]=nstart

compile_plots,1,'tpas_themis08',tmpplot,px,py,r1,r2,nstart
hashplots8[0101]=tmpplot
hashstarts8[0101]=nstart

compile_plots,6,'tpas_themis08',tmpplot,px,py,r1,r2,nstart
hashplots8[0106]=tmpplot
hashstarts8[0106]=nstart

compile_plots,26,'tpas_themis08',tmpplot,px,py,r1,r2,nstart
hashplots8[0126]=tmpplot
hashstarts8[0126]=nstart

compile_plots,30,'tpas_themis08',tmpplot,px,py,r1,r2,nstart
hashplots8[0130]=tmpplot
hashstarts8[0130]=nstart

compile_plots,31,'tpas_themis08',tmpplot,px,py,r1,r2,nstart
hashplots8[0131]=tmpplot
hashstarts8[0131]=nstart

hashplots14=hash()
hashstarts14=hash()
compile_plots,1,'fykn_themis14',tmpplot,px,py,r1,r2,nstart,/zenith,/point
hashplots14[0101]=tmpplot
hashstarts14[0101]=nstart

compile_plots,3,'fykn_themis14',tmpplot,px,py,r1,r2,nstart,/zenith,/point
hashplots14[0103]=tmpplot
hashstarts14[0103]=nstart

compile_plots,4,'fykn_themis14',tmpplot,px,py,r1,r2,nstart
hashplots14[0104]=tmpplot
hashstarts14[0104]=nstart

hashplots20=hash()
hashstarts20=hash()
compile_plots,1,'gako_themis20',tmpplot,px,py,r1,r2,nstart,/zenith,/point
hashplots20[0101]=tmpplot
hashstarts20[0101]=nstart

compile_plots,3,'fykn_themis14',tmpplot,px,py,r1,r2,nstart,/zenith,/point
hashplots14[0103]=tmpplot
hashstarts14[0103]=nstart

compile_plots,4,'fykn_themis14',tmpplot,px,py,r1,r2,nstart
hashplots14[0104]=tmpplot
hashstarts14[0104]=nstart
end