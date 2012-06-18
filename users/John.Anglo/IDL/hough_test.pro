


function raster_circle, radius, a, b, frame_x, frame_y, value, fill=fill
  if ~keyword_set(value) then value = 1
  x0=a+frame_x
  y0=b+frame_y
  frame=ulonarr(frame_x*3, frame_y*3)
  
  f = 1 - radius
  ddf_x = 1
  ddf_y = -2 * radius
  
  x = 0
  y = radius
  if ~keyword_set(fill) then begin
    frame(x0+radius,y0)=value
    frame(x0-radius,y0)=value
    frame(x0,y0+radius)=value
    frame(x0,y0-radius)=value
  
    while x lt y do begin
      if f ge 0 then begin y-- & ddf_y += 2 & f += ddf_y & endif
      x++
      ddf_x += 2
      f += ddf_x
    
      frame(x0+x,y0+y)=value
      frame(x0-x,y0+y)=value
      frame(x0+x,y0-y)=value
      frame(x0-x,y0-y)=value
      frame(x0+y,y0+x)=value
      frame(x0-y,y0+x)=value
      frame(x0+y,y0-x)=value
      frame(x0-y,y0-x)=value
      endwhile
  endif else begin
    frame(x0-radius:x0+radius,y0)=value
    frame(x0,y0+radius)=value
    frame(x0,y0-radius)=value
  
    while x lt y do begin
      if f ge 0 then begin y-- & ddf_y += 2 & f += ddf_y & endif
      x++
      ddf_x += 2
      f += ddf_x
    
      frame(x0-x:x0+x,y0+y)=value
      frame(x0-x:x0+x,y0-y)=value
      frame(x0-y:x0+y,y0+x)=value
      frame(x0-y:x0+y,y0-x)=value
      endwhile
  endelse
  return, frame[frame_x:2*frame_x-1,frame_y:2*frame_y-1]
end

function transform, data, min_radius,max_radius, threshold
  if ~keyword_set(threshold) then threshold=0
  size_x = (size(data))[1]
  size_y = (size(data))[2]
  accumulator = ulonarr(max_radius,size_x,size_y)
  
  for i_x = 0, size_x-1 do begin
    for i_y = 0, size_y-1 do begin
      temp = data[i_x,i_y]
      if temp gt threshold then for i_r = min_radius, max_radius-1 do accumulator[i_r,*,*] = accumulator[i_r,*,*] + raster_circle(i_r, i_x, i_y, size_x, size_y,temp)
    endfor
  endfor
  
  return, accumulator
end

function transform2, data, x0, y0, min_radius, max_radius, threshold
  if ~keyword_set(threshold) then threshold=0
  size_x = (size(data))[1]
  size_y = (size(data))[2]
  accumulator = ulon64arr(size_x, size_y)
  
  for i_r = min_radius,max_radius do begin
    temp = raster_circle(i_r, x0, y0, size_x, size_y)
    for i_x = 0, size_x-1 do for i_y = 0, size_y-1 do if temp[i_x,i_y] gt threshold then accumulator += data[i_x,i_y]*temp
  endfor
  return,accumulator
end

function rad_rotate, data, x0, y0, frame_x, frame_y
  img = ulon64arr(frame_x, frame_y)
  for i_r = 0, (size(data))[1]-1 do if data[i_r] ne 0 then img += raster_circle(x0, y0, frame_x, frame_y, data[i_r])
  return, img
end

var=variance(data[*,*,60:299]-minval,dimension=3)
ave=total((data[*,*,60:299]-minval)/240,3)
shade_surf,var<(max(var)/800),ax=90,az=0
var = var<(max(var)/800)>min(var)
ave = ave<(max(ave)/800)>min(ave)
test_data2 = var*ave
test_acc2=transform2(test_data2, 132,82, 20,140)
end