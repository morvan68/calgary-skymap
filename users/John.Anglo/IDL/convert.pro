function toSphere, coords
 x = coords(0)
 y = coords(1)
 z = coords(2)
 r = sqrt(x^2+y^2+z^2) 
 phi = atan(y,x)
 theta = acos(z/r)
return,[r,theta,phi]
end

function toCart, coords
 r = coords(0)
 theta = coords(1)
 phi = coords(2)
 x = r*sin(theta)*cos(phi)
 y = r*sin(theta)*sin(phi)
 z = r*cos(theta)
return,[x,y,z]
end

pro convert
  coords0 = systime(1)
  for indx=0,99999L do begin
    coords0 = randomu(coords0(0),3)*2-1d
    coords1 = tocart(tosphere(coords0))
    if total(abs(coords0-coords1)) ge 1e-14 then begin
      print,coords0
      print,coords1
      print,indx
      break
    endif
  endfor
  
  coords0 = systime(1)
  for indx=0,99999L do begin
    coords0 = randomu(coords0(0),3)*[1d,!dpi,2*!dpi]-[0,0,!dpi]
    coords1 = tosphere(tocart(coords0))
    if total(abs(coords0-coords1)) ge 1e-14 then begin
      print,coords0
      print,coords1
      print,indx
      break
    endif
  endfor
end