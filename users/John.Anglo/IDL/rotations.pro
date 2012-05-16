function rotate_matrix,vec,axis,angle,degrees=degrees
  if keyword_set(degrees) then angle = angle*!dpi/180
;  axis*=1d
;  length = vector_length(axis)
;  if abs(length-1d) ge 1e-16 then axis = axis/length
  a = axis[0]
  b = axis[1]
  c = axis[2]
  
  cos_angle = cos(angle)
  sin_angle = sin(angle)
  one_minus_cos = 1 - cos_angle
  
  rmatrix = dblarr(3,3) ;+ one_minus_cos
  
;  rmatrix *= [[a*a, a*b, a*c], [a*b, b*b, b*c], [a*c, b*c, c*c]]
;  rmatrix += [[cos_angle, -c*sin_angle, b* sin_angle],  $
;              [+c*sin_angle, cos_angle, -a*sin_angle],  $ 
;              [-b*sin_angle, a*sin_angle, cos_angle]]
  rmatrix[[0,1,2,3,4,5,6,7,8]] =   $
  [one_minus_cos*a*a+cos_angle,   $
   one_minus_cos*a*b-c*sin_angle, $
   one_minus_cos*a*c+b*sin_angle, $
   one_minus_cos*a*b+c*sin_angle, $
   one_minus_cos*b*b+cos_angle,   $
   one_minus_cos*b*c-a*sin_angle, $
   one_minus_cos*a*c-b*sin_angle, $
   one_minus_cos*b*c+a*sin_angle, $
   one_minus_cos*c*c+cos_angle]
   
   return,reform(rmatrix##vec,3)
;   return,rmatrix
end

function rotate_vector,vec,axis,angle,degrees=degrees
  if keyword_set(degrees) then angle = angle*!dpi/180
;  axis*=1d
;  length = vector_length(axis)
;  if abs(length-1d) ge 1e-16 then axis = axis/length
  
  vec_p = dot_product(vec,axis)*axis
  vec_o = vec-vec_p
  
  u = crossp(axis,vec)
  result = vec_p+vec_o*cos(angle)+u*sin(angle)
  return, result
end

function vector_length,vector
  return, sqrt(total(vector^2))
end

function dot_product,vec1,vec2
  return,total(vec1*vec2)
end

ve = [1d,0d,0d]
ax = [1/sqrt(3d),1/sqrt(3d),1/sqrt(3d)]
th = 60d

x=ve

st = systime(1)
for i = 0,999999L do begin
  x=rotate_vector(x,[ax[0:2]],th[0],/degrees)
endfor
st1 = systime(1)-st
print, "vector: ",st1

x=ve

st = systime(1)
for i = 0,999999L do begin
  x=rotate_matrix(x,ax[0:2],th[0],/degrees)
endfor
st2 = systime(1)-st
print, "matrix: ",st2

print, (st2-st1)/st2
print, (st2-st1)/st1

x=ve

st = systime(1)
for i = 0,999999L do begin
  x=rotate_matrix(x,[ax[0:2]],th[0],/degrees)
endfor
st1 = systime(1)-st
print, "vector: ",st1

x=ve

st = systime(1)
for i = 0,999999L do begin
  x=rotate_vector(x,ax[0:2],th[0],/degrees)
endfor
st2 = systime(1)-st
print, "matrix: ",st2

print, (st2-st1)/st2
print, (st2-st1)/st1
end