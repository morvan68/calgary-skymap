function euler_rotate, alpha, beta, gamma, degrees=degrees
  if keyword_set(degrees) then begin
    alpha *= !dpi/180
    beta *= !dpi/180
    gamma *= !dpi/180
  endif
  cos_alpha = cos(alpha) & sin_alpha = sin(alpha) ;alpha<->phi
  cos_beta = cos(beta) & sin_beta = sin(beta)     ;beta<->theta
  cos_gamma = cos(gamma) & sin_gamma = sin(gamma) ;gamma<->psi
  
  rmatrix = dblarr(3,3)
  rmatrix[[0,1,2,3,4,5,6,7,8]] = $
            [cos_gamma*cos_alpha-cos_beta*sin_alpha*sin_gamma,  $
             cos_gamma*sin_alpha+cos_beta*cos_alpha*sin_gamma,  $ 
             sin_gamma*sin_beta,                                $
            -sin_gamma*cos_alpha-cos_beta*sin_alpha*cos_gamma,  $
            -sin_gamma*sin_alpha+cos_beta*cos_alpha*cos_gamma,  $
             cos_gamma*sin_beta,                                $
             sin_beta*sin_alpha,                                $
            -sin_beta*cos_alpha,                                $
             cos_beta]
   return,rmatrix
end

pro cos_test
  for i=0,360 do begin
    a = i*!dpi/180
    st = systime(1)
    for ind=0,99999L do a = cos(a)
    print,i,systime(1)-st
  endfor
end

pro sin_test
  for i=0,360 do begin
    a = i*!dpi/180
    st = systime(1)
    for ind=0,99999L do a = sin(a)
    print,i,systime(1)-st
  endfor
end

vec = skymap_vector([1,2,3])

st=systime(1)
for i=0,999999L do x=vec.euler_matrix(120,120,120,/degrees)
print,systime(1)-st

st=systime(1)
for i=0,999999L do x=euler_rotate(120,120,120,/degrees)
print,systime(1)-st

end