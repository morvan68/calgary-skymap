function cartToSphere, coords
 x = coords(0)
 y = coords(1)
 z = coords(2)
 r = sqrt(x^2+y^2+z^2) 
 lambda = atan(y,x)
 phi = asin(z/r)
return,[r,phi,lambda]
end

function sphereToCart, coords
 r = coords(0)
 phi = coords(1)
 lambda = coords(2)
 x = r*cos(phi)*cos(lambda)
 y = r*cos(phi)*sin(lambda)
 z = r*sin(phi)
return,[x,y,z]
end

function geodToCart, coords
 a = 6378.137
 b = 6356.752
 h = coords(0)
 phi = coords(1)*!pi/180
 lambda = coords(2)*!pi/180
 N = (a^2)/sqrt((a^2)*cos(phi)^2+(b^2)*sin(phi)^2)
 x = (N+h)*cos(phi)*cos(lambda)
 y = (N+h)*cos(phi)*sin(lambda)
 z = (N*(b^2)/(a^2)+h)*sin(phi)
return,[x,y,z]
end

function cartToGeod, coords, numIterations
 if n_params() eq 1 then numIterations=2
 a = 6378.137
 b = 6356.752
 ec = 8.181919e-2
 
 x = coords(0)
 y = coords(1)
 z = coords(2)
 r = sqrt(x^2+y^2+z^2)
 p = sqrt(x^2+y^2)
 phi = asin(z/r)
 lambda = atan(y,x)
 
 N = (a^2)/sqrt((a^2)*cos(phi)^2+(b^2)*sin(phi)^2)
 h = sqrt(x^2+y^2)/cos(phi)-N
 for ind=0,numIterations do begin
  crr=1-((ec^2)*N/(N+h))
  phi=atan(z/crr,p)
  N = (a^2)/sqrt((a^2)*cos(phi)^2+(b^2)*sin(phi)^2)
  h = p/cos(phi)-N
 endfor
 return,[h,phi*180/!dpi,lambda*180/!dpi]
end

function convertTest0,numIterations,limit
  result=0l
  for indx=1,numIterations do begin
    test=randomu(indx(0),3)*20000d - 10000d
    diff=abs(geodtocart(carttogeod(test))-test)
    result=result+ (total(diff) ge limit)
  endfor
  print, result
  result=0
end

function convertTest1
  for indx=0,89 do begin
    test=randomu(indx(0),2)*[10000d,360]-[0,180]
    test=[test(0),indx,test(1)]
    result=carttogeod(geodtocart(test))
    print,test
    print,abs(result-test)
  endfor
end

function convertNR, coords, numIterations
 if n_params() eq 1 then numIterations=2
 a = 6378.137d
 b = 6356.752d
 ec = 8.181919e-2
 
 x = coords(0)
 y = coords(1)
 z = coords(2)
 r = sqrt(x^2+y^2+z^2)
 p = sqrt(x^2+y^2)
 lambda = atan(y,x)
 
 kappa0 = 1/(1-ec^2)
 kappa=kappa0
 for indx=0,numIterations do begin
  c = (((p^2)+(1-ec^2)*(z^2)*(kappa^2))^(3/2))/(a*ec^2)
  kappa=1+(p^2+(1-ec^2)*(z^2)*(kappa^3))/(c-p^2)
 endfor
 h=((1/kappa)-(1/kappa0))*sqrt(p^2+(z^2)*(kappa^2))/ec^2
 phi = atan(z*kappa,p)
return,[h,phi*180/!dpi,lambda*180/!dpi]
end