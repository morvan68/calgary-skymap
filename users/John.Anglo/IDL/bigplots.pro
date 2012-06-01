pro bigplots, seconds, counts,ind
  init=(*seconds[0])[430,0]
  
  for indx=0,ind-1 do begin
    t1=init+indx*86400
    cyc=0
    t2=(*seconds[indx])[cyc,0]
    while t2 lt t1 do begin
      cyc++
      t2=(*seconds[indx])[cyc,0]
    endwhile
    wait,1
    shade_surf,(*counts[indx])[cyc:cyc+120,120:-200],ax=90,az=0
  endfor
end

ind=7

cycles=intarr(7)
countsp=ptrarr(7,/allocate_heap)
mirrorp=ptrarr(7,/allocate_heap)
secondp=ptrarr(7,/allocate_heap)
microsp=ptrarr(7,/allocate_heap)

;datadir= 'c:/data/norstar/msp/2011/02/01/'
;filelist= FILE_SEARCH(datadir,'*_FSMI_iv[23].dat',COUNT=nfiles)

;data= NORSTAR_MSP_READFILE(filelist[0])
;for i=1,((size(filelist))[1]-1) do data=[data,(NORSTAR_MSP_READFILE(filelist[i]))]
;i=0
;for i=0,((size(data))[1]-1) do if ((data[i].mirror_step ne (i mod 564)) && (i mod 564 lt 544 || data[i].mirror_step ne 544)) then break
;if i ne ((size(data))[1]) then print,i
;cycles=((size(data))[1]/564)
;counts=rotate(reform([data.counts[1],uintarr(20)],564,cycles+1),3)
;help,counts
;mirror=rotate(reform([data.mirror_step,uintarr(20)+544],564,cycles+1),3)
 
for indx=0,ind-1 do begin
 datadir= 'c:/data/norstar/msp/2011/02/'+string((indx+1),format='(i2.2)')+'/'
 print,datadir
 filelist= FILE_SEARCH(datadir,'*_FSMI_iv[23].dat',COUNT=nfiles)

 temp= NORSTAR_MSP_READFILE(filelist[0])
 for i=1,((size(filelist))[1]-1) do temp=[temp,(NORSTAR_MSP_READFILE(filelist[i]))]
 i=0
 for i=0,((size(temp))[1]-1) do if ((temp[i].mirror_step ne (i mod 564)) && (i mod 564 lt 544 || temp[i].mirror_step ne 544)) then break
 if i ne ((size(temp))[1]) then print,i
 cycles[indx]=((size(temp))[1]/564)
 ;counts=rotate(reform([temp.counts[1],uintarr(20)],564,cycles[indx]+1),3)
 ;mirror=rotate(reform([temp.mirror_step,uintarr(20)+544],564,cycles[indx]+1),3)
 counts=rotate(reform(temp[0:(cycles[indx]*564l-1)].counts[1],564,cycles[indx]),3)
 mirror=rotate(reform(temp[0:(cycles[indx]*564l-1)].mirror_step,564,cycles[indx]),3)
 second=rotate(reform(temp[0:(cycles[indx]*564l-1)].seconds,564,cycles[indx]),3)
 micros=rotate(reform(temp[0:(cycles[indx]*564l-1)].microseconds,564,cycles[indx]),3)
 countsp[indx]=ptr_new(counts)
 mirrorp[indx]=ptr_new(mirror)
 secondp[indx]=ptr_new(second)
 microsp[indx]=ptr_new(micros)
 print,cycles[indx]
endfor

end

im1=image((*countsp[0])[430:550,120:-300])
q=poly_fit(indgen(80),(*countsp[0])[461:540,170],8)
plot,indgen(80),(*countsp[0])[461:540,170]
oplot,q[0]+x*q[1]+x^2*q[2]+x^3*q[3]+x^4*q[4]+x^5*q[5]+x^6*q[6]+x^7*q[7]+x^8*q[8]
;for indx=0,ind-1 do begin
;  shade_surf,(*countsp[indx])[*,20:*]<max(counts)/100,ax=90,az=0,xrange=[cycles[indx],0]
;  wait,4
;endfor
  
  ;shade_surf,counts[*,20:*]<max(counts)/100,ax=90,az=0,xrange=[cycles,0]
  ;shade_surf,counts[*,20:*]<max(counts)/200,xrange=[cycles,0]
  ;print,cycles
  ;tvscl,counts<max(counts)/100
  ;im1=image(data[*,20:*])