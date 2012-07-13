pro star_data_to_file, site, location, object, pole, zenith, ratio, days = days, refine = refine, star_name = star_name, visual = visual, runtime = runtime
  stime = systime(1)
  if ~keyword_set(days) then days = indgen(31)+1
  if ~keyword_set(star_name) then star_name = ''
  print, days[0]
  data_init, data, metadata, days[0], site
  tsize = (size(data))[3]
  rdata = ulonarr(256,256,tsize)
  for i = 0, tsize-1 do rdata[*,*,i] = rotate(data[*,*,i],7)
  
  
  if keyword_set(refine) then manual_refine_params, rdata, metadata, location, pole, zenith, ratio
  print, pole, zenith, ratio
  print, object
  
  foreach element, days, key do begin
    z_angle = dblarr(tsize)
    result=ulonarr(tsize)
    for i = 0,tsize-1 do begin
    tmp2 = 0
      get_count, rdata, metadata, i, object, location, pole,zenith, ratio, tmp, z_angle=tmp2, visual = visual
      z_angle[i] = tmp2
      result[i] = tmp
    endfor

    openw, lun1, site+'_'+star_name+'_01'+string(element,format='(i2.2)')+'_counts.dat',/get_lun
    openw, lun2, site+'_'+star_name+'_01'+string(element,format='(i2.2)')+'_angle.dat',/get_lun
    printf, lun1, tsize
    printf, lun2, tsize
    printf, lun1, result
    printf, lun2, z_angle
    free_lun,lun1
    free_lun,lun2
    if keyword_set(runtime) then begin
      st1 = systime(1) 
      print, st1-stime
      stime = st1
    endif
    
    if key+1 eq (size(days))[1] then break
    print,days[key+1]
    data_init, data, metadata, days[key+1], site
    tsize = (size(data))[3]
    rdata = ulonarr(256,256,tsize)
    for i = 0, tsize-1 do rdata[*,*,i] = rotate(data[*,*,i],7)
  endforeach
end

pro star_readfile, site, day, counts, angles, star = star
  if ~keyword_set(star) then begin &star = ''
  endif else star = '_'+star
  openr, lun1, site+star+'_01'+string(day,format='(i2.2)')+'_counts.dat',/get_lun
  openr, lun2, site+star+'_01'+string(day,format='(i2.2)')+'_angle.dat',/get_lun
  readf, lun1, csize
  readf, lun2, asize
  if csize ne asize then print, 'Warning: files are not of matching size'
  counts = ulonarr(csize)
  angles = dblarr(asize)
  readf, lun1, counts
  readf, lun2, angles
end

pro tmp_data_compile ;delete once finished
  star_data_to_file, 'whit_themis07', [60.71, 224.95], [114.99,5.2], -15, [131, 124], 83,star_name = 'procyon',days = (indgen(31)+1)[28:*]
  star_data_to_file, 'whit_themis07', [60.71, 224.95], [88.96,7.408], -15, [131, 124], 83,star_name = 'betelgeuse', days = (indgen(31)+1)[28:*]
  
  star_data_to_file, 'snap_themis04', [62.52, 249.11], [114.99,5.2], -12, [98,105], 81.5,star_name = 'procyon'
  star_data_to_file, 'snap_themis04', [62.52, 249.11], [88.96,7.408], -12, [98,105], 81.5,star_name = 'betelgeuse'
  
  star_data_to_file, 'snap_themis04', [62.52, 249.11], [279.34,38.79], -12, [98,105], 81.5,star_name = 'vega'
  star_data_to_file, 'atha_themis02', [54.7191, 246.72], [279.34,38.79], -15, [126,129], 82.5,star_name = 'vega'
  star_data_to_file, 'talo_themis15', [69.537, 267.47], [279.34,38.79], 50, [126,126], 84, days = (indgen(31)+1)[1:*],star_name = 'vega'
  star_data_to_file, 'fsim_themis04', [61.86, 238.64], [279.34,38.79], -4, [120,120], 84, days = (indgen(31)+1)[1:*],star_name = 'vega'

  star_data_to_file, 'nrsq_themis01', [61.1, 314.56], [279.34,38.79], 32, [127,129], 83.5, days = (indgen(31)+1)[1:*],star_name = 'vega'
  star_data_to_file, 'pina_themis18', [50.16, 263.93], [279.34,38.79], 1, [124, 127], 82.5, days = (indgen(7)+1)[1:*],star_name = 'vega'
  star_data_to_file, 'pina_themis18', [50.16, 263.93], [279.34,38.79], 1, [124, 127], 82.5, days = (indgen(31)+1)[14:*],star_name = 'vega'
  star_data_to_file, 'rank_themis12', [62.82,267.89], [279.34,38.79], 15.7, [136, 119], 82, days = (indgen(31)+1)[1:*],star_name = 'vega'
  star_data_to_file, 'fykn_themis14', [66.56, 214.79], [279.34,38.79], -27.33, [128,128], 81.5, days = (indgen(31)+1)[2:*],star_name = 'vega'

  star_data_to_file, 'chbg_themis16', [49.8,285.58], [279.34,38.79], 11, [124,122], 82.5, days = (indgen(6)+1)[2:*],star_name = 'vega'
  star_data_to_file, 'chbg_themis16', [49.8,285.58], [279.34,38.79], 11, [124,122], 82.5, days = (indgen(31)+1)[20:*],star_name = 'vega'
  star_data_to_file, 'fsmi_themis10', [59.98, 248.16], [279.34,38.79], -22, [129, 121], 83.5, days = (indgen(31)+1)[2:*],star_name = 'vega'
  star_data_to_file, 'inuv_themis17', [68.3, 226.52], [279.34,38.79], -33.6, [127,126], 81.5, days = (indgen(31)+1)[2:*],star_name = 'vega'
  star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [279.34,38.79], -13.6, [127,129], 81.5, days = (indgen(9)+1)[3:*],star_name = 'vega'
  star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [279.34,38.79], -13.6, [127,129], 81.5, days = (indgen(31)+1)[10:23],star_name = 'vega'
  star_data_to_file, 'kapu_themis21', [49.39, 277.68], [279.34,38.79], 11, [126,129], 83.5, days = (indgen(31)+1)[3:*],star_name = 'vega'
  
  star_data_to_file, 'gill_themis19', [56.3,265.3], [214.06,19.124], -2, [130,124], 82.5, days = [3,1,6,28,30,31],star_name = 'arcturus'
  star_data_to_file, 'tpas_themis08', [53.82, 258.75], [214.06,19.124], -1, [127,124], 83.5, days = [1,2,3,4,5,30,31],star_name = 'arcturus'
  star_data_to_file, 'gako_themis20', [62.3,214.67], [214.06,19.124], -22, [125, 128], 82.5,star_name = 'arcturus'
  star_data_to_file, 'whit_themis07', [60.71, 224.95], [214.06,19.124], -15, [131, 124], 83,star_name = 'arcturus', days = (indgen(27)+1)
  star_data_to_file, 'whit_themis07', [60.71, 224.95], [214.06,19.124], -15, [131, 124], 83,star_name = 'arcturus', days = (indgen(31)+1)[28:*]
  
  star_data_to_file, 'snap_themis04', [62.52, 249.11], [214.06,19.124], -12, [98,105], 81.5,star_name = 'arcturus'
  star_data_to_file, 'atha_themis02', [54.7191, 246.72], [214.06,19.124], -15, [126,129], 82.5,star_name = 'arcturus'
  star_data_to_file, 'talo_themis15', [69.537, 267.47], [214.06,19.124], 50, [126,126], 84, days = (indgen(31)+1)[1:*],star_name = 'arcturus'
  star_data_to_file, 'fsim_themis04', [61.86, 238.64], [214.06,19.124], -4, [120,120], 84, days = (indgen(31)+1)[1:*],star_name = 'arcturus'
  
  star_data_to_file, 'nrsq_themis01', [61.1, 314.56], [214.06,19.124], 32, [127,129], 83.5, days = (indgen(31)+1)[1:*],star_name = 'arcturus'
  star_data_to_file, 'pina_themis18', [50.16, 263.93], [214.06,19.124], 1, [124, 127], 82.5, days = (indgen(7)+1)[1:*],star_name = 'arcturus'
  star_data_to_file, 'pina_themis18', [50.16, 263.93], [214.06,19.124], 1, [124, 127], 82.5, days = (indgen(31)+1)[14:*],star_name = 'arcturus'
  star_data_to_file, 'rank_themis12', [62.82,267.89], [214.06,19.124], 15.7, [136, 119], 82, days = (indgen(31)+1)[1:*],star_name = 'arcturus'
  star_data_to_file, 'fykn_themis14', [66.56, 214.79], [214.06,19.124], -27.33, [128,128], 81.5, days = (indgen(31)+1)[2:*],star_name = 'arcturus'

  star_data_to_file, 'chbg_themis16', [49.8,285.58], [214.06,19.124], 11, [124,122], 82.5, days = (indgen(6)+1)[2:*],star_name = 'arcturus'
  star_data_to_file, 'chbg_themis16', [49.8,285.58], [214.06,19.124], 11, [124,122], 82.5, days = (indgen(31)+1)[20:*],star_name = 'arcturus'
  star_data_to_file, 'fsmi_themis10', [59.98, 248.16], [214.06,19.124], -22, [129, 121], 83.5, days = (indgen(31)+1)[2:*],star_name = 'arcturus'
  star_data_to_file, 'inuv_themis17', [68.3, 226.52], [214.06,19.124], -33.6, [127,126], 81.5, days = (indgen(31)+1)[2:*],star_name = 'arcturus'
  star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [214.06,19.124], -13.6, [127,129], 81.5, days = (indgen(9)+1)[3:*],star_name = 'arcturus'
  star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [214.06,19.124], -13.6, [127,129], 81.5, days = (indgen(31)+1)[10:23],star_name = 'arcturus'
  star_data_to_file, 'kapu_themis21', [49.39, 277.68], [214.06,19.124], 11, [126,129], 83.5, days = (indgen(31)+1)[3:*],star_name = 'arcturus'
end

star_data_to_file, 'gill_themis19', [56.3,265.3], [79.35, 46.01], -2, [130,124], 82.5, days = [3,1,6,28,30,31]
star_data_to_file, 'gill_themis19', [56.3,265.3], [79.35, 46.01], -2, [130,124], 82.5, days = 2
star_data_to_file, 'tpas_themis08', [53.82, 258.75], [79.35, 46.01], -1, [127,124], 83.5, days = [1,2,3,4,5,30,31]
star_data_to_file, 'gako_themis20', [62.3,214.67], [79.35, 46.01], -22, [125, 128], 82.5
star_data_to_file, 'whit_themis07', [60.71, 224.95], [79.35, 46.01], -15, [131, 124], 83

star_data_to_file, 'snap_themis04', [62.52, 249.11], [79.35, 46.01], -12, [98,105], 81.5, days = 29
star_data_to_file, 'atha_themis02', [54.7191, 246.72], [79.35, 46.01], -15, [126,129], 82.5
star_data_to_file, 'talo_themis15', [69.537, 267.47], [79.35, 46.01], 50, [126,126], 84, days = (indgen(31)+1)[1:*]
star_data_to_file, 'fsim_themis04', [61.86, 238.64], [79.35, 46.01], -4, [120,120], 84, days = (indgen(31)+1)[1:*]

star_data_to_file, 'nrsq_themis01', [61.1, 314.56], [79.35, 46.01], 32, [127,129], 83.5, days = (indgen(31)+1)[1:*]
star_data_to_file, 'pina_themis18', [50.16, 263.93], [79.35, 46.01], 1, [124, 127], 82.5, days = (indgen(7)+1)[1:*]
star_data_to_file, 'pina_themis18', [50.16, 263.93], [79.35, 46.01], 1, [124, 127], 82.5, days = (indgen(31)+1)[14:*]
star_data_to_file, 'rank_themis12', [62.82,267.89], [79.35, 46.01], 15.7, [136, 119], 82, days = (indgen(31)+1)[1:*]
star_data_to_file, 'fykn_themis14', [66.56, 214.79], [79.35, 46.01], -27.33, [128,128], 81.5, days = (indgen(31)+1)[2:*]

star_data_to_file, 'chbg_themis16', [49.8,285.58], [79.35, 46.01], 11, [124,122], 82.5, days = (indgen(6)+1)[2:*]
star_data_to_file, 'chbg_themis16', [49.8,285.58], [79.35, 46.01], 11, [124,122], 82.5, days = (indgen(31)+1)[20:*]
star_data_to_file, 'fsmi_themis10', [59.98, 248.16], [79.35, 46.01], -22, [129, 121], 83.5, days = (indgen(31)+1)[2:*]
star_data_to_file, 'inuv_themis17', [68.3, 226.52], [79.35, 46.01], -33.6, [127,126], 81.5, days = (indgen(31)+1)[2:*]
star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [79.35, 46.01], -13.6, [127,129], 81.5, days = (indgen(9)+1)[3:*]
star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [79.35, 46.01], -13.6, [127,129], 81.5, days = (indgen(31)+1)[10:23]
star_data_to_file, 'kapu_themis21', [49.39, 277.68], [79.35, 46.01], 11, [126,129], 83.5, days = (indgen(31)+1)[3:*]

star_data_to_file, 'gill_themis19', [56.3,265.3], [114.99,5.2], -2, [130,124], 82.5, days = [3,1,6,28,30,31],star_name = 'procyon'
star_data_to_file, 'tpas_themis08', [53.82, 258.75], [114.99,5.2], -1, [127,124], 83.5, days = [1,2,3,4,5,30,31],star_name = 'procyon'
star_data_to_file, 'gako_themis20', [62.3,214.67], [114.99,5.2], -22, [125, 128], 82.5,star_name = 'procyon'
star_data_to_file, 'whit_themis07', [60.71, 224.95], [114.99,5.2], -15, [131, 124], 83,star_name = 'procyon',days = (indgen(27)+1)
star_data_to_file, 'whit_themis07', [60.71, 224.95], [114.99,5.2], -15, [131, 124], 83,star_name = 'procyon',days = (indgen(31)+1)[28:*]

star_data_to_file, 'snap_themis04', [62.52, 249.11], [114.99,5.2], -12, [98,105], 81.5,star_name = 'procyon'
star_data_to_file, 'atha_themis02', [54.7191, 246.72], [114.99,5.2], -15, [126,129], 82.5,star_name = 'procyon'
star_data_to_file, 'talo_themis15', [69.537, 267.47], [114.99,5.2], 50, [126,126], 84, days = (indgen(31)+1)[1:*],star_name = 'procyon'
star_data_to_file, 'fsim_themis04', [61.86, 238.64], [114.99,5.2], -4, [120,120], 84, days = (indgen(31)+1)[1:*],star_name = 'procyon'

star_data_to_file, 'nrsq_themis01', [61.1, 314.56], [114.99,5.2], 32, [127,129], 83.5, days = (indgen(31)+1)[1:*],star_name = 'procyon'
star_data_to_file, 'pina_themis18', [50.16, 263.93], [114.99,5.2], 1, [124, 127], 82.5, days = (indgen(7)+1)[1:*],star_name = 'procyon'
star_data_to_file, 'pina_themis18', [50.16, 263.93], [114.99,5.2], 1, [124, 127], 82.5, days = (indgen(31)+1)[14:*],star_name = 'procyon'
star_data_to_file, 'rank_themis12', [62.82,267.89], [114.99,5.2], 15.7, [136, 119], 82, days = (indgen(31)+1)[1:*],star_name = 'procyon'
star_data_to_file, 'fykn_themis14', [66.56, 214.79], [114.99,5.2], -27.33, [128,128], 81.5, days = (indgen(31)+1)[2:*],star_name = 'procyon'

star_data_to_file, 'chbg_themis16', [49.8,285.58], [114.99,5.2], 11, [124,122], 82.5, days = (indgen(6)+1)[2:*],star_name = 'procyon'
star_data_to_file, 'chbg_themis16', [49.8,285.58], [114.99,5.2], 11, [124,122], 82.5, days = (indgen(31)+1)[20:*],star_name = 'procyon'
star_data_to_file, 'fsmi_themis10', [59.98, 248.16], [114.99,5.2], -22, [129, 121], 83.5, days = (indgen(31)+1)[2:*],star_name = 'procyon'
star_data_to_file, 'inuv_themis17', [68.3, 226.52], [114.99,5.2], -33.6, [127,126], 81.5, days = (indgen(31)+1)[2:*],star_name = 'procyon'
star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [114.99,5.2], -13.6, [127,129], 81.5, days = (indgen(9)+1)[3:*],star_name = 'procyon'
star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [114.99,5.2], -13.6, [127,129], 81.5, days = (indgen(31)+1)[10:23],star_name = 'procyon'
star_data_to_file, 'kapu_themis21', [49.39, 277.68], [114.99,5.2], 11, [126,129], 83.5, days = (indgen(31)+1)[3:*],star_name = 'procyon'

star_data_to_file, 'gill_themis19', [56.3,265.3], [88.96,7.408], -2, [130,124], 82.5, days = [3,1,6,28,30,31],star_name = 'betelgeuse'
star_data_to_file, 'tpas_themis08', [53.82, 258.75], [88.96,7.408], -1, [127,124], 83.5, days = [1,2,3,4,5,30,31],star_name = 'betelgeuse'
star_data_to_file, 'gako_themis20', [62.3,214.67], [88.96,7.408], -22, [125, 128], 82.5,star_name = 'betelgeuse'
star_data_to_file, 'whit_themis07', [60.71, 224.95], [88.96,7.408], -15, [131, 124], 83,star_name = 'betelgeuse', days = (indgen(27)+1)
star_data_to_file, 'whit_themis07', [60.71, 224.95], [88.96,7.408], -15, [131, 124], 83,star_name = 'betelgeuse', days = (indgen(31)+1)[28:*]

star_data_to_file, 'snap_themis04', [62.52, 249.11], [88.96,7.408], -12, [98,105], 81.5,star_name = 'betelgeuse'
star_data_to_file, 'atha_themis02', [54.7191, 246.72], [88.96,7.408], -15, [126,129], 82.5,star_name = 'betelgeuse'
star_data_to_file, 'talo_themis15', [69.537, 267.47], [88.96,7.408], 50, [126,126], 84, days = (indgen(31)+1)[1:*],star_name = 'betelgeuse'
star_data_to_file, 'fsim_themis04', [61.86, 238.64], [88.96,7.408], -4, [120,120], 84, days = (indgen(31)+1)[1:*],star_name = 'betelgeuse'

star_data_to_file, 'nrsq_themis01', [61.1, 314.56], [88.96,7.408], 32, [127,129], 83.5, days = (indgen(31)+1)[1:*],star_name = 'betelgeuse'
star_data_to_file, 'pina_themis18', [50.16, 263.93], [88.96,7.408], 1, [124, 127], 82.5, days = (indgen(7)+1)[1:*],star_name = 'betelgeuse'
star_data_to_file, 'pina_themis18', [50.16, 263.93], [88.96,7.408], 1, [124, 127], 82.5, days = (indgen(31)+1)[14:*],star_name = 'betelgeuse'
star_data_to_file, 'rank_themis12', [62.82,267.89], [88.96,7.408], 15.7, [136, 119], 82, days = (indgen(31)+1)[1:*],star_name = 'betelgeuse'
star_data_to_file, 'fykn_themis14', [66.56, 214.79], [88.96,7.408], -27.33, [128,128], 81.5, days = (indgen(31)+1)[2:*],star_name = 'betelgeuse'

star_data_to_file, 'chbg_themis16', [49.8,285.58], [88.96,7.408], 11, [124,122], 82.5, days = (indgen(6)+1)[2:*],star_name = 'betelgeuse'
star_data_to_file, 'chbg_themis16', [49.8,285.58], [88.96,7.408], 11, [124,122], 82.5, days = (indgen(31)+1)[20:*],star_name = 'betelgeuse'
star_data_to_file, 'fsmi_themis10', [59.98, 248.16], [88.96,7.408], -22, [129, 121], 83.5, days = (indgen(31)+1)[2:*],star_name = 'betelgeuse'
star_data_to_file, 'inuv_themis17', [68.3, 226.52], [88.96,7.408], -33.6, [127,126], 81.5, days = (indgen(31)+1)[2:*],star_name = 'betelgeuse'
star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [88.96,7.408], -13.6, [127,129], 81.5, days = (indgen(9)+1)[3:*],star_name = 'betelgeuse'
star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [88.96,7.408], -13.6, [127,129], 81.5, days = (indgen(31)+1)[10:23],star_name = 'betelgeuse'
star_data_to_file, 'kapu_themis21', [49.39, 277.68], [88.96,7.408], 11, [126,129], 83.5, days = (indgen(31)+1)[3:*],star_name = 'betelgeuse'

star_data_to_file, 'gill_themis19', [56.3,265.3], [279.34,38.79], -2, [130,124], 82.5, days = [3,1,6,28,30,31],star_name = 'vega'
star_data_to_file, 'tpas_themis08', [53.82, 258.75], [279.34,38.79], -1, [127,124], 83.5, days = [1,2,3,4,5,30,31],star_name = 'vega'
star_data_to_file, 'gako_themis20', [62.3,214.67], [279.34,38.79], -22, [125, 128], 82.5,star_name = 'vega'
star_data_to_file, 'whit_themis07', [60.71, 224.95], [279.34,38.79], -15, [131, 124], 83,star_name = 'vega', days = (indgen(31)+1)[28:*]
star_data_to_file, 'whit_themis07', [60.71, 224.95], [279.34,38.79], -15, [131, 124], 83,star_name = 'vega', days = (indgen(27)+1)

star_data_to_file, 'snap_themis04', [62.52, 249.11], [279.34,38.79], -12, [98,105], 81.5,star_name = 'vega'
star_data_to_file, 'atha_themis02', [54.7191, 246.72], [279.34,38.79], -15, [126,129], 82.5,star_name = 'vega'
star_data_to_file, 'talo_themis15', [69.537, 267.47], [279.34,38.79], 50, [126,126], 84, days = (indgen(31)+1)[1:*],star_name = 'vega'
star_data_to_file, 'fsim_themis04', [61.86, 238.64], [279.34,38.79], -4, [120,120], 84, days = (indgen(31)+1)[1:*],star_name = 'vega'

star_data_to_file, 'nrsq_themis01', [61.1, 314.56], [279.34,38.79], 32, [127,129], 83.5, days = (indgen(31)+1)[1:*],star_name = 'vega'
star_data_to_file, 'pina_themis18', [50.16, 263.93], [279.34,38.79], 1, [124, 127], 82.5, days = (indgen(7)+1)[1:*],star_name = 'vega'
star_data_to_file, 'pina_themis18', [50.16, 263.93], [279.34,38.79], 1, [124, 127], 82.5, days = (indgen(31)+1)[14:*],star_name = 'vega'
star_data_to_file, 'rank_themis12', [62.82,267.89], [279.34,38.79], 15.7, [136, 119], 82, days = (indgen(31)+1)[1:*],star_name = 'vega'
star_data_to_file, 'fykn_themis14', [66.56, 214.79], [279.34,38.79], -27.33, [128,128], 81.5, days = (indgen(31)+1)[2:*],star_name = 'vega'

star_data_to_file, 'chbg_themis16', [49.8,285.58], [279.34,38.79], 11, [124,122], 82.5, days = (indgen(6)+1)[2:*],star_name = 'vega'
star_data_to_file, 'chbg_themis16', [49.8,285.58], [279.34,38.79], 11, [124,122], 82.5, days = (indgen(31)+1)[20:*],star_name = 'vega'
star_data_to_file, 'fsmi_themis10', [59.98, 248.16], [279.34,38.79], -22, [129, 121], 83.5, days = (indgen(31)+1)[2:*],star_name = 'vega'
star_data_to_file, 'inuv_themis17', [68.3, 226.52], [279.34,38.79], -33.6, [127,126], 81.5, days = (indgen(31)+1)[2:*],star_name = 'vega'
star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [279.34,38.79], -13.6, [127,129], 81.5, days = (indgen(9)+1)[3:*],star_name = 'vega'
star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [279.34,38.79], -13.6, [127,129], 81.5, days = (indgen(31)+1)[10:23],star_name = 'vega'
star_data_to_file, 'kapu_themis21', [49.39, 277.68], [279.34,38.79], 11, [126,129], 83.5, days = (indgen(31)+1)[3:*],star_name = 'vega'

star_data_to_file, 'gill_themis19', [56.3,265.3], [214.06,19.124], -2, [130,124], 82.5, days = [3,1,6,28,30,31],star_name = 'arcturus'
star_data_to_file, 'tpas_themis08', [53.82, 258.75], [214.06,19.124], -1, [127,124], 83.5, days = [1,2,3,4,5,30,31],star_name = 'arcturus'
star_data_to_file, 'gako_themis20', [62.3,214.67], [214.06,19.124], -22, [125, 128], 82.5,star_name = 'arcturus'
star_data_to_file, 'whit_themis07', [60.71, 224.95], [214.06,19.124], -15, [131, 124], 83,star_name = 'arcturus', days = (indgen(27)+1)
star_data_to_file, 'whit_themis07', [60.71, 224.95], [214.06,19.124], -15, [131, 124], 83,star_name = 'arcturus', days = (indgen(31)+1)[28:*]

star_data_to_file, 'snap_themis04', [62.52, 249.11], [214.06,19.124], -12, [98,105], 81.5,star_name = 'arcturus'
star_data_to_file, 'atha_themis02', [54.7191, 246.72], [214.06,19.124], -15, [126,129], 82.5,star_name = 'arcturus'
star_data_to_file, 'talo_themis15', [69.537, 267.47], [214.06,19.124], 50, [126,126], 84, days = (indgen(31)+1)[1:*],star_name = 'arcturus'
star_data_to_file, 'fsim_themis04', [61.86, 238.64], [214.06,19.124], -4, [120,120], 84, days = (indgen(31)+1)[1:*],star_name = 'arcturus'

star_data_to_file, 'nrsq_themis01', [61.1, 314.56], [214.06,19.124], 32, [127,129], 83.5, days = (indgen(31)+1)[1:*],star_name = 'arcturus'
star_data_to_file, 'pina_themis18', [50.16, 263.93], [214.06,19.124], 1, [124, 127], 82.5, days = (indgen(7)+1)[1:*],star_name = 'arcturus'
star_data_to_file, 'pina_themis18', [50.16, 263.93], [214.06,19.124], 1, [124, 127], 82.5, days = (indgen(31)+1)[14:*],star_name = 'arcturus'
star_data_to_file, 'rank_themis12', [62.82,267.89], [214.06,19.124], 15.7, [136, 119], 82, days = (indgen(31)+1)[1:*],star_name = 'arcturus'
star_data_to_file, 'fykn_themis14', [66.56, 214.79], [214.06,19.124], -27.33, [128,128], 81.5, days = (indgen(31)+1)[2:*],star_name = 'arcturus'

star_data_to_file, 'chbg_themis16', [49.8,285.58], [214.06,19.124], 11, [124,122], 82.5, days = (indgen(6)+1)[2:*],star_name = 'arcturus'
star_data_to_file, 'chbg_themis16', [49.8,285.58], [214.06,19.124], 11, [124,122], 82.5, days = (indgen(31)+1)[20:*],star_name = 'arcturus'
star_data_to_file, 'fsmi_themis10', [59.98, 248.16], [214.06,19.124], -22, [129, 121], 83.5, days = (indgen(31)+1)[2:*],star_name = 'arcturus'
star_data_to_file, 'inuv_themis17', [68.3, 226.52], [214.06,19.124], -33.6, [127,126], 81.5, days = (indgen(31)+1)[2:*],star_name = 'arcturus'
star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [214.06,19.124], -13.6, [127,129], 81.5, days = (indgen(9)+1)[3:*],star_name = 'arcturus'
star_data_to_file, 'mcgr_themis11', [62.95, 205.58], [214.06,19.124], -13.6, [127,129], 81.5, days = (indgen(31)+1)[10:23],star_name = 'arcturus'
star_data_to_file, 'kapu_themis21', [49.39, 277.68], [214.06,19.124], 11, [126,129], 83.5, days = (indgen(31)+1)[3:*],star_name = 'arcturus'
end