;creating a simple object oriented program

function data::init

;-- allocate memory to pointer when initializing object
  self.ptr=ptr_new(/allocate)
  return, 1
end
;-------------------------------------------------

pro data::set,value

;-- if data value exists, then insert into pointer location

  if n_elements(value) ne 0 then *(self.ptr)=value
  return
  
end
;------------------------------------------------

function data::get,value
;-- if data value is stored in object pointer, then copy it out
  if n_elements(*(self.ptr)) ne 0 then value=*(self.ptr)
  return, value
end

;-------------------------------------------------

function data::cleanup
;-- free memory allocated to pointer when destroying object
  ptr_free,self.ptr
  return,1

end
;-----------------------------------------------

pro data__define
  void={data,ptr:ptr_new()}
  return
  
end