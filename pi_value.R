##### Pi value
##Trapezoidal application

y = function(x){
  
  return(sqrt(1-x^2))
}

area = function(b=1){
  
  h=0.0001
  x =0
  x = x+h
  Ar =0
  while(b>x){
    Ar = Ar + 0.5*(y(x)+y(x-h))*h
    x = x+h
    
  }
  
  return(Ar)
  
}

#######Mid point rule######
area_midpt = function(b=1){
  
  h=0.0001
  x =0
  x = x+h
  Ar =0
  while(b>x){
    Ar = Ar + (y((x + x -h)/2))*h
    x = x+h
    
  }
  
  return(Ar)
  
}

#####Simpson Rule######

area_simpson = function(b=1){
  
  h=0.0001
  x =0
  x = x+h
  Ar =0
  while(b>x){
    Ar = Ar + (y(x-h)+ 4*(y((x+x-h)/2))+y(x))*h/6
    x = x+h
    
  }
  
  return(Ar)
  
}

print(4*area_trapezoidal(b=1))
print(4*area_midpt(b=1))
print(4*area_simpson(b=1))



