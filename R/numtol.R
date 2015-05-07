numtol = function(x,ndig=1) {
  l=floor(log10(abs(x)))-ndig+1
  del= 0.5*10^l
  return(del)
}

