var n, v, w, k: integer; 
var d: array 32 of boolean;

begin
  v := 31415926;

  k := 0;
  while v <> 0 do
    d[k] := (v mod 2) <> 0;
    v := v div 2;
    k := k+1
  end;

  w := 0;
  while k > 0 do
    k := k-1;
    w := 2*w;
    if d[k] then w := w+1 end
  end;

  print w; newline
end.

(*<<
 31415926
>>*)
