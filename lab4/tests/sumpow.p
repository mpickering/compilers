(* lab4/sumpow.p *)

proc sumpow(n, k);
  var m, s;

  proc pow(p);
    var j, q;
  begin
    j := 0; q := 1;
    while j < k do 
      j := j+1; q := q*p
    end;
    return q
  end;

begin
  m := 0; s := 0;
  while m < n do 
    m := m + 1; 
    s := s + pow(m)
  end;
  return s
end;

begin
  print sumpow(5, 4); newline
end.

(*<<
 979
>>*)
