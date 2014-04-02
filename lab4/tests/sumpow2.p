(* lab4/sumpow2.p *)

proc sum(n, f);
  var m, s;
begin
  m := 0; s := 0;
  while m < n do 
    m := m + 1; 
    s := s + f(m)
  end;
  return s
end;

proc sumpow(n, k);

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
  return sum(n, pow)
end;

begin
  print sumpow(5, 4); newline
end.

(*<<
 979
>>*)
