proc f(x, y);
  var s;

  proc g(u, v);
  var t;

    proc h(p, q);
    begin
      if p = 0 then
        return s + 2*t + 3*q
      else
        return f(p-1, q+1) + g(p-1, q+4)
      end
    end;

  begin (* g *)
    t := v;
    return h(u, v+1)
  end;

begin (* f *)
  s := y;
  return g(x, y+2)
end;

begin
  print f(5, 8); newline
end.

(*<<
 6117
>>*)
