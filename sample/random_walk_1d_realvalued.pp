{ true }                          x := 2;

{ x >= 0 }                        while true do
{ x >= 0 }                          if prob(0.7) then
{ x >= 0 }                            z := Unif(0,1);
{ x >= 0 and 0 <= z and z <= 1 }      x := x - z
                                    else
{ x >= 0 }                            z := Unif(0,1);
{ x >= 0 and 0 <= z and z <= 1 }      x := x + z
                                    fi;
{ x >= -1 }                         refute (x < 0)
                                  od
