{ true }               x := 2;

{ 0 <= x and x <= 13 } while true do
{ 0 <= x and x <= 10 }   if prob(0.8) then
{ 0 <= x and x <= 10 }     skip
                         else
{ 0 <= x and x <= 10 }     if prob(0.5) then
{ 0 <= x and x <= 10 }       x := x + 1
                           else
{ 0 <= x and x <= 10 }       x := x + 2
                           fi
                         fi;
{ 0 <= x and x <= 12 }   if * then
{ 0 <= x and x <= 12 }     if prob(0.875) then
{ 0 <= x and x <= 12 }       x := x - 1
                           else
{ 0 <= x and x <= 12 }       skip
                           fi
                         else
{ 0 <= x and x <= 12 }     if prob(0.8) then
{ 0 <= x and x <= 12 }       skip
                           else
{ 0 <= x and x <= 12 }       if prob(0.5) then
{ 0 <= x and x <= 12 }         x := x + 1
                             else
{ 0 <= x and x <= 12 }         x := x + 2
                             fi
                           fi;
{ 0 <= x and x <= 14 }     x := x - 1
                         fi;
{ 0 <= x and x <= 13 }   refute (x <= 0)
                       od
