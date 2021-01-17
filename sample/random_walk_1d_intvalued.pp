{ true } x := 1;

{x >= 1} while true do
{x >= 1}   if prob(0.6) then
{x >= 1}     x := x - 1
           else
{x >= 1}     x := x + 1
           fi;
{x >= 0}   refute (x < 1)
         od
