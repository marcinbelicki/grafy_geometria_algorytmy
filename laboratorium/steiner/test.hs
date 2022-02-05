import Steiner

branch1 = [ 
      P 1 10,
      P 2  9,
      P 4  8,
      P 6  7,
      P 6  6,
      P 8  5,
      P 10 3,
      P 11 2,
      P 13 1
   ]

branch2 = [ 
      P 13 11,
      P 11 10,
      P 9  9 ,
      P 8  9 ,
      P 8  7 ,
      P 5  5 ,
      P 4  4 ,
      P 2  3 ,
      P 1  1
   ]

result1 = approxRSA branch1

result2 = approxRSA branch2
