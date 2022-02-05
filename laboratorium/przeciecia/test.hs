import Przeciecia

h1 = [
        H (P (-2) 2   ) (-1),
        H (P (-2) 1   ) (-1),
        H (P (-2) (-1)) (-1),
        H (P (-2) (-3)) (-1),
        H (P (-1) 0   ) 1,
        H (P (-1) (-2)) 1,
        H (P 1    2   ) 2,
        H (P 1    1   ) 2,
        H (P 1    (-1)) 2,
        H (P 1    (-3)) 2
    ]

v1 = [
        V (P (-2) 2) (-3),
        V (P (-1) 2) (-3),
        V (P 1    2) (-3),
        V (P 2    2) (-3)
    ]


h2 = [
        H (P 0 4) 4,
        H (P 0 3) 3,
        H (P 0 2) 2,
        H (P 0 1) 1
    ]

v2 = []


h3 = [
        H (P (-2) 1   ) 2,
        H (P (-2) (-1)) 2
     ]

v3 = [
        V (P (-1) 2) (-2),
        V (P 1    2) (-2)
     ]


result1 = allintersections h1 v1

result2 = allintersections h2 v2

result3 = allintersections h3 v3
