data Tree = Node [Tree]

-- Funkcja zbiór - wyznacza moc najliczniejszego zbioru niezależnego drzewa t
zbior :: Tree -> Int
zbior t = head $ zbior_help t where
    suma = foldl (\x y -> zipWith (+) x $ zbior_help y) [0,1] 
    zbior_help (Node l) = [wezel,sumapotomkow] where 
        list = suma l
        sumapotomkow = head list
        wezel = maximum list

a :: Tree
a = do { 
    Node [
        Node [
            Node [],
            Node []
        ],
        Node [
            Node [],
            Node []
        ]
    ]
}

b :: Tree
b = do { 
    Node [
        Node [
            Node [
                Node [
                    Node []
                ]
            ]
        ],
        Node [
            Node [],
            Node [],
            Node [
                Node [],
                Node [],
                Node [],
                Node []
            ]
        ]
    ]
}

-- Wartość result - moc najliczniejszego zbioru niezależnego dla drzewa a 
result :: Int
result = zbior a
