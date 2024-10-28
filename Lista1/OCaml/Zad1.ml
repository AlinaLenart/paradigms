(*Napisać funkcję fiddle4 zmieniającą elementy podanej krotki czteroelementowej w
następujący sposób: (1.3, 2.0, 3.1, 4.2) -> (4.2, 1.3, 3.1, 2.0)*)

let fiddle4 = fun (x, y, z, u) -> (u, x, z -. y);; 

fiddle4((1.3, 2.0, 3.1, 4.2));; 
