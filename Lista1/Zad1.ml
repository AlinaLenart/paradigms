(*Napisać funkcję fiddle4 zmieniającą elementy podanej krotki czteroelementowej w
następujący sposób: (1.3, 2.0, 3.1, 4.2) -> (4.2, 1.3, 3.1, 2.0)*)

let fiddle4 = fun(x, y, z, u) -> (u, x, z, y);; (*wyrazenie funkcji*)

fiddle4((1.3, 2.0, 3.1, 4.2));; (*zwroci krotke ze zmieniona kolejnoscia*)
fiddle4((1, 2, 3, 4)) = (4, 1, 3, 2) (*zwroci true bo po wykonaniu funkcji przyrownam wynik do oczekiwanego*)

