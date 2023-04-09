Autor: Ondřej Mach (xmacho12)

# Logický projekt FLP22: Babylonská věž

Tento projekt implementuje řešení Babylonské věže v jazyce Prolog.
Program se chová podle zadání, implementace je schopná vyřešit zadaný příklad.
Implementace využívá algoritmu IDS (Iterative Depth Search) pro prohledávání stavového prostoru.

# Datová struktura pro věž

Věž je interně reprezentována jako matice, která je tvořena seznamem seznamů.
Vnořené seznamy představují řádky matice.
Každý ze zanořených seznamů obsahuje termy `tuple(X, Y)`, kde proměnné `X` a `Y` reprezentují požadované souřadnice kuličky.
Speciálním prvkem je `empty`, který zastupuje volné místo ve věži.
Pro ilustraci je datová struktura předvedena na jednoduchém příkladu.

```
$ cat test/easy.in
B1 B2
** A1
```

Zdrojový soubor také obsahuje debugovací predikát `read_and_write/0`, který načte reprezentaci věže ze standardního vstupu a vypíše interní reprezentaci.

```
$ swipl -g 'read_and_write' -s src/flp22-log.pl < test/easy.in
[[tuple(1,2),tuple(2,2)],[empty,tuple(1,1)]]
```

Vstup a výstup programu jsou implementovány predikáty `read_tower/1` a `print_tower/1`.
Ty využívají predikátu `inrow_to_towerrow/2`, který dokáže obousměrně převádět vnější a vnitřní reprezentaci.
Tento přístup snižuje duplikaci kódu a je specifický pro Prolog.
Pro otestování vstupních a výstupních predikátů také existuje ladící predikát `read_and_print/0`.
Ten načte věž ze standardního vstupu a následně ji vypíše na standardní výstup.
Jednoduchý test vstupu a výstupu lze spustit příkazem `make test`.

```
swipl -g read_and_print -s src/flp22-log.pl < test/spec_example.in
```

# Pravidla pro manipulaci věže

Legální tahy věže jsou definovány predikátem `move/2`.
Ten je implementován ve dvou klauzulích.
První představuje rotaci patra věže, zatímco druhá představuje vertikální posun kuliček ve sloupci s volným místem.
Predikát `move` je dále využit v prohledávání stavového prostoru.
Pro ladící účely obsahuje zdrojový kód predikát `list_moves/0`, který načte věž ze stdin a vypíše všechny legální tahy.

```
$ swipl -g 'list_moves' -s src/flp22-log.pl < test/easy.in
B2 B1
** A1

B1 B2
A1 **

** B2
B1 A1
```

# Prohledávací algoritmus

Vyhledávací algoritmus byl původně implementován jako BFS (Breadth First Search).
Toto řešení bylo dostatečné pro malé stavové prostory.
Pro zadání s větším stavovým prostorem už ale nefungovalo kvůli paměťové náročnosti.
Např. na referenční věž v zadání bylo využito přes 25 GB RAM a stále nebyla spočítána.

Proto bylo potřeba najít nový algoritmus.
Nejprve byl implementován DLS (Depth Limited Search).
Ten ale nedošel vždy k optimálnímu řešení.
Proto byl přepracován na IDS (Iterative Depth Search), který je optimální (jako BFS)
a zároveň má nízkou paměťovou složitost.

Algoritmus ale stále nedosahoval hloubky, která je třeba pro vyřešení referenčního zadání.
Z profilování vyplynulo, že velké množství času je stráveno predikátem `member`.
Ten kontroluje, zda nově vygenerované uzly nejsou v seznamu již navštívených (`Visited`).
Nezanedbatelný čas byl také stráven predikátem `append`, který přidává nové uzly do tohoto seznamu.

Pro množinu `Visited` bylo nutné najít efektivnější datovou strukturu.
Testovány byly knihovny `ordsets` a `assoc`.
Zatímco `ordsets` nijak nezlepšila výkon (zřejmě kvůli častému vkládání), asociativní pole zrychlilo implementaci řádově.
Důvodem je, že `assoc` interně používá AVL stromy, proto má komplexitu vkládání i vyhledávání logaritmickou.

Poté už bylo možné vyřešit referenční úlohu, program běžel 1min 44s, a spotřeboval maximálně 2.4 GB RAM.

```
$ ./flp22-log < test/spec_example.in
D1 E1 F1 A1 B1 C1
F2 E3 B2 C2 D2 E2
D3 E4 F3 A2 B3 C3
A3 B4 C4 D4 ** F4

D1 E1 F1 A1 B1 C1
C2 D2 E2 F2 E3 B2
D3 E4 F3 A2 B3 C3
A3 B4 C4 D4 ** F4

D1 E1 F1 A1 B1 C1
C2 D2 E2 F2 E3 B2
A2 B3 C3 D3 E4 F3
A3 B4 C4 D4 ** F4

D1 E1 F1 A1 B1 C1
C2 D2 E2 F2 ** B2
A2 B3 C3 D3 E3 F3
A3 B4 C4 D4 E4 F4

D1 E1 F1 A1 B1 C1
** B2 C2 D2 E2 F2
A2 B3 C3 D3 E3 F3
A3 B4 C4 D4 E4 F4

A1 B1 C1 D1 E1 F1
** B2 C2 D2 E2 F2
A2 B3 C3 D3 E3 F3
A3 B4 C4 D4 E4 F4

A1 B1 C1 D1 E1 F1
A2 B2 C2 D2 E2 F2
A3 B3 C3 D3 E3 F3
** B4 C4 D4 E4 F4
```

