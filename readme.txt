Tema2

Nume: Mihai Ionut Cosmin
Grupa: 342C4

*Continut Arhiva:
Tema2.rkt
rules.txt
monovalued.txt
baza_de_cunostiinte2.txt
baza_de_cunostiinte_3.txt

Fisierul Tema2.rkt contine rezolvarea temei
Fisierul rules.txt contine regulile in formatul:

daca atribut1 = valoare1
si atribut2 = valoare2
...si atributn = valoaren
atunci atribut1 = valoare1 cf coeficient1
...si atributm = valoarem cf coeficitntm
Exemplu:
daca sos-meniu = sos-alb
atunci tip-vin = sec cf 0.8
si tip-vin = demisec cf 0.6

Fisierul monovalued.txt contine pe fiecare linie numele fiecarui atribut monovalued.

baza_de_cunostiinte 2 si 3 contin exemple de baze de cunostiinte.

Parserul considera ca fisierul este intr-un format scris corect.
Coeficientul d certitudine al faptelor citite de la tastatura e 1.
Am rezolvat tema folosind algoritmul pentru reguli cu inlantuire inapoi din curs de la pagina 19, cursul 5.
Practic metoda e urmatoarea:
Se da un atribut pentru care se vrea valoarea sau valorile.La urmatorul pas se colecteaza
toate regulile care au in concluzie acel atribut, apoi se itereaza prin fiecare si se afla valoarea
de adevr a premisei(ori extrasa din fapte, ori determinandu-se in acelasi mod in care e determinata
valoarea curenta).Functia de evaluare a premisei intoarce fie -1, in cazul in care este falsa,
fie coeficientul de certitudine aflat ca minimul dintre coeficientii premiselor.Peste coeficientii
curenti ai concluziilor se aplica coeficientii premisei, facandu-se un produs.
Daca un atribut este unknown se intoarce lista vida.
