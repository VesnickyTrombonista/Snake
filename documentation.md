# SnakeGame

## Uživatelská dokumentace

### Popis programu: 
Tato implemetace odpovídá známé hře snake. Na začátku začneme jen jako hlava hada, která má za úkol prodloužit svůj ocas co nejvíce a přitom nenabourat do sebe ani do zdi. Prodlužování nastane při snězení potravy-jablka.

### Podrobněji: 
Po spuštění .exe souboru nebo .hs souboru ve vscodu uvidíme úvodní obrazovku, která nás vybízí, abychom zmáčkli mezerník pro pokračování do hry. Zároveň vidíme podrobnější informace o právě dosaženém maximálním skóre. 

Jakmile jsme vstoupili do hry tak už ovládáme pohyb hada dle sekce Ovládání (šipky/wasd + mezerník).
Vždy se vygeneruje jen jeden symbol jablka - modrý čtvereček.
Náš had má oranžovou hlavu a zelený ocas, který se po snězení potravy prodlouží.
Každý článek hada je jednotkový.
Pozor si musíme dát na nabourání. Nesmíme narazit do sebe a ani do stěn, které ohraničují hrací pole.
Zároveň nad hrou vidíme aktuální délku hada (skóre). V případě, že nabouráme, tak se vracíme na úvodní obrazovku, kde se zobrazí informace o maximálním skóre. 

Hra nemá úplně vyhrávající stav, ale vítězství je, když had dosáhne maximální délky, tj. zabírá celé hrací pole. Odpovídá to 713 bodům.

Odejít ze hry můžeme v libovolný moment. Skóre se mezi jednotlivým spuštěním programu neukládájí.

### Ovládání:
Hra podporuje ovládání šipkami i wasd. 
Mezerník zrychlí hada. Na úvodní obrazovce nás mezerník jen přesune do hry. 
Odejít ze hry je možné klávesou Esc (případně křížkem okna).


## Programátorská dokumentace

### Implementace:

### Detaily:
713 bodům. (23x31), pak už musíme nutně narazit alespoň do sebe.
Pro načítání skóre mezi jednotlivým spuštěním je možné přidat dokument, který by sloužil pro čtení a zápis maximálního skóre.

#### Závěrem:
