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
Zároveň nad hrou vidíme aktuální délku hada (skóre). V případě, že nabouráme, tak se vracíme na úvodní
obrazovku, kde se zobrazí informace o maximálním skóre. 

Hra nemá úplně vyhrávající stav, ale vítězství je, když had dosáhne maximální délky, tj. zabírá celé hrací
pole. Odpovídá to 713 bodům.

Odejít ze hry můžeme v libovolný moment. Skóre se mezi jednotlivým spuštěním programu neukládájí.

### Ovládání:
Hra podporuje ovládání šipkami i wasd. 
Mezerník zrychlí hada. Na úvodní obrazovce nás mezerník jen přesune do hry. 
Odejít ze hry je možné klávesou Esc (případně křížkem okna).


## Programátorská dokumentace

### Implementace:
V mainu volám pro funkci play všechny funkce, které obstarávají hru.
Nastavím si okno, pozadí, počet FPS, počáteční stav okna a pak je přidám funkci na renedrování textu a okrajů hracího pole na obrazovce, funkci na obsloužení kláves a update stavu hry.

Okno si natavím podle libosti, jen je dobré pamatovat na to, aby se mi tam vešla samotná hra podle počtu řádků a sloupců a barva pozadí mi vyhovuje bílá.

Počet FPS mi ovlivňuje rychlost a plynulost hry.

V počátečním stavu jen nastavím pozici hlavy hada, pozici prvního jídla a směr. Dále vynuluji skóre a přidám generátor čísel pro generování náhodné pozice.

Funkce renderAll kreslí hrací pole, hada i jídlo. FillRectangleBy využívá hodnoty pro odsazení od pravého spodního rohu. Také využívám funkcí fromIntegral, protože translate pracuje nad Floaty. 
Při konci hry se vypíšou i potřebné texty o stavu hry, jinak se při hraní zobrazuje aktuální skóre.

Na obsluhování káves není nic zvláštního. Při detekci stisknutí upravím směr, u mezerníku buď spustím hru nebo zrychlím v jednomm kroku pohyb hada, jinak nedělám nic. Jak jsem psal, esc je zabudované pro konec.
Jen technická, řešil jsem otáčení hada. To má na starosti funkce changeDirection. Tam zakazuji otočení se čelem vzad, ale při rychlém dvojkliku kláves lze podobného výsledku dosáhnout také. Pro vyřešení i tohoto případu by bylo za potřebí znát, jestli se od poslední změny směru změnila i pozice hlavy. Funkce fst a snd zpracovávají jednottlivé složky vektorů.

V updatu zkontroluji, jestli jsem nenarazil do krajů nebo do sebe a pokračuji s pohybem. Na to si zavolám vnější funkci na pohyb, které při snězení vrátí true a rozšíří hada o hlavu (délka se přidává na začátek kvůli rychlosti a render ji přebarví dle potřeby.) A pokud jsem snědl jídlo, tak zvětším skóre a zavolám si generátor na nové jídlo. Generátor má meze o jedna menší, aby se jídlo negenerovalo už na krajích, na které se nelze dostat.

### Detaily:
Většina detailů je vysvětlena v komentářích přímo v kódu, aby to bylo na místě, kde to poslouží nejvíce.
Maximum: 713 bodů (23x31) odpovídá rozměrům pole, protože nultý a poslední index tvoří hranice pole.
Větší skóre získat nelze, pak už musíme nutně narazit alespoň do sebe/do zdi.
Není zatím otestováno, jak se začne chovat generátor, pokud bychom tohoto skŕe dosáhli, nejspíš by nám pod hlavu dal potravu a mi bychom si kousli do ocasu/narazili do stěny.

### Možné vylepšení:
Pro načítání skóre mezi jednotlivým spuštěním je možné přidat dokument, který by sloužil pro čtení a zápis maximálního skóre a tak v případě velmi dobrého výsledku bychom tuto informaci neztratili.
Pro rozměry by bylo vhodné už zavést globální proměnné, aby nebylo zmšnu informace přepisovat všude v kódu.

#### Závěrem:
Tvoření hry mě bavilo a dost jsem se u toho naučil, např. pracovat s haskellem a jeho knihovnami. Také mě potěšilo, že lze udělat okenní aplikaci a hru v ní.