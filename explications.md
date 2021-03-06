# A propos du commentaire avant `init_sheet` :

La [doc](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html#VALmake_matrix) précise

> All the elements of this new matrix are initially physically equal to `e`

La stratégie de OCaml est le [copy-on-write](https://fr.wikipedia.org/wiki/Copy-on-write), c'est à dire que tous les éléments du tableau pointent originellement vers le même emplacement en mémoire, qui contient une copie de la valeur de `e`, puis si on modifie une case du tableau, on effectue une copie pour cette case uniquement. Ça prend moins de temps de calcul, et moins de mémoire.\
Par contre, si la valeur d'initialisation du tableau est mutable (une `ref` ou, comme c'est le cas ici, un record avec un élément `mutable`), on copie dans le tableau la référence (le "pointeur") vers cette valeur, et pas la référence vers une copie de la valeur. Ainsi, quand on modifie une case on les modifie toutes !\
Exemple très parlant :
```ocaml
# let y = ref 0;;
val y : int ref = {contents = 0}
# let t = Array.make_matrix 2 2 y;;
val t : int ref array array =
  [|[|{contents = 0}; {contents = 0}|]; [|{contents = 0}; {contents = 0}|]|]
# incr y;;
- : unit = ()
# t;;
- : int ref array array =
[|[|{contents = 1}; {contents = 1}|]; [|{contents = 1}; {contents = 1}|]|]
```

Dans `init_sheet` on remplace la référence dans chaque case du tableau par une nouvelle référence unique, ce qui supprime l'aliasing.\
Continuons l'exemple précédent :
```ocaml
# t.(0).(0) <- ref 0;;
- : unit = ()
# t;;
- : int ref array array =
[|[|{contents = 0}; {contents = 1}|]; [|{contents = 1}; {contents = 1}|]|]
# incr y;;
- : unit = ()
# t;;
- : int ref array array =
[|[|{contents = 0}; {contents = 2}|]; [|{contents = 2}; {contents = 2}|]|]
# incr t.(0).(1);;
- : unit = ()
# t;;
- : int ref array array =
[|[|{contents = 0}; {contents = 3}|]; [|{contents = 3}; {contents = 3}|]|]
```

# Choix du parcours de l'arbre des dépendances entre cellules

Pour parcourir l'arbre des dépendances dans l'étape 2.2, on peut indifféremment choisir un parcours en largeur ou un parcours en profondeur.\
La situation qui pourrait poser problème est de la forme :
```
      A
     / \
    C   B
        |
        C
```
Or après chaque calcul d'une cellule dont C dépend, on recalcule au moins une fois C (plus ou moins tôt selon le type de parcours). Donc le dernier calcul de C s'effectue après le calcul de chacune de ses dépendendances, ce qui assure que C a la bonne valeur.

# Type `number`

Somme de `float` et `int`, avec coercion de type vers `float` si nécessaire (au moins un `float` dans le calcul)\
La moyenne fait toujours intervenir une division, donc `float`
