
;;;; ================================================
;;;; = 3I020 - Principes des programmes déclaratifs =
;;;; ================================================
;;;; = TME Solo - édition 2015/2016                 =
;;;; ================================================

(ns tme-solo.tme1
  (:use midje.sweet))

;;; Exercice 1
;;; ==========

;;; Question 1.1
;;; ------------

;;; Compléter la définition de fonction suivante :

(defn separation
  "Partitionne une collection `coll` (ou une séquence) en deux parties:
  - la première composé des éléments reconnaissant le prédicat `p?`
  - la seconde composée des éléments ne le reconnaissant pas."
  [p? coll]
  (loop [c coll p1 [] p2 []]
    (cond
      (not (seq c)) (vector p1 p2)
      (p? (first c)) (recur (rest c) (conj p1 (first c)) p2)
      :else (recur (rest c) p1 (conj p2 (first c))))))

;;; afin de valider les tests ci-dessous :

(fact "à propose de `separation`."

      (separation even? [1 2 3 4 5 6 7 8 9])
      => [[2 4 6 8], [1 3 5 7 9]]

      (separation string? [1 "un" 2 "deux" 3 "trois"])
      => [["un" "deux" "trois"], [1 2 3]]

      (separation #(<= % 5) [5 2 3 9 7 1 8 4 6])

      => [[5 2 3 1 4], [9 7 8 6]]

      )

;;; Question 1.2
;;; ------------

;;; Utiliser `separation` pour réaliser un tri de séquence par fusion
;;; consistant à séparer une séquence (e1 e2 ... eN) en deux vecteurs
;;; - un premier vecteur des éléments strictement inférieurs à e1
;;; - un second vecteur des autres éléments de la séquence
;;; et en triant récursivement les deux sous-vecteurs

;;; Remarque 1 : le résultat retourné est un vecteur trié
;;; Remarque 2 : on ne demande pas de solution récursive terminale,
;;;              car l'exercice devient alors assez difficile
;;;              (challenge maison).

(defn tri
  "Trie la séquence `s` passée en paramètre, en
  fonction du comparateur `<?`"
  [s <=?]
  (cond 
    (> (count (take 3 s)) 2 ) (let [p #(<=? (first s) %) tabs (separation p (rest s))]
                                (concat (tri (second tabs) <=?) (vector (first s)) (tri (first tabs) <=?)))
    (or (< (count s) 2) (<=? (first s) (second s))) s
    :else (vector (second s) (first s))))


(fact "Quelques exemples de `tri`."

      (tri [5 2 3 9 7 1 8 4 6] <=)
      => [1 2 3 4 5 6 7 8 9]

      (tri ["quatre" "deux" "un" "cinq" "trois"] #(< (count %1) (count %2)))
      => ["un" "cinq" "deux" "trois" "quatre"]
      )

;;; Exercice 2
;;; ==========

;;; Question 2.1
;;; ------------

;;; Donner une définition (récursive, non-terminale) de la fonction `cond-xform`

(defn cond-xform
  [hd tl s]
  (cond
    (< (count s) 2) tl
    (= (count s) 2) (list hd (first s) (second s) tl)
    (zero? (rem (count s) 2)) (reverse (cons (cond-xform hd tl (rest (rest s))) (list (second s) (first s) hd)))
    :else (println "Erreur de nombre d'éléments du 3e argument")))

;;; telle que les tests suivants soient validés :

(fact "les tests pour `cond-xform`"
      (cond-xform 'quand 'sinon '())
      => 'sinon

      (cond-xform 'quand 'sinon '(cond1 alors1))
      => '(quand cond1 alors1 sinon)

      (cond-xform 'quand 'sinon '(cond1 alors1 cond2 alors2))
      => '(quand cond1 alors1
                 (quand cond2 alors2 sinon))

      (cond-xform 'quand 'sinon '(cond1 alors1 cond2 alors2 cond3 alors3))
      => '(quand cond1 alors1
                 (quand cond2 alors2
                        (quand cond3 alors3 sinon))))

;;; Question 2.2.
;;; -------------

;;; On souhaite redéfinir la macro `cond` dont on rappelle le fonctionnement ci-dessous :

(defn cond-type-de [x]
  (cond
    (number? x) :nombre
    (string? x) :chaine
    (keyword? x) :mot-cle
    (symbol? x) :symbole
    :else :j-en-sais-rien))

(fact "sur `cond-type-de`."
      (cond-type-de 3) => :nombre
      (cond-type-de "trois") :chaine
      (cond-type-de :trois) :mot-cle
      (cond-type-de 'trois) :symbole
      (cond-type-de [1 2 3]) :j-en-sais-rien)

;;; On peut réécrire la fonction précédente sans macro, en utilisant
;;; uniquement la forme spéciale `if`.

(defn if-type-de [x]
  (if (number? x) :nombre
      (if (string? x) :chaine
          (if (keyword? x) :mot-cle
              (if (symbol? x) :symbole
                  (if :else :j-en-sais-rien nil))))))

(fact "sur `if-type-de`."
      (if-type-de 3) => :nombre
      (if-type-de "trois") :chaine
      (if-type-de :trois) :mot-cle
      (if-type-de 'trois) :symbole
      (if-type-de [1 2 3]) :j-en-sais-rien)

;;; Il faut remarquer ici que la transformation du `cond` vers `if`
;;; ressemble beaucoup à la transformation effectuée paramètre
;;; la fonction `cond-xform`.

;;; En déduire une définition de la macro `my-cond` en utilisant
;;; `cond-xform`.

;;; (cond)  => nil
;;; (cond c1 t1) => (if c1 t1 nil)
;;; (cond c1 t1 c2 t2) => (if c1 t1 (if c2 t2 nil))
;;; (cond c1 t1 c2 t2 c3 t3) => (if c1 t1 (if c2 t2 (if c3 t3 nil)))

(defmacro my-cond [& clauses]
 `(cond-xform 'if 'nil (list ~@clauses)))

;;; Afin de faire passer les tests suivants:

(defn my-cond-type-de [x]
  (eval (my-cond
   (number? x) :nombre
   (string? x) :chaine
   (keyword? x) :mot-cle
   (symbol? x) :symbole
   :else :j-en-sais-rien)))

(fact "sur `my-cond-type-de`."
      (my-cond-type-de 3) => :nombre
      (my-cond-type-de "trois") :chaine
      (my-cond-type-de :trois) :mot-cle
      (my-cond-type-de 'trois) :symbole
      (my-cond-type-de [1 2 3]) :j-en-sais-rien
      )


