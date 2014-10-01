(define fileRules '())
(define fileFacts '())
(define monovalues '())

;citirea din fisier a regulilor
(call-with-input-file "rules.txt"
  (lambda (input-port)
    (let loop ((x (read-char input-port)))
      (if (not (eof-object? x))
          (begin
                (set! fileRules (append fileRules (list (string x))))
            (loop (read-char input-port)))))))

;Citirea din fisier a valorilor monovalued
(call-with-input-file "monovalued.txt"
  (lambda (input-port)
    (let loop ((x (read-char input-port)))
      (if (not (eof-object? x))
          (begin
                (set! monovalues (append monovalues (list (string x))))
            (loop (read-char input-port)))))))


;-------------------------------------------------
;Start parsing zone
;-------------------------------------------------

;Elimina new line-ul si il inlocuieste cu spatiu
(define removeBackslashN
 (lambda(charList acc)
   (if (null? charList)
       acc
       (if (string=? (car charList) "\r")
           (removeBackslashN (cdr charList) acc)
           (if (string=? (car charList) "\n")
            (removeBackslashN (cdr charList) (append acc (list " ")))   
           (removeBackslashN (cdr charList) (append acc (list (car charList))))))
       )
   )
 )

; Formeaza lista de stringuri
(define formWords
  (lambda(charList acc)
    (if (null? charList)
        acc
        (if (string=? (car charList) " ")
            (formWords (cdr charList) (append (list "") acc))
            (formWords (cdr charList) (append (list (string-append (car acc) (car charList))) (cdr acc)))
        )
    )
  )
)

;Elimina stringurile vide
(define removeNullString
  (lambda(lista acc)
    (if (null? lista)
        acc
        (if (string=? (car lista) "")
            (removeNullString (cdr lista) acc)
            (removeNullString (cdr lista) (append acc (list (car lista))))
            ))
    )
  )

;Returneaza cuvintele 
(define getTokens
  (lambda(charList acc)
    (removeNullString (reverse (formWords (removeBackslashN charList '()) acc)) '())
    )
  )

;Spune daca un atribut este monovalued
(define isMonovalued
  (lambda(value monoval)
    (if (null? monoval)
        #f
        (if (string=? (car monoval) value)
            #t
            (isMonovalued value (cdr monoval))
            )
        )
    )
  )

;Parseaza partea din dreapta daca-ului pana la atunci
(define parseIf
  (lambda(wordList acc)
    (if (null? wordList)
        (append (list acc) '(()))
        (if (string=? (car wordList) "atunci")
            (append (list acc) (list wordList))
            (parseIf (cddddr wordList) (append acc (list (append  (list (car (cdr wordList))) (list (car (cdddr wordList))) )) ))
            )
        )
  )
)
  
; Parseaza partea din dreapta atunci-ului pana la un daca sau pana la sfarsitul fisierului
(define parseThen
  (lambda(wordList acc)
    (if (null? wordList)
        (append (list acc) '(()))
        (if (string=? (car wordList) "daca")
            (append (list acc) (list wordList))
            (if (null? (cddddr wordList))
                (parseThen (cddddr wordList) (append acc (list (append  (list (car (cdr wordList))) (append (list (car (cdddr wordList))) (list 1)) )) ))
                (if (string=? (car (cddddr wordList)) "cf")
                    (parseThen (cddr (cddddr wordList)) (append acc (list (append  (list (car (cdr wordList)))  (append (list (car (cdddr wordList))) (list (string->number (car (cddr (cdddr wordList))))))))))
                    (parseThen (cddddr wordList) (append acc (list (append  (list (car (cdr wordList))) (append (list (car (cdddr wordList))) (list 1)) )) ))
                    )
                )
            )
        )
  )
)


;Formeaza regulile in formatul propus de tema
(define formRules
  (lambda(tokens acc)
    (if (null? tokens)
        acc
        (formRules (car (cdr (parseThen (car (cdr (parseIf tokens '()))) '())))
                   (append acc (list (append (list (car (parseIf tokens '()))) (list (car (parseThen (car (cdr (parseIf tokens '()))) '()))) )))
        )
     )
   )
)



;-----------------------------------------------------------
; END PARSING ZONE
;-----------------------------------------------------------

;Calculeaza valoarea minima intre x si y
(define minVal
  (lambda(x y)
    (if (< x y)
        x
        y)
    )
  )
  
;Returneaza true daca o regula are un anumit atribut in concluzie
(define findValueInconclusion
  (lambda(ruleConclusions value)
    (if (null? ruleConclusions)
        #f
        (if (string=? (car (car ruleConclusions)) value)
            #t
            (findValueInconclusion (cdr ruleConclusions) value)
        )
    )
  )
  )

;Spune daca un fapt are coeficient
(define hasCoef
  (lambda(attrVal)
    (if (null? (cddr attrVal))
        #f
        #t)
    )
  )

;Returneaza coeficientul unui fapt
(define getCoef
        (lambda(attrVal)
          (car (cddr attrVal)))
        )

; Returneaza regulile care au ca si concluzie valoarea atributului attr
(define getRulesContainingAttr
  (lambda(rules attr ind acc)
    (if (null? rules)
        acc
        (if (findValueInconclusion (car (cdr (car rules))) attr)
            (getRulesContainingAttr (cdr rules) attr (+ ind 1) (append acc (list ind)))
            (getRulesContainingAttr (cdr rules) attr (+ ind 1)  acc)
            ))
    )
  )

; Returneaza rezultatul atributului din fapte
(define getValueFromFacts
  (lambda(facts attribute acc)
    (if (null? facts)
        acc
        (if (string=? (car (car facts)) attribute)
            (getValueFromFacts (cdr facts) attribute (append acc (list (car (cdr (car  facts))))))
            (getValueFromFacts (cdr facts) attribute  acc)
        )
      )
    )
  )

;Determina daca un atribut exista in fapte
(define existInFacts
  (lambda(attr facts)
    (if (null? facts)
        #f
        (if (string=? (car (car facts)) (car attr))
            #t
            (existInFacts attr (cdr facts))
            )
        )
    )
  )


;Determina daca un string se gaseste intr-o lista
(define exist
  (lambda(lista x)
    (if (null? lista)
        #f
        (if (string=? (car lista) x)
            #t
            (exist (cdr lista) x)))
    )
  )

;Returneaza faptul cu un anumit atribut
(define getFact
  (lambda(facts attr val)
    (if (null? facts)
        '()
        (if (and (string=? (car (car facts)) attr) (string=? (car (cdr (car facts))) val))
            (car facts)
            (getFact (cdr facts) attr val)
            ))
    )
  )

;Returneaza pozitia unui fapt cu un anumit atribut in lista de fapte
(define getFactPos
  (lambda(facts attr val acc)
    (if (null? facts)
        -1
        (if (and (string=? (car (car facts)) attr) (string=? (car (cdr (car facts))) val))
            acc
            (getFactPos (cdr facts) attr val (+ acc 1))
            ))
    )
  )

;Elimina un element dintr-o lista de pe pozitia pos
(define removeFromList
 (lambda(lista pos)
   (if (= pos 0)
       (cdr lista)
       (append (list (car lista)) (removeFromList (cdr lista) (- pos 1)))
       )
   )
 )

;Insereaza un element in fapte conform regulii CF = CF1 - CF2*(1-CF1)
(define insertIntoFacts
  (lambda(facts newFact)
    (if (null? (getFact facts (car newFact) (car (cdr newFact))))
        (append facts (list newFact))
        (append (removeFromList facts (getFactPos facts (car  newFact) (car (cdr newFact)) 0)) 
                ( list (list (car newFact) (car (cdr newFact))  
                      (+ (car (cddr (list-ref facts (getFactPos facts (car  newFact) (car (cdr newFact)) 0) ))) 
                         (* (car (cddr newFact)) (- 1 (car (cddr (list-ref facts (getFactPos facts (car  newFact) (car (cdr newFact)) 0) ))))))    )))
    ))
  )

;Insereaza mai multe fapte in lista de fapte
(define insertAllFacts
  (lambda(facts newFacts)
    (if (null? newFacts)
        facts
        (insertAllFacts (insertIntoFacts facts (car newFacts)) (cdr newFacts)))
    )
  )

;Filtreaza concluzia astefel incat sa existe doar atributele cautate
(define filterConclusion
  (lambda(listaConcluzii attribut acc)
    (if (null? listaConcluzii)
        acc
        (if (string=? (car (car listaConcluzii)) attribut)
            (filterConclusion (cdr listaConcluzii) attribut (append acc (list (car listaConcluzii))))
            (filterConclusion (cdr listaConcluzii) attribut acc))
            )
        )
    )


;Aplica coeficientul unei premise peste o lista de concluzii conform formulei din tema
(define applyCoef
  (lambda(factsList coefPremisa acc)
    (if (null? factsList)
        acc
        (applyCoef (cdr factsList) coefPremisa (append acc (list (list (car (car factsList)) (car (cdr (car factsList))) (* coefPremisa (car (cddr (car factsList))))))))
        )
    )
  )


;Verifica valoarea de adevar a unei ipoteze data ca lista de conditii
;rules - lista de reguli
;ipothesis ((atr1 = val1) ...(atrn = valn)
;fapte ((atr1 = val1) ...(atrm = valm)
(define evaluateCond
  (lambda(rules ipothesis min)
    ;Daca multimea de ipoteze e goala inseamna ca au fost verificate toate conditiile din lista
    (if (null? ipothesis)
        min
        (if (null? (getValueFromFacts fileFacts (car (car ipothesis)) '()))
            ;Daca valoarea nu se gasese in facts incercam sa o determinam
                (if  (null? (detValoare rules (car (car ipothesis)) '()))
                    ;Daca valoarea nu a putut fi determinata atunci toata conditia nu se satisface
                    -1
                    ;Verificam daca una din valorile determinate (pot fi mai multe pentru atributele multivaloare) este cea care duce la indeplinirea conditiei curent
                    (if (exist (getValueFromFacts fileFacts (car (car ipothesis)) '()) (car (cdr (car ipothesis))))
                        ; Daca conditia curenta este satisfacuta trecem la urmatoarea
                        (evaluateCond rules (cdr ipothesis) (minVal min (car (cddr (getFact fileFacts (car (car ipothesis)) (car (cdr (car ipothesis))))))))
                        ;Daca conditia nu este satisfacuta se returneaza fals
                        -1
                    )
                 )
            ;Daca valoarea se gaseste in facts
                (if (exist (getValueFromFacts fileFacts (car (car ipothesis)) '()) (car (cdr (car ipothesis))))
                    (evaluateCond rules (cdr ipothesis) (minVal min (car (cddr (getFact fileFacts (car (car ipothesis)) (car (cdr (car ipothesis)))))))) 
                    -1
                )
          )
       )
    )
  )


; rulesList (((lista ipoteze) (lista concluzii))....)
;Iteram printr-un set de reguli care au o concluzie comuna si le pastram pe acelea care au premisele  adevarate
(define iterateTroughRules
  (lambda(rules rulesList acc)
    (set! fileFacts (insertAllFacts fileFacts acc))
    ;(write fileFacts)
    (if (null? rulesList)
        (list fileFacts)
        (if ( >  (evaluateCond rules (car (list-ref rules (car rulesList))) 1) -1)
            (iterateTroughRules rules (cdr rulesList) (append acc  (applyCoef (car (cdr (list-ref rules (car rulesList)))) (evaluateCond rules (car (list-ref rules (car rulesList))) 1) '())))
            (iterateTroughRules rules (cdr rulesList) acc)
            )
        )
    )
  )
;Functia care intreaba utilizatorul valoarea atributului
(define readInput
  (lambda(atribut)
    (write "Care este valoarea pentru atibutul ")(write atribut)(write "? ")
    (let ((x (read-line)))
      x)
    )
  )


;Determina valoarea unui atribut intreband utilizatorul sau cautand in faptele deja existente
;Returneaza o lista care contine pe prima pozitie valoarea de adevar a determinarii valorii 
;iar pe a 2-a o lista cu faptele noi determinate, adaugate la vechiul set
(define detValoare
  (lambda(rules atribut result)
    (if (null? (getRulesContainingAttr rules atribut 0 '()))
         (let ((x (readInput atribut)))
           (set! fileFacts (insertAllFacts fileFacts (list (append (list atribut) (append (list x) (list 1))))))
           (if (string=? x "unknown")
               '()
               (list x)))
         (if (not (isMonovalued atribut (getTokens (removeBackslashN monovalues '()) '(""))))
             (filterConclusion (car (iterateTroughRules rules (getRulesContainingAttr rules atribut 0 '()) '())) atribut '())
             (list (car (filterConclusion (car (iterateTroughRules rules (getRulesContainingAttr rules atribut 0 '()) '())) atribut '()))))
    )
  )
)

;Formeaza rezultatul in formatul propus de tema
(define filterFinalResult
  (lambda(rezList acc)
    (if (null? rezList)
        acc
        (filterFinalResult (cdr rezList) (append acc (list (list (list-ref (car rezList) 1) (list-ref (car rezList) 2)))))
        )
    )
  )

(define go
  (lambda(atribut)
    (append (list atribut) (filterFinalResult (detValoare (formRules (getTokens (removeBackslashN fileRules '()) '("")) '()) atribut  '()) '()))
    )
  )

(go "mod")


