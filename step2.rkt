; заготовка "Доктора". Сентябрь 2022
#lang scheme/base
(require racket/vector)

; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name '())
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)

; цикл диалога Доктора с пациентом с сохранением истории ответов
; параметр name -- имя пациента
; параметр history -- список ответов пациента
(define (doctor-driver-loop-v2 name history)
  (newline)
  (print '**)
  (let ((user-response (read)))
    (cond
      ((equal? user-response '(goodbye))
       (printf "Goodbye, ~a!\n" name)
       (print '(see you next week)))
      (else (print (reply-v2 user-response history))
            (doctor-driver-loop-v2 name (cons user-response history))
      )
    )
  )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
      (case (random 0 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (hedge-answer))  ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ

      )
)

; версия генерации ответной реплики с тремя способами
(define (reply-v2 user-response history)
  (let ((number-of-choices (if (null? history) 2 3))) ; 3й способ не доступен, если список истории пуст
        (case (random 0 number-of-choices)
          ((0) (hedge-answer)) ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ
          ((2) (history-answer history)) ; 3й способ
        )
  )
)

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (can you describe it in more detail?)
                              (it's absolutely normal to feel that way)
                              (can you be more informative))
         )
)

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
)

; случайный выбор одного из элементов непустого списка
(define (pick-random-list lst)
  (list-ref lst (random 0 (length lst)))
)

; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (what do you think is the reason why)
                                       (you emphasize that)
                                       (what makes you think that))
                )
                (change-person user-response)
        )
 )

; 3й способ генерации ответной реплики -- замена лица в ранее сказанной реплике пользователя и приписывание к результату заготовленного начала
(define (history-answer history)
  (append '(earlier you said that) (change-person (pick-random-list history)))
)

; замена лица во фразе
(define (change-person phrase)
        (many-replace-v3
		'((am are)
        (are am)
        (i you)
        (me you)
        (mine yours)
        (my your)
        (myself yourself)
        (you i)
        (your my)
        (yours mine)
        (yourself myself)
        (we you)
        (us you)
        (our your)
        (ours yours)
        (ourselves yourselves)
        (yourselves ourselves)
        (shall will))
                      phrase)
 )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
)

; итеративная версия many-replace
(define (many-replace-v2 replacement-pairs lst)
  (let helper ((replacement-pairs replacement-pairs) (lst lst) (answer '())) ; вспомогательная функция, результат строится в список answer
    (cond ((null? lst) (reverse answer))
          (else (let ((pat-rep (assoc (car lst) replacement-pairs)))
                  (helper 
                     replacement-pairs ; 1й аргумент
                     (cdr lst) ; 2й аргумент
                     (cons (if pat-rep (cadr pat-rep)
                               (car lst)
                           )
                           answer) ; добавляем в ответ нужное слово
                   )
                )
          )
     )
  )
)

; реализация many-replace с использованием map
(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (n)
         (let ((pat-rep (assoc n replacement-pairs)))
          (if pat-rep (cadr pat-rep)
              n
          )
         )
       )
       lst
  )
)
; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i 0) (result init))
   (if (= i length) result
    (loop (add1 i) (f i result (vector-ref vctr i)))))))
	
; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i (sub1 length)) (result init))
   (if (= i -1) result
    (loop (sub1 i) (f i result (vector-ref vctr i)))))))



;---------------------- block 2 --------------------------

(define (ask-patient-name)
  (begin
    (println '(next!))
    (println '(who are you?))
    (print '**)
    (car (read))
  )
)

(define (visit-doctor-v2 stop-word limit-people)
  (let new-visitor ((stop-word stop-word) (limit-people limit-people) (name (ask-patient-name)))
        (cond ((or (equal? stop-word name) (= limit-people 0)) (println '(time to go home))) ; выход из цикла по имени, либо 0-м количестве пациентов
              ((begin
                (printf "Hello, ~a!\n" name) ; приветствие для каждого нового пациента
                (print '(what seems to be the trouble?))
                (doctor-driver-loop-v2 name '())
                (if (= limit-people 1) (printf "\nTime to go home\n") ; выход после последнего пациента
                    (new-visitor stop-word (- limit-people 1) (ask-patient-name))
                )
              ))
        )
  )
)


(define keywords-structure '#(
  #( ; 1-я группа
    #(depressed suicide exams university)
    #(  
	  (when you feel depressed, go out for ice cream)
          (depression is a disease that can be treated)
	)
  )
  #( ; 2-я группа 
    #(mother father parents brother sister uncle aunt grandma grandpa)
    #(
	  (tell me more about your * , i want to know all about your *)
          (why do you feel that way about your * ?)
	)
  )
  #( ; 3-я группа
    #(university scheme lections)
	#(
	  (your education is important)
	  (how much time do you spend on your studies ?)
	)
  )
))


; вектор всех ключевых слов
(define keywords (vector-append (vector-ref (vector-ref keywords-structure 0) 0)
                                (vector-ref (vector-ref keywords-structure 1) 0)
                                (vector-ref (vector-ref keywords-structure 2) 0)))



; проверка на наличие хотя бы одного ключевого слова в предложении
(define (have-keywords? statement keywords)
  (let ((id-list (build-list (length statement) values))) ; список индексов в предложении
        (number? (ormap (lambda (n) (vector-member (list-ref statement n) keywords)) id-list))
  )
)

; вспомогательная функция, вычисляющая количество возможных выборов стратегий
(define (number-of-choices user-response history keywords)
  (cond ((have-keywords? user-response keywords) (if (null? history) 3 4))
        ((not (null? history)) (if (have-keywords? user-response keywords) 4 3))
        (else 2)))


;(vector-map (lambda (n) (vector-append (vector-ref (vector-ref keywords-structure n) 0))) (build-vector 3 values))
(define (get-keywords2 keywords-structure)
  (vector-append (vector-map (lambda (n) (vector-ref (vector-ref keywords-structure n) 0)) (build-vector 3 values)))
)

;(define kw (get-keywords2 keywords-structure))

;(define (have-keywords2? statement keywords)
 ; (let ((id-list (build-list (length statement) values))
  ;      (str-vector (build-vector (vector-length keywords) values))) ; список индексов в предложении
   ;     (vector-map (lambda (y) (ormap (lambda (n) (vector-member (list-ref statement n) (vector-ref y 0))) id-list)) keywords)
    
  ;)
;)

(define (vector-id-keywords-by-group-non-filtered statement keywords)
  (let ((id-list (build-list (length statement) values))
        (str-vector (build-vector (vector-length keywords) values))) ; список индексов в предложении
        (vector-map (lambda (y) (map (lambda (n) (vector-member (list-ref statement n) (vector-ref y 0))) id-list)) keywords)
    
  )
)

(define (vector-id-keywords-by-group statement keywords)
  (let ((id-list (build-list (length statement) values))
        (str-vector (build-vector (vector-length keywords) values))) ; список индексов в предложении
        (matrix-without-#f (vector-map (lambda (y) (map (lambda (n) (vector-member (list-ref statement n) (vector-ref y 0))) id-list)) keywords))
    
  )
)
;(define (test3 matrix)
  ;(vector-map (lambda (y) (map (lambda (n) (eq? (list-ref y n) #t) (cons '() n)) (build-list (length y) values))) matrix))
 ; (vector-map (lambda (y) (map (lambda (n) (println (list-ref y n))) (build-list (length y) values))) matrix))

; вспомогательная функция для фильтра
(define (true? x)
  (not (equal? x #f)))

; вспомогательная функция, строящая вектор из списков из исходных векторов матрицы, исключая все #t
(define (matrix-without-#f matrix)
  (vector-map (lambda (n) (filter true? n)) matrix))

;(vector-id-keywords-by-group-non-filtered '(i uncle am mother scheme suicide) keywords-structure)
;(vector-id-keywords-by-group '(i uncle am mother scheme suicide) keywords-structure)

; вспомогательная функция, строящая вектор из длин его подсписков (ключевых слов, разбитых по группам)
(define (vec-sum-keywords-by-group veclst)
  (vector-map (lambda (n) (length n)) veclst))

(define (sum-vec vec)
  ;(vector-foldl + 0 vec)) не работает
  (let helper ((sum 0) (id 0))
    (if (= id (vector-length vec)) sum
        (helper (+ sum (vector-ref vec id)) (+ id 1)))))

(define (group-keywords-probability vec)
  (let ((sum (sum-vec vec)))
    (vector-map (lambda (n) (if (= sum 0) sum (/ n sum))) vec)))
  ;(vector-map (lambda (n) (/ n (if (= (sum-vec vec) 0) 1)) vec))

(define (keywords-answer user-response keywords-structure)
  (vector-id-keywords-by-group user-response keywords-structure))

(define (reply-v3 user-response history keywords-structure keywords)
  (let ((choices-num (random 0 (number-of-choices user-response history keywords)))
        (history-strategy-num (if (null? history) 3 2)) ; ставим последним номером, если невозможно применить
        (keywords-strategy-num (if (have-keywords? user-response keywords)
                                   (if (null? history) 2 3) 3)); учитываем случай, когда и стратегия с историей применима, и с ключевыми словами
       )
       (cond ((= choices-num 0) (println '(hedge-answer)))
             ((= choices-num 1) (println '(qualifier-answer user-response)))
             ((= choices-num history-strategy-num) (println '(history-answer history)))
             ((= choices-num keywords-strategy-num) (keywords-answer user-response keywords-structure))
       )
  )
)



;тесты для выбора стратегии
(reply-v3 '(i have met with my uncle) '() keywords-structure keywords)
;(reply-v3 '(i have met with my uncle) '(history) keywords)
;(reply-v3 '(i have met with my friend) '(history) keywords)
;(reply-v3 '(i have met with my friend) '() keywords)