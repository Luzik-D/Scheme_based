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
      (else (print (reply-v4 user-response history))
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
  (let new-visitor ((i limit-people) (name (ask-patient-name)))
        (cond ((or (equal? stop-word name) (= i 0)) (println '(time to go home))) ; выход из цикла по имени, либо 0-м количестве пациентов
              ((begin
                (printf "Hello, ~a!\n" name) ; приветствие для каждого нового пациента
                (print '(what seems to be the trouble?))
                (doctor-driver-loop-v2 name '())
                (if (= i 1) (printf "\nTime to go home\n") ; выход после последнего пациента
                    (new-visitor stop-word (- i 1) (ask-patient-name))
                )
              ))
        )
  )
)


;----------------------------------ex6---------------------------------------;

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
(define keywords (vector-foldl (lambda (i res el) (vector-append res (vector-ref el 0))) (vector) keywords-structure))

; функция выдает все ответы, связанные с ключевым словом keyword
(define (get-templates keyword)
  (vector-foldl (lambda (i res el) (vector-append res (vector-ref el 1)))
                (vector)
                (vector-filter (lambda (n) (vector-member keyword (vector-ref n 0))) keywords-structure)
  )
) 
                   
; проверка на наличие хотя бы одного ключевого слова в предложении
(define (have-keywords? statement)
   (ormap (lambda (el) (vector-member el keywords)) statement))


; стратегия ответа по ключевым словам
(define (keywords-answer user-response)
  (let ((keyword (pick-random-list (filter (lambda (el) (vector-member el keywords)) user-response))))
       (many-replace-v3 (list (cons '* (list keyword))) (pick-random-vector (get-templates keyword)))))

; генерация ответной реплики, учитывающая стратегии по ключевым словам и истории
(define (reply-v3 user-response history)
  (let ((top-limit (if (have-keywords? user-response) 4 3)) ; верхняя граница, если стратегия доступна, то будет достижим 3 вариант case
        (bot-limit (if (not (null? history)) 0 1))) ; нижняя граница, если стратегия доступна, то будет достижим 0 вариант case
    (case (random bot-limit top-limit)
      ((0) (history-answer history))
      ((1) (hedge-answer))
      ((2) (qualifier-answer user-response))
      ((3) (keywords-answer user-response))
    )
  )
)

; тесты для проверки генерации ответов
;(reply-v3 '(learning scheme) '((holidays))) ; любая стратегия
;(reply-v3 '(learning scheme) '()) ; все стратегии кроме истории
;(reply-v3 '(learning) '()) ; все кроме ключевых слов и истории


;--------------------------step 3-------------------------;

; структура для хранения стратегий -- список векторов, где:
; vector-ref 0 -- вес
; vector-ref 1 -- функция, определяющая возможность использования стратегии
; vector-ref 2 -- функция, реализующая стратегию
(define strats (list (vector 1 (lambda (user-response history) #t) (lambda (user-response history) (hedge-answer)))
                     (vector 2 (lambda (user-response history) #t) (lambda (user-response history) (qualifier-answer user-response)))
                     (vector 3 (lambda (user-response history) (not (null? history))) (lambda (user-response history) (history-answer history)))
                     (vector 3 (lambda (user-response history) (have-keywords? user-response)) (lambda (user-response history) (keywords-answer user-response)))
               )
)

; случайный выбор стратегии с учетом веса
(define (pick-random-list-with-weight lst)
  (let ((weight (random (foldl (lambda (x y) (+ y (vector-ref x 0))) 0 lst))))
    (call/cc
     (lambda (cc-exit)
       (foldl (lambda (x y) (if (> (vector-ref x 0) y)
                                (cc-exit x)
                                (- y (vector-ref x 0))
                            )
              )
              weight
              lst
       )
     )
    )
  )
)

; генерация реплики
; выбирается случайная реплика из доступных с учетом веса
(define (reply-v4 user-response history)
  ((vector-ref (pick-random-list-with-weight (filter (lambda (s) ((vector-ref s 1) user-response history)) strats)) 2) user-response history)
)

