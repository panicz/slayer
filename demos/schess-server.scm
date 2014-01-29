#!/usr/bin/guile \
-L ./guile-modules -L ../guile-modules -L . -e main -s
!#

(set-port-encoding! (current-input-port) "UTF-8")
(set-port-encoding! (current-output-port) "UTF-8")
(set-port-encoding! (current-error-port) "UTF-8")
(fluid-set! %default-port-encoding "UTF-8")

(use-modules (extra common) (extra ref) (extra network)
	     (ice-9 threads) (oop goops) (schess network-server))

;; a na dobry sen: krótki opis tego, jak powinien działać serwer (w wersji
;; uproszczonej, tj. klient = gracz):
;; 1. wczytujemy plik z regułami
;; 2. sprawdzamy, ilu klientów
;; 3. czytamy z gniazda. jeżeli pojawia się nowy klient, to go dodajemy
;;    do listy bieżących klientów i generujemy dla niego protokół
;; 4. w przeciwnym razie obsługujemy go należycie (zgodnie z protokołem)

;; przykładowy scenariusz:
;; 1. łączy się jeden gracz
;; 2 .łączy się drugi gracz
;; 3. pierwszy gracz pyta o dostępne ruchy dla pionka. w odpowiedzi
;;    dostaje określoną listę
;; 4. gracz wybiera jeden z ruchów i wysyła na serwer.
;; 5. serwer rozgłasza wszystkim graczom nowy stan planszy
;;    oraz ogólnie turę i informację o bieżącym graczu
;; 6. drugi gracz pyta o dostępne ruchy i wybiera ...

(define (main (program-name args ...))
  (let ((server (make <board-network-server> #:rule-book "chess.ss")))
    (start-gameplay server)
    (handle-clients! server)))
