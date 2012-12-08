(define-module (extra network-objects)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-2)
  :use-module (srfi srfi-11)
  :use-module (oop goops)
  :use-module (ice-9 match)
  :use-module (ice-9 optargs)
  :use-module (ice-9 regex)
  ;:use-module (extra slayer)
  :use-module (extra ref)
  :use-module (extra common)
  :use-module (extra network)
  :use-module ((rnrs) :version (6))
  :export (
	   objects-visible-to
	   <unique-id>
	   <registered-object>
	   <network-object>
	   <network-client>
	   request
	   command
	   state-of
	   ))

(read-hash-extend 
 #\<
 (lambda (char port)
   (let* ((chars->data (lambda(l)
			 (let ((s (list->string (reverse l))))
			   (cond 
			    ((string-match "^[0-9a-fA-F]+$" s)
			     (string->number (string-append "#x" s)))
			    (else
			     (string->symbol s))))))
	  (return 
	   (lambda (tokens current-token)
	     (reverse
	      (if (null? current-token)
		  tokens
		  (cons (chars->data current-token) tokens))))))
     (let loop ((level 0)
		(current-token '())
		(tokens '()))
       (let ((char (read-char port)))
	 (cond ((eof-object? char)
		(return tokens current-token))
	       ((char-whitespace? char)
		(if (null? current-token)
		    (loop level current-token tokens)
		    (loop level '() (cons (chars->data current-token)
					  tokens))))
	       ((equal? char #\<)
		(loop (1+ level) (cons char current-token) tokens))
	       ((equal? char #\>)
		(if (= level 0)
		    (return tokens current-token)
		    (loop (1- level) (cons char current-token)
			  tokens)))
	       (else
		(loop level (cons char current-token) tokens))))))))

"
Trzeba rozwazyc nastepujacych kilka kwestii zwiazanych z architektura
 swiata:
- czy po stronie klienta podzial na portale i sektory ma zostac 
zachowany?
jezeli nie, to implikuje, ze kazdy klient postrzega swiat ze swojego
 punktu widzenia; problematyczne wowczas staje sie posiadanie przez
 klienta wiecej niz jednego postrzegajacego agenta, poniewaz gdyby
 tak mialo w istocie byc, to ten sam postrzegany obiekt musialby miec
 rozne wspolrzedne.

Ewentualnie mozna wtedy pomyslec jeden obiekt glowny, i ze z
 pozostalymi obiektami sa skorelowane przeksztalcenia (obrot i
 przesuniecie), ktore sa wymagane, zeby przetransformowac do punktu
 widzenia glownego obiektu (czy cos)

- czy cos stoi na przeszkodzie temu, zeby portal byl jednoczesnie 
obiektem i podprzestrzenia? a rozwazmy moze male okienko bedace
 portalem. z punktu widzenia wyswietlania chcemy, zeby widoczne bylo
 tylko to, co znajduje sie w okienku (co normalnie jest robione
 generowaniem tekstur, ale mozna to dorobic, wycinajac plaszczyzny)

Pomysl wydaje sie o tyle niefortunny, ze w pierwszej gesi portal
 stanowil granice dla sektora -- dlatego nie bylo przypadkiem, ze
jedynym dopuszczalnym ksztaltem granic sektora byla plaszczyzna.
Z drugiej strony, tego rodzaju generalizacja raczej nic nie daje.
Oczywiscie, mozna tworzyc obiekty, ktore teleportuja, ale to nie
jest to samo, co portale! Dlatego roznice pomiedzy nowa gesia a stara
powinny byc minimalne w aspekcie pojeciowym. (Za to w aspekcie
implementacyjnym oczywiscie jezyk scheme daje duzo wieksze mozliwosci)

Czyli decyzje ostateczne:
- portale i sektory sa zaimplementowane wylacznie po stronie serwera,
przy czym klient posiada obiekt glowny, wzgledem ktorego mierzone sa
polozenia wszystkich innych obiektow. Zgadzaloby sie to z
powszechnie znana formula, ze 'czlowiek jest miara wszech-rzeczy'.
Z drugiej strony, argumentem przeciwko takiemu stawianiu sprawy jest
to, ze dla kazdego klienta serwer bedzie musial wykonac szereg
obliczen.

Moze zatem lepiej jest zachowac podzial na sektory rowniez po stronie
klienta, aby zminimalizowac rozbieznosc miedzy klientem a serwerem
-- dzieki temu nie bedzie potrzeby wyrozniania zadnego obiektu,
a jesli klient zechce byc miara wszech rzeczy, to sam sie o wszystko
zatroszczy.

A w starej gesi bylo to tak:
- byla sobie lista sektorow oraz lista portali, a kazdy z sektorow
lub portali zawieral swoje obiekty

Jest jeszcze jedna decyzja architektoniczna, ktora trzeba podjac,
mianowicie: czy sektory i portale reprezentowac jako obiekty goose,
czy jako cos koncepcyjnie odrebnego?

Chodzi o to, ze tak jak do tej pory mielismy akcje pojawienia sie
i znikniecia obiektu, a takze zmiany jego stanu, tak teraz bedziemy
musieli rozwazyc pojawienie sie i znikniecie obiektu w danym sektorze.

Z punktu widzenia implementacji roznica jest taka, jezeli sektory
i portale tez beda obiektami goose, to po prostu bedziemy mieli do
czynienia ze zmiana ich stanu -- zmieniac sie bedzie konkretnie
zawartosc listy.

Z cala pewnoscia nie warto jednak relatywizowac polozenia wszystkich
obiektow do jednego obiektu, poniewaz wowczas -- oprocz tego, ze
serwer bedzie musial wykonac niepotrzebnie wiele obliczen -- 
kazdy ruch klienta bedzie powodowal zmiane polozenia wszystkich
obiektow. Nonsens!

Dlatego moze raczej nalezaloby rozwiazac sprawe w nastepujacy sposob:
Kazdy sektor sklada sie z obiektow, przy czym portal rowniez jest
szczegolnego rodzaju obiektem. 

A czy sektor jest? Nie, sektor nie jest. Po prostu widoczne obiekty
maja swoj context odpowiednio okreslony. Wowczas po prostu na
podstawie kontekstu okreslamy, ktore przedmioty przynaleza do ktorych
sektorow. Kiedy natomiast tracimy pewien portal z oczu, tracimy tez
z oczu wszystkie obiekty, ktore naleza do tego portalu.

W ten sposob struktura swiata pozostaje dynamiczna.

Jednakze, w jaki sposob jest (ma byc) nasz swiat zorganizowany?
Oczywiscie, swiat sam w sobie sklada sie z sektorow, ktore sa
polaczone portalami. Zatem wyswietlanie obiektow wyglada tak, ze
wyswietlamy wszystkie normalne obiekty, a jezeli obiekt jest portalem,
to wnikamy w niego (transformujemy), wyswietlamy wszystkie obiekty 
wewnatrz, wnikamy na druga strone, wyswietlamy wszystkie obiekty
po drugiej stronie, i sie cofamy

"


(define-class <unique-id> ()
  (id #:init-value 0))

(define-method (initialize (this <unique-id>) args)
  (next-method)
  (match-let (((type ... id)
	       (with-input-from-string 
		   (with-output-to-string (\ display this)) read)))
    (set! #[this 'id] id)))

(define-class <registered-object> (<unique-id>)
  (registry #:init-value (make-hash-table)
	    #:allocation #:class))

(define-method (initialize (this <registered-object>) args)
  (next-method)
  (set! #[#[this 'registry] #[this 'id]] this))

(define-generic remove!)

(define-method (remove! (object <registered-object>))
  (hash-remove! #[object 'registry] #[object 'id]))

(define-class <network-object> (<registered-object>)
  (owners #:init-value #f #:init-keyword #:owners)
  (context #:init-value #f) ; the subspace to which it belongs
  (private-slots #:init-value '() #:allocation #:each-subclass)
  (client-slots #:init-value '() #:allocation #:each-subclass))

(define* (state-of object #:optional (owner #t))
  (map (lambda (slot) (list slot #[object slot]))
       (lset-difference equal?
			(map first (class-slots (class-of object)))
			(map first (class-slots <network-object>))
			#[object 'client-slots]
			(if owner
			    '()
			    #[object 'private-slots]))))

(define-method (objects-visible-to object)
  (list object))

(define-class <network-client> ()
  (socket.address #:init-value #f)
  #;(mutex #:init-thunk make-mutex)
  (protocol #:init-thunk make-hash-table)
  (type-hash #:init-thunk make-hash-table)
  (receive #:init-value noop) ; this is a procedure called
  ;; when a packet is received
  ;; type-hash contains a hash whose keys are type-names (symbols)
  ;; and values are GOOPS types, thus making it closer
  ;; 
  ;; the keywords the class is meant to be
  ;; initialized with:
  ;;   #:address "nu.mb.e.r:port"
  ;;   #:types '((typename type) ...)
  ;;   #:username "name"
  ;;   #:password "phrase"
  )

(define-method (request (gate <network-client>) content handler)
  (match-let (((socket . address) #[gate 'socket.address])
	      (requests #[#[gate 'protocol] 'requests])
	      (request-id (gensym "r-")))
    (set! #[requests request-id] handler)
    (sendto socket (with-output-to-utf8 
		    (\ display `(request ,request-id ,content)))
	    address)))

(define-method (command (gate <network-client>) content)
  (match-let (((socket . address) #[gate 'socket.address]))
    (sendto socket (with-output-to-utf8
		    (\ display content))
	    address)))
