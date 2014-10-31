;#| -*-Scheme-*-
;
;$Id: copyright.scm,v 1.4 2005/12/13 06:41:00 cph Exp $
;
;Copyright 2005 Massachusetts Institute of Technology
;
;This file is part of MIT/GNU Scheme.
;
;MIT/GNU Scheme is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 2 of the License, or (at
;your option) any later version.
;
;MIT/GNU Scheme is distributed in the hope that it will be useful, but
;WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with MIT/GNU Scheme; if not, write to the Free Software
;Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;USA.
;
;|#

;;;; SI units
(eval-when (compile load eval)
  (set-current-module generic-environment))

(define-unit-system 'SI
  (list 'meter "m" "length")
  (list 'kilogram "kg" "mass")
  (list 'second "s" "time")
  (list 'ampere "A" "electric current")
  (list 'kelvin "K" "temperature")
  (list 'mole "mol" "amount of substance")
  (list 'candela "cd" "luminous intensity")
;  #|
;  (list 'radian "rad" "plane-angle")
;  (list 'steradian "sr" "solid angle")
;  |#
  )
;#|
;;;; Used in with-units.scm
;(set! *angular* (unit-exponents radian))
;|#

(define-derived-unit SI 'radian 
  "rad" "plane-angle" unitless)


;;; Derived SI units

(define-derived-unit SI 'newton
  "N" "force" (/ (* kilogram meter) (square second)))

(define-derived-unit SI 'joule
  "J" "energy" (* newton meter))


(define-derived-unit SI 'coulomb
  "C" "electric charge" (* ampere second))


(define-derived-unit SI 'watt
  "W" "power" (/ joule second))

(define-derived-unit SI 'volt
  "V" "electric potential" (/ watt ampere))

(define-derived-unit SI 'ohm
  "\\Omega" "electric resistance" (/ volt ampere))

(define-derived-unit SI 'siemens
  "S" "electric conductance" (/ unitless ohm))

(define-derived-unit SI 'farad
  "F" "capacitance" (/ coulomb volt))


(define-derived-unit SI 'weber
  "Wb" "magnetic flux" (* volt second))

(define-derived-unit SI 'henry
  "H" "inductance" (/ weber ampere))

(define-derived-unit SI 'hertz
  "Hz" "frequency" (/ unitless second))

(define-derived-unit SI 'tesla
  "T" "magnetic flux density" (/ weber (square meter)))

(define-derived-unit SI 'pascal
  "Pa" "pressure" (/ newton (square meter)))

;#|
;(define-derived-unit SI 'lumen
;  "lm" "luminous flux" (* candela steradian))
;
;
;(define-derived-unit SI 'lux
;  "lx" "illuminance" (/ lumen (square meter)))
;|#

(define-derived-unit SI 'katal
  "kat" "catalytic activity" (/ mole second))

(define-derived-unit SI 'becquerel
  "Bq" "activity" (/ unitless second))

(define-derived-unit SI 'gray
  "Gy" "absorbed dose" (/ joule kilogram))

(define-derived-unit SI 'sievert
  "Sv" "dose equivalent" (/ joule kilogram))

;;;; SI multipliers

(define-multiplier 'exa "E" 1e18)

(define-multiplier 'peta "P" 1e15)

(define-multiplier 'tera "T" 1e12)

(define-multiplier 'giga "G" 1e9)

(define-multiplier 'mega "M" 1e6)

(define-multiplier 'kilo "k" 1e3)

(define-multiplier 'hecto "h" 1e2)

(define-multiplier 'deka "da" 1e1)

(define-multiplier 'deci "d" 1e-1)

(define-multiplier 'centi "c" 1e-2)

(define-multiplier 'milli "m" 1e-3)

(define-multiplier 'micro "\\mu" 1e-6)

(define-multiplier 'nano "n" 1e-9)

(define-multiplier 'pico "p" 1e-12)

(define-multiplier 'femto "f" 1e-15)

(define-multiplier 'atto "a" 1e-18)

;;; Other units in terms of SI system

(define-additional-unit SI 'degree
  "$^\\circ$" "1/360 circle" angular (/ :2pi 360))

(define-additional-unit SI 'inch
  "in" "English length" meter (* 2.54 centi))

(define-additional-unit SI 'pound
  "lb" "English force" newton 4.4482)

(define-additional-unit SI 'slug
  "slug" "English mass" kilogram 14.594)


(define-additional-unit SI 'foot
  "ft" "English length" inch 12)

(define-additional-unit SI 'mile
  "mi" "English length" foot 5280)


(define-additional-unit SI 'dyne
  "dyne" "Force" newton 1.0e-5)

(define-additional-unit SI 'calorie	;at 20 C
  "cal" "Heat energy" joule 4.1819)

(define-additional-unit SI 'minute
  "day" "Time" second 60)

(define-additional-unit SI 'hour
  "day" "Time" second 3600)

(define-additional-unit SI 'day
  "day" "Time" second 86400)

(define-additional-unit SI 'year
  "yr" "Tropical year 1900"  second 31556925.9747)

(define-additional-unit SI 'sidereal-year
  "syr" "Sidereal year 1900"  second 3.1558149984e7)


(define-additional-unit SI 'AU
  "AU" "Astronomical Unit" meter 1.4959787066e11)

(define-additional-unit SI 'arcsec
  "arcsec" "arc second" radian (/ (* 2 :pi) (* 60 60 360)))


(define parsec
  (/ (& 1 AU) (tan (& 1 arcsec))))

(define-additional-unit SI 'pc
  "pc" "Parsec" meter (u:value parsec))


(define speed-of-light 2.99792458e8)
(define light-year (& speed-of-light year))

(define-additional-unit SI 'ly
  "ly" "Light Year" meter (u:value light-year))

(define-additional-unit SI 'esu
  "esu" "Electrostatic Unit" coulomb
  (/ 1 (* 10 speed-of-light)))

(define-additional-unit SI 'ev
  "ev" "Electron Volt" joule 1.602e-19)
