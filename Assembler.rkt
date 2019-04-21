#lang racket

(provide 
         entry entry? entry-key entry-value
         symbol-table
         bin->n sm->n n->bin n->sm
         assemble-one assemble
         ram-read ram-write equal-rams?
         conf conf? conf-cpu conf-ram
         cpu cpu? cpu-acc cpu-pc cpu-rf cpu-aeb
         equal-configs? addr->pc incr-pc 
         acc->mem mem->acc
         sum diff
         do-input do-output
         next-config
         init-config simulate run
         prog-greater prog-list)

;TC-201 assembler and simulator,


; A table is a list of entries, where each entry has two fields: key and value.
(struct entry (key value) #:transparent)

;************************************************************
; a TC-201 assembler is
; a procedure that takes a symbolic assembly language
; program as input and returns as output the corresponding
; list of 16-bit words representing TC-201 instructions and data.
=

(define prog-sum
  '((start:  load constant-0)
   (         store sum)
   (next:    input)
   (         skipzero)
   (         jump add-num)
   (         load sum)
   (         output)
   (         halt)
   (add-num: add sum)
   (         store sum)
   (         jump next)
   (sum:     data 0)
   (constant-0: data 0)))

; Here is the result of assembling this program

;> (assemble prog-sum)
;'((0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0)
;  (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; Here are two simpler test programs.
; First, a program with only instructions, 
; numeric addresses, and no labels.

(define prog1
  '((load 3)
    (store 4)
    (halt)))

; Second, a program with only data statements, three labels, and both numeric
; and symbolic data values

(define prog2
  '((x: data 1)
    (y: data -2)
    (z: data y)))

; Here are the values returned by assemble on these two programs.

;> (assemble prog1)
;'((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;> (assemble prog2)
;'((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

; Note the sign/magnitude representation of -2, and
; the fact that the label y translates to the number 1
; in this example.

;************************************************************
; A symbolic assembly-language program is a list of lists, with
; each list representing one instruction or data statement.
; An instruction or data statement may optionally have a label,
; that is, a symbol ending in colon (:), which is the first
; element of the list.  The next symbol must be one of the
; opcodes (in the table opcode-table) or the symbol 'data.

; For the opcodes load, store, add, sub, jump, loadi, storei,
; there is one more field, the address field, which may be 
; a label (defined somewhere in the program) or a 
; decimal number between 0 and 4095 inclusive.
; For the other opcodes, there is no additional field.
; For the data directive, there is one more field, the
; value, which may be a label (defined somewhere in the
; program) or a decimal number between -32767 and 32767
; inclusive.

;************************************************************
; (symbol-table prog) takes a symbolic assembly-language program prog
; as input, and returns a table with entries listing
; (in order) the labels defined in the program and their
; corresponding numeric values (instructions and data
; statements are numbered from 0.)

; Examples
;> (symbol-table prog-sum)
;(list
; (entry 'start 0)
; (entry 'next 2)
; (entry 'add-num 8)
; (entry 'sum 11)
; (entry 'constant-0 12))

;> (symbol-table prog2)
;'()

;> (symbol-table prog3)
;(list (entry 'x 0) (entry 'y 1) (entry 'z 2))

;************************************************************

(define (symbol-table prog)
  (symb prog 0))

(define (symb lst n)
  (if (not (empty? lst))
      (let ((fl (substring (symbol->string (first (first lst))) (- (string-length (symbol->string (first (first lst)))) 1) (string-length (symbol->string (first (first lst)))))))
        (if (equal? fl ":")
            (cons (symbol-table-helper (first lst) n) (symb (rest lst) (+ n 1)))
            (symb (rest lst) (+ n 1))))
        '()))


(define (symbol-table-helper lst n) ; given a single command lst and n creates an entry
  (let* ((sym (first lst)) (val (string->symbol (substring (symbol->string sym) 0 (- (string-length (symbol->string sym)) 1)))))
    (entry val n)))

;************************************************************
; (bin->n lst)
; takes a list of binary digits and returns the nonnegative
; integer that they represent in unsigned binary in base 2.

; (sm->n lst)
; takes a list of k binary digits and returns the negative, zero, or
; positive number that they represent in k-bit sign/magnitude representation.
; You may assume k is at least 2.

; (n->bin n len)
; takes a nonnegative integer n and returns a list of len binary digits
; representing n in unsigned binary.  If necessary, the representation 
; is padded on the left with 0's, or truncated on the left, so that the 
; resulting list has length len.

; (n->sm n len)
; If the negative,
;> (bin->n '(0 0 1 1))
;3
;> (bin->n '(1 1 1 1 1))
;31
;> (sm->n '(0 0 1 1))
;3
;> (sm->n '(1 0 1 1))
;-3
;> (n->bin 13 5)
;'(0 1 1 0 1)
;> (n->bin 13 3)
;'(1 0 1)
;> (n->sm 13 16)
;'(0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1)
;> (n->sm 17 4)
;'(0 0 0 0)
;************************************************************
(define (calc lst n) ; n should start as 1. given a binary lst will calculate the value
  (if (empty? lst)
      0
      (+ (* n (last lst)) (calc (take lst (- (length lst) 1)) (* n 2)))))
  
(define (bin->n lst)
  (calc lst 1))

(define (sm->n lst)
  (if (= (first lst) 0)
      (calc (rest lst) 1)
      (* -1 (calc (rest lst) 1))))

(define (dec2bin num) ; gives a binary list for num
  (if (> num 0)
      (append (dec2bin (floor (/ num 2))) (list (remainder num 2)))
      '(0)))

(define (zeros n) ; returns a list of zeros of length n
  (if (> n 0)
      (cons 0 (zeros (- n 1)))
      '()))

(define (sublist lst start end n) ; like substring but for lists, set n to zero for indexing like substring (first element is 0)
  (cond
    [(< n start)
     (sublist (rest lst) start end (+ n 1))]
    [(and (>= n start) (<= n end))
     (cons (first lst) (sublist (rest lst) start end (+ n 1)))]
    [else
     '()]))
     
(define (n->bin n len)
  (let ((bin (dec2bin (abs n))))
    (if (< (length bin) len)
        (append (zeros (- len (length bin))) bin)
        (sublist bin (+ 1 (- (length bin) len)) (length bin) 1))))
      
(define (n->sm n len)
  (let ((bin (rest (dec2bin n))))
    (cond
      [(>= len (+ 1 (length bin))) ;it fits
       (if (>= n 0) ;n is positive
           (append '(0) (rest (n->bin n len)))
           (append '(1) (rest (n->bin n len))))]
      [else
       (zeros len)])))

;************************************************************
; Now we create a procedure to assemble one
; line of a program (given the symbol table), and use that
; to assemble the whole program.

;************************************************************

; (assemble-one line table)
; takes one line (instruction or data statement) from a program
; and a symbol table for the program
; and returns a list of 16 bits representing that line of the program.

; (assemble prog)
; takes a symbolic assembly-language program prog and returns
; a list of 16-bit lists, one for each line of the program, giving
; the machine language version of the program.

;************************************************************
; Here is a useful table of the TC-201 opcodes and their
; corresponding 4-bit representations.

(define opcode-table
  (list
   (entry 'halt '(0 0 0 0))
   (entry 'load '(0 0 0 1))
   (entry 'store '(0 0 1 0))
   (entry 'add '(0 0 1 1))
   (entry 'sub '(0 1 0 0))
   (entry 'input '(0 1 0 1))
   (entry 'output '(0 1 1 0))
   (entry 'jump '(0 1 1 1))
   (entry 'skipzero '(1 0 0 0))
   (entry 'skippos '(1 0 0 1))
   (entry 'skiperr '(1 0 1 0))
   (entry 'loadi '(1 0 1 1))
   (entry 'storei '(1 1 0 0))))
;************************************************************

(define (key->val lst key); given a key will search through a list of entries and return the value that corresponds to the key
  (if (empty? lst)
      "A key that isn't in the list of entries was given"
      (if (equal? (entry-key (first lst)) key)
          (entry-value (first lst))
          (key->val (rest lst) key))))

(define (assemble-one line table)
  (cond
    [(= 3 (length line)) ; must have the first thing: then a operation then a value \
     (if (not (equal? 'data (second line)))
        (if (number? (third line))
            (append (key->val opcode-table (second line)) (n->sm (third line) 12))
            (append (key->val opcode-table (second line)) (n->sm (key->val table (third line)) 12)))
        (if (number? (third line))
            (n->sm (third line) 16)
            (n->sm (key->val table (third line)) 16)))]
    [(equal? (substring (symbol->string (first line)) (- (string-length (symbol->string (first line))) 1) (string-length (symbol->string (first line))))
             ":") ; this means there will only be the operation and no value
     (append (key->val opcode-table (second line)) (zeros 12))]
    [(= 2 (length line)) ; line has a command and then either a symbol or a number
     (if (not (equal? 'data (first line)))
        (if (number? (second line))
            (append (key->val opcode-table (first line)) (n->sm (second line) 12))
            (append (key->val opcode-table (first line)) (n->sm (key->val table (second line)) 12)))
        (if (number? (second line))
            (n->sm (second line) 16)
            (n->sm (key->val table (third line)) 16)))]
    [else ; the final case means there's just a command chilling alone
     (append (key->val opcode-table (first line)) (zeros 12))]))
           
    ; (append (key->val opcode-table (second line))

(define (assemble prog)
  (assemble-helper prog prog))

(define (assemble-helper prog progs)
  (if (not (empty? prog))
      (cons (assemble-one (first prog) (symbol-table progs)) (assemble-helper (rest prog) progs))
      '()))
      
 
;************************************************************
; Now that we can produce machine language from symbolic assembly-language
; programs, we'll create a simulator that can execute the machine
; language instructions step by step.  First, we specify a representation of
; of the random access memory (RAM) and procedures to read
; and write it.

;************************************************************
; Random access memory (RAM)

; The contents of RAM are represented by a table
; in which the key is a nonnegative integer in the range
; 0 through 4095 (the memory address), and the value is a list of 16 bits
; (the bits stored by the register with that address.)
; No address may appear twice.  The contents of any register
; whose address does not appear as a key is assumed to contain 16 zeroes.

; Examples of RAMs.

;************************************************************


; (ram-read address ram)
; takes a memory address and a ram
; and returns a list of 16 bits giving the contents
; of the memory register in ram with the given address.

; (ram-write address contents ram)
; takes a memory address (address), a list of 16 bits (contents) and a ram,
; and returns a ram representing the result of copying the contents 
; into the memory register of ram specified by the memory address.

; (equal-rams? ram1 ram2)
; takes two rams and compares their contents, returning
; #t if they are equal and #f if they are unequal.

; Examples
(define ram1
  (list
   (entry 0 '(0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1))
   (entry 1 '(0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0))
   (entry 2 '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 3 '(1 1 1 1  0 0 0 0  1 1 1 1  0 0 0 0))
   (entry 4 '(0 0 0 1  0 0 1 1  0 1 1 1  1 1 1 1))))

(define ram2
  (list
   (entry 1 '(0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0))
   (entry 4 '(0 0 0 1  0 0 1 1  0 1 1 1  1 1 1 1))
   (entry 0 '(0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1))
   (entry 3 '(1 1 1 1  0 0 0 0  1 1 1 1  0 0 0 0))))

(define ram3
  (list
   (entry 0 '(1 0 1 1  0 0 0 0  0 0 0 0  0 1 0 0))
   (entry 1 '(1 1 0 0  0 0 0 0  0 0 0 0  0 1 0 1))
   (entry 2 '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 4 '(0 0 0 0  0 0 0 0  0 0 0 0  0 1 1 1))
   (entry 5 '(0 0 0 0  0 0 0 0  0 0 0 0  1 0 1 0))
   (entry 7 '(1 1 1 1  1 1 1 1  0 0 0 0  0 0 0 0))
   (entry 10 '(1 0 1 0  0 0 0 0  0 1 0 1  1 1 1 1))))

;> (ram-read 0 ram1)
;'(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;> (ram-read 2 ram2)
;'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;> (ram-write 5 '(1 1 0 0  0 0 1 1  1 1 0 0  0 0 1 1) ram2)
;(list
; (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
; (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
; (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
; (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
; (entry 5 '(1 1 0 0 0 0 1 1 1 1 0 0 0 0 1 1)))
;> (ram-write 10 '(1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1) ram3)
;(list
; (entry 0 '(1 0 1 1 0 0 0 0 0 0 0 0 0 1 0 0))
; (entry 1 '(1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1))
; (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
; (entry 5 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
; (entry 7 '(1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0))
; (entry 10 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
;> (equal-rams? ram1 ram2)
;#t
;> (equal-rams? ram2 ram3)
;#f

;************************************************************

(define (ram-read address ram)
  (if (not (equal? (key->val ram address) "A key that isn't in the list of entries was given")) 
      (key->val ram address)
      (zeros 16)))

(define (ram-write address contents ram)
  (if (not (empty? ram))
      (if (equal? (entry-key (first ram)) address)
          (cons (entry address contents) (rest ram))
          (cons (first ram) (ram-write address contents (rest ram))))
      (list (entry address contents))))

(define (equal-rams? ram1 ram2)
  (if (and (eqhr? ram1 ram2) (eqhr? ram2 ram1))
      #t
      #f))

(define (eqhr? ram1 ram2)
  (if (empty? ram1)
      #t
      (if (or (not (equal? (length ram2) (length (remove* (list (first ram1)) ram2)))) (equal? (zeros 16) (entry-value (first ram1))))
          (eqhr? (rest ram1) ram2)
          #f)))
          

;************************************************************
; For the TC-201 Central Processing Unit (CPU), 
; the contents of the registers are represented by a struct with 4 fields 
; giving the values of the CPU registers:

; the accumulator (acc)
; the program counter (pc)
; the run flag (rf)
; the arithmetic error bit (aeb)

(struct cpu (acc pc rf aeb) #:transparent)

; Each field contains a list of bits of the correct length
; giving the value of the corresponding register; 16 bits for
; the acc, 12 bits for the pc, 1 bit each for the rf and the aeb.
; The constructor is cpu, the type predicate is cpu?, and
; the selectors are cpu-acc, cpu-pc, cpu-rf, cpu-aeb.

; Examples

(define cpu1 
  (cpu
   '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
   '(0 0 0 0 0 0 0 0 0 1 1 1)
   '(1)
   '(0)))

(define cpu2 
  (cpu
   '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
   '(0 0 0 0 0 0 0 0 0 1 1 1)
   '(1)
   '(1)))

;************************************************************
; A configuration of the TC-201 is a struct with two fields:
; (1) the contents of the CPU registers, in the above format, and
; (2) the contents of the RAM, in the format of problem 4.

(struct conf (cpu ram) #:transparent)

; Note that the constructor is conf, the type-predicate
; is conf?, and the selectors are conf-cpu, conf-ram.

; Examples

;************************************************************

; (equal-configs? config1 config2)
; takes two configurations config1 and config2, and returns
; #t if they represent the same contents of the RAM and the CPU registers,
; and returns #f otherwise.

; (addr->pc addr config)
; takes a configuration and a memory address addr (a number
; in the range 0 to 4095 inclusive), and returns a new configuration
; in which the program counter is set to the given address.
; No other registers are changed.

; (incr-pc n config)
; takes a nonnegative integer n and a TC-201 configuration config
; and returns the TC-201 configuration that is obtained by adding n 
; to the value of pc.  Note that the sum should be taken modulo 4096.  
; (Racket has a modulo procedure.)

; Examples

(define config1
  (conf cpu1 ram1))

(define config2
  (conf cpu1 ram2))

(define config3
  (conf cpu2 ram2))

;> (equal-configs? config1 config2)
;#t
;> (equal-configs? config2 config3)
;#f
;> (addr->pc 5 config1)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 0 1) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))
;> (addr->pc 1 config3)
;(conf
; (cpu '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) '(0 0 0 0 0 0 0 0 0 0 0 1) '(1) '(1))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))))
;> (incr-pc 1 config2)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 1 0 0 0) '(1) '(0))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))))
;> (incr-pc 4093 config1)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 0 0) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))
;************************************************************

(define (equal-configs? config1 config2)
  (if (equal? (conf-cpu config1) (conf-cpu config2))
      (if (equal-rams? (conf-ram config1) (conf-ram config2))
          #t
          #f)
      #f))

(define (addr->pc addr config)
  (conf
   (cpu
    (cpu-acc (conf-cpu config))
    (n->bin addr 12)
    (cpu-rf (conf-cpu config))
    (cpu-aeb (conf-cpu config)))
   (conf-ram config)))
   
(define (incr-pc n config)
  (conf
   (cpu
    (cpu-acc (conf-cpu config))
    (n->bin (modulo (+ (bin->n (cpu-pc (conf-cpu config))) n) 4096) 12)
    (cpu-rf (conf-cpu config))
    (cpu-aeb (conf-cpu config)))
   (conf-ram config)))

;************************************************************


; (acc->mem addr config)
; takes a memory address and a configuration, and
; returns the configuration in which the contents of the accumulator
; are copied to the addressed memory register.
; No other registers change value.

; (mem->acc addr config)
; that takes a memory address and a configuration, and
; returns the configuration in which the contents of the addressed
; memory register are copied to the accumulator.
; No other registers change value.

; Examples
;> (acc->mem 3 config1)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))
;> (acc->mem 13 config3)
;(conf
; (cpu '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(1))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 13 '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))))
;> (mem->acc 4 config1)
;(conf
; (cpu '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))
;> (mem->acc 12 config3)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(1))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))))
;************************************************************

(define (acc->mem addr config)
  (conf
   (conf-cpu config)
   (ram-write addr (cpu-acc (conf-cpu config)) (conf-ram config))))

(define (mem->acc addr config)
  (conf
   (cpu
    (ram-read addr (conf-ram config))
    (cpu-pc (conf-cpu config))
    (cpu-rf (conf-cpu config))
    (cpu-aeb (conf-cpu config)))
   (conf-ram config)))

;************************************************************

; (sum sm1 sm2)
; takes two lists of bits, sm1 and sm2, of the same length, k,
; representing two numbers in k-bit sign/magnitude, and returns, in a list,
; two values.
; If the sum of the two numbers can be correctly represented in k-bit
; sign/magnitude, then the first value in the list is #t and the second
; value is the k-bit sign/magnitude representation of the sum.
; If the sum of the two numbers cannot be correctly represented in k-bit
; sign/magnitude, then the first value in the list is #f and the
; second value is a list of k zeroes.

; (diff sm1 sm2)
; is analogous to (sum sm1 sm2), except that, instead of the
; sum of the numbers represented by sm1 and sm2, the value
; computed is their difference, that is, the number represented
; by sm1 minus the number represented by sm2.
; The format of the result is the same: a list with #t and the
; k-bit sign/magnitude representation of the difference, or
; #f and a list of k zeroes.

; For both procedures, you may assume that sm1 and sm2 are 
; lists of bits of equal length, and that the length is at least 2.

; Examples
;>  (sum '(0 0 1 1) '(0 0 1 0))
;'(#t (0 1 0 1))
;> (sum '(1 0 1 1) '(0 0 1 0))
;'(#t (1 0 0 1))
;> (sum '(1 0 0 0  0 0 0 0  0 0 0 0  0 1 0 1) '(0 0 0 0  0 0 0 0  0 0 0 0  0 1 0 1))
;'(#t (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;> (sum '(0 1 0 0) '(0 1 0 0))
;'(#f (0 0 0 0))
;>  (diff '(0 0 1 1) '(0 0 1 0))
;'(#t (0 0 0 1))
;>  (diff '(0 0 1 1) '(0 1 0 1))
;'(#t (1 0 1 0))
;> (diff '(0 0 1 1) '(1 1 0 1))
;'(#f (0 0 0 0))
;************************************************************

(define (sum sm1 sm2)
  (if (not (>= (exx (- (length sm1) 1) 1) (abs (+ (sm->n sm1) (sm->n sm2))))) ; this means it's not possible
      (cons #f (list (zeros (length sm1))))
      (cons #t (list (n->sm (+ (sm->n sm1) (sm->n sm2)) (length sm1))))))

(define (exx lim n) ; will return the maximum possible value for a binary with lim digits, n should always be set to start at 1
  (if (<= n lim)
      (+ (expt 2 (- n 1)) (exx lim (+ n 1)))
      0))

(define (diff sm1 sm2)
  (cond
    [(= 1 (first sm2))
     (sum sm1 (append '(0) (rest sm2)))]
    [(= 0 (first sm2))
     (sum sm1 (append '(1) (rest sm2)))]))

;************************************************************

; (do-input config)
; (do-output config)

; Each takes a TC-201 configuration and performs the appropriate action 
; (reading a number from the user or writing a number out to the user)
; and also ***returns*** the resulting TC-201 configuration.

; For input, the new configuration has the value read in the 
; accumulator, and all other registers unchanged.
; To read in a value, you may use the following
; let construct:
; (let ((value (begin (display "input = ") (read)))) ...)

; To ensure the number typed by the user is in the correct range, 
; you may take its remainder on division by 2^(15).

; For output, the new configuration is returned *unchanged*.  
; If the integer value from the accumulator is in 
; value-from-accumulator, then the output to the user can be 
; produced by:

; (display "output = ")
; (display value-from-accumulator)
; (newline)

; Examples

; The lines input = .. and output = .. show the interaction between 
; TC-201 and user.  The TC-201 configuration shows the value
; returned by the procedure.  This assumes init-config is working.

;> (do-input (init-config '()))
;input = 14
;(conf (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0)) '())
;> (do-output (do-input (init-config '())))
;input = 33
;output = 33
;(conf (cpu '(0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0)) '())

;************************************************************

(define (do-input config)
  (let ((value (begin (display "input = ") (read))))
    (conf
   (cpu
    (n->sm value 16)
    (cpu-pc (conf-cpu config))
    (cpu-rf (conf-cpu config))
    (cpu-aeb (conf-cpu config)))
   (conf-ram config))))

(define (do-output config)
  (display "output = ")
  (display (sm->n (cpu-acc (conf-cpu config))))
  (newline)
  config)

;************************************************************

; (next-config config)

; that takes a TC-201 configuration and returns the next TC-201 configuration,
; after one iteration of the fetch/execute cycle.

; If the run flag (rf) is 0, then the configuration config is returned unchanged,
; because the machine is halted.

; The instructions that should be implemented are:

; halt, load, store, add, sub, input, output, jump
; skipzero, skippos, skiperr, loadi, storei.

; These are opcodes 0000 through 1100, respectively.
; You should intepret an undefined opcode  (1101 through 1111) 
; as a halt instruction.

; For a halt instruction, in the returned configuration 
; the run flag is 0 and all other registers are unchanged.

; Otherwise, the program counter (pc) contains a memory address, and the TC-201 
; instruction at that location is fetched and executed, and the resulting 
; configuration is returned.  Note that all instructions result in a configuration
; being returned, ***even*** input and output.
;************************************************************


(define (next-config config)
  (if (= 1 (first (cpu-rf (conf-cpu config)))) ; run flag says to do something
      (let* ((cppu (conf-cpu config))
             (acc (cpu-acc cppu))
             (ram (conf-ram config))
             (dddd (cpu-acc (conf-cpu (mem->acc (bin->n (cpu-pc (conf-cpu config))) config))))
             (ramop (ram-read (bin->n (cpu-pc cppu)) ram))
             (opcode (sublist ramop 1 4 1))
             (mem (sublist ramop 5 16 1))
             (addr (bin->n mem))
             (pc+ (cpu-pc (conf-cpu (incr-pc 1 config))))
             (mem-addr (ram-read addr ram)) ;Mem[addr]
             (cpupc+ (conf-cpu (incr-pc 1 config))) ; the cpu but only change is that pc + 1
             (cpupc++ (conf-cpu (incr-pc 2 config)))
             (mem-addr-mem (ram-read (bin->n (list-tail mem-addr 4)) ram))); for storei
        (cond
          [(equal? opcode '(0 0 0 1)) ;load
           (conf (cpu mem-addr ;acc := Mem[addr]
                       pc+ ; add one to pc
                      '(1)
                      (cpu-aeb cppu))
                 ram)]
          
          [(equal? opcode '(0 0 1 0)) ;store
           (conf cpupc+ ; add 1 to pc
                 (ram-write (bin->n mem) acc ram))]; Mem[addr] := acc

          [(equal? opcode '(0 0 1 1)) ; add,       acc := acc + Mem[addr]
           (conf
            (cpu
             (second (sum acc mem-addr)) ; sums acc and Mem[addr]
             pc+
             '(1) ; rf
             (if (first (sum acc mem-addr)) ; changes aeb to 1 if the addition doesnt work, otherwise leaves its value
                 (cpu-aeb cppu)
                 '(1)))
            ram)]
          
          [(equal? opcode '(0 1 0 0)) ;subtract
           (conf
            (cpu
             (second (diff acc mem-addr)) ; sums acc and Mem[addr]
             pc+
             '(1) ; rf
             (if (first (diff acc mem-addr)) ; changes aeb to 1 if the addition doesnt work, otherwise leaves its value
                 (cpu-aeb cppu)
                 '(1)))
            ram)]
          
          [(equal? opcode '(0 1 0 1)) ;input
           (incr-pc 1 (do-input config))]
          
          [(equal? opcode '(0 1 1 0)) ;output
           (incr-pc 1 (do-output config))]
          
          [(equal? opcode '(0 1 1 1)) ;jump
           (addr->pc addr config)]
          
          [(equal? opcode '(1 0 0 0)) ;skipzero
           (conf
            (if (= (sm->n acc) 0)
                cpupc++
                cpupc+)
            ram)]
          
          [(equal? opcode '(1 0 0 1)) ;skippos
           (conf
            (if (> (sm->n acc) 0)
               cpupc++
               cpupc+)
            ram)]
          
          [(equal? opcode '(1 0 1 0)) ;skiperr
           (if (= (cpu-aeb cppu) 1)
               cpupc++
               cpupc+)]
          
          [(equal? opcode '(1 1 0 0)) ;storei
           ;(display mem-addr-mem)
           (conf cpupc+ ; add 1 to pc
                 (ram-write (bin->n (list-tail (ram-read (bin->n (list-tail dddd 4)) ram) 4)) acc ram))]
          
          [(equal? opcode '(1 0 1 1)) ;loadi
           (conf 
            (cpu (ram-read (bin->n (list-tail (ram-read (bin->n (list-tail dddd 4)) ram) 4)) ram)
                 (cpu-pc cpupc+)
                 '(1)
                 (cpu-aeb cppu))
                 ram)]
          
          [else ;halt
           (conf
            (cpu acc
                 (cpu-pc cppu)
                 '(0)
                 (cpu-aeb cppu))
            ram)]))
      "given a '(0) run flag"))
             
                    
;************************************************************
; (init-config lst)
; takes a list lst 16 bit patterns, and returns a TC-201 configuration 
; in which those patterns are loaded into RAM starting with address 0, 
; and the CPU registers are initialized so that the accumulator has
; 16 zeroes, the program counter has 12 zeroes, the run flag has 
; value 1, and the arithmetic error bit has value 0.

; (simulate steps config)
; takes a number of steps and a configuration config of the TC-201
; and simulates (using next-config) the machine until the machine
; halts (that is, the run flag is 0) or the given number of steps
; have been executed, whichever occurs first.  The list
; of configurations reached, starting from config, is returned.

; (run steps prog)
; takes a number of steps and a symbolic assembly-language program
; prog, and assembles the program (using assemble), and loads it
; into memory (using init-config) and runs it until either it
; halts or has run for the given number of steps (using simulate).

; Examples

(define patterns
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 1 1  0 0 0 0  0 0 0 0  0 1 0 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 1 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 1 0)
    (1 0 0 0  0 0 0 0  0 0 0 0  0 0 1 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 0 0)))

;> (init-config patterns)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;  (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;  (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;  (entry 5 '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))

;> (simulate 5 (init-config patterns))
;(list
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) '(0 0 0 0 0 0 0 0 0 0 0 1) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) '(0 0 0 0 0 0 0 0 0 0 1 0) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) '(0 0 0 0 0 0 0 0 0 0 1 1) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) '(0 0 0 0 0 0 0 0 0 0 1 1) '(0) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)))))

;> (define results (run 200 prog-sum))
;input = 3
;input = -14
;input = 55
;input = 0
;output = 44
;************************************************************

; initial configuration construction

(define (init-config lst)
  (let* ((cpppu (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0)))
         (rram (init-config-helper lst 0)))
    (conf cpppu rram)))
(define (init-config-helper lst n); creates ram out of lst given n=0
  (if (not (empty? lst))
      (cons (entry n (first lst)) (init-config-helper (rest lst) (+ 1 n)))
      '()))

(define (simulate steps config)
  (if (or (= 0 (first (cpu-rf (conf-cpu config)))) (= steps 0))
      (list config)
      (cons config (simulate (- steps 1) (next-config config)))))

(define (run steps prog)
  (simulate steps
            (init-config (assemble prog))))

;************************************************************

; prog-greater
; reads in two numbers from the user and
; outputs 1 if the first is greater than the second
; and outputs 0 if the first is less than or equal to
; the second, and then halts.

; prog-list
; reads in a zero-terminated sequence of numbers from
; the user, and then prints the numbers out in the same
; order in which they were input (not including the final 0),
; and halts.

; Note that you can do this problem even if your simulator
; is not yet working.  Your programs will be tested with our
; simulator.

; Examples (showing the user interaction, not the configurations returned.)

;> (define results (run 200 prog-greater))
;input = 13
;input = 6
;output = 1
;>  (define results (run 200 prog-greater))
;input = -1
;input = -11
;output = 1
;>  (define results (run 200 prog-greater))
;input = 32767
;input = -32767
;output = 1
;>  (define results (run 200 prog-greater))
;input = -32767
;input = 32767
;output = 0
;>  (define results (run 200 prog-greater))
;input = 14
;input = 14
;output = 0

;> (define results (run 200 prog-list))
;input = 7
;input = 2
;input = 15
;input = -88
;input = 0
;output = 7
;output = 2
;output = 15
;output = -88
;> 
;************************************************************

(define prog-greater
  '((input)
    (store a1)
    (input)
    (store a2)
    (load a1)
    (sub a2)
    (skippos)
    (jump f)
    (load one)
    (output)
    (halt)
    (f: load zero)
    (output)
    (halt)
    (a1: data 0)
    (a2: data 0)
    (one: data 1)
    (zero: data 0)))

(define prog-list
  '((s: input)
  (skipzero)
  (jump n)
  (jump f)
  (n: storei i)
  (load i)
  (add c-i)
  (store i)
  (jump s)
  (f: load s-i)
  (sub i)
  (skipzero)
  (jump f-2)
  (halt)
  (f-2: loadi s-i)
  (output)
  (load s-i)
  (add c-i)
  (store s-i)
  (jump f)
  (i: data 45)
  (s-i: data 45)
  (c-i: data 1)))
