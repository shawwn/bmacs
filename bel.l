(during-compilation
  (define-macro eassert (x)
    `(unless ,x (die ',x)))
  
  (define-macro DEFSYM (var name)
    `(do (set ,var (if (undefined? ,var) (set ,var (Lisp_Symbol)) ,var))
         (define_symbol ,var ,name)))

  (define-macro c->get (p name)
    `(get ,p ',name))

  (define-macro c->set (p name value)
    `(set (get ,p ',name) ,value))

  (define-symbol SYMBOL_UNINTERNED 0)
  (define-symbol SYMBOL_INTERNED 1)
  (define-symbol SYMBOL_INTERNED_IN_INITIAL_OBARRAY 2)

  (define-symbol SYMBOL_PLAINVAL 4)
  (define-symbol SYMBOL_VARALIAS 1)
  (define-symbol SYMBOL_LOCALIZED 2)
  (define-symbol SYMBOL_FORWARDED 3)

  (define-symbol SYMBOL_UNTRAPPED_WRITE 0)
  (define-symbol SYMBOL_NOWRITE 1)
  (define-symbol SYMBOL_TRAPPED_WRITE 2)

  (define-macro SET_SYMBOL_VAL (sym v)
    `(do (eassert (= (c->get ,sym redirect) SYMBOL_PLAINVAL))
         (c->set ,sym value ,v)))
)

(set Qnil (Lisp_Symbol)
     Qt (Lisp_Symbol)
     Qunbound (Lisp_Symbol))

(define-global SYMBOLP (x)
  (print (str `(SYMBOLP ,x)))
  (and (obj? x) (= (get x '&kind) 'Lisp_Symbol)))

(define-global XSYMBOL (sym)
  (eassert (SYMBOLP sym))
  sym)

(define-global Lisp_Symbol ()
  (obj &kind: 'Lisp_Symbol))

(define-global define_symbol (sym str)
  (init_symbol sym str)
  ; TODO: Qunbound uninterned
  )

(define-global init_symbol (val name)
  (let p (XSYMBOL val)
    (set_symbol_name val name)
    (set_symbol_plist val Qnil)
    (c->set p redirect SYMBOL_PLAINVAL)
    (SET_SYMBOL_VAL p Qunbound)
    (set_symbol_function val Qnil)
    (set_symbol_next val nil)
    (c->set p gcmarkbit false)
    (c->set p interned SYMBOL_UNINTERNED)
    (c->set p trapped_write SYMBOL_UNTRAPPED_WRITE)
    (c->set p declared_special false)
    (c->set p pinned false)
    ))

(define-global die (x)
  (unless (string? x)
    (set x (str x)))
  (error (cat "assertion failed: " x)))

(define-global set_symbol_name (sym name)
  (c->set (XSYMBOL sym) name name))

(define-global set_symbol_function (sym function)
  (c->set (XSYMBOL sym) function function))

(define-global set_symbol_plist (sym plist)
  (c->set (XSYMBOL sym) plist plist))

(define-global set_symbol_next (sym next)
  (c->set (XSYMBOL sym) next next))

(define-global init_obarray_once ()
  (DEFSYM Qunbound "unbound")

  (DEFSYM Qnil "nil")
  (SET_SYMBOL_VAL (XSYMBOL Qnil) Qnil)
  ; (make_symbol_constant Qnil)
  (c->set (XSYMBOL Qnil) declared_special true)

  (DEFSYM Qt "t")
  (SET_SYMBOL_VAL (XSYMBOL Qt) Qt)
  ; (make_symbol_constant Qt)
  (c->set (XSYMBOL Qnil) declared_special true)

  (set Vpurify_flag Qt)
  
  (DEFSYM Qvariable_documentation "variable-documentation")
)

(init_obarray_once)

; (define-global Fmake_symbol (name)
;   (obj &kind: 'symbol name: name value: '&unbound function: '&unbound plist: ()))

; (guard (wipe l-obarray))

; (define-global l-obarray (if (undefined? l-obarray) (obj &kind: 'obarray) l-obarray))

; (define-global l-intern-soft (name (o obarray l-obarray))
;   (if (has? obarray name)
;       (get obarray name)
;     nil))

; (define-global l-intern (name (o obarray l-obarray))
;   (if (has? obarray name)
;       (get obarray name)
;     (set (get obarray name) (l-make-symbol name))))

; (define-global l-mapatoms (function (o obarray l-obarray))
;   (each (k v) obarray
;     (when (obj? v)
;       (call function v))))

; (define-global l-symbol-name (symbol)
;   (get symbol 'name))

; (set Qt (l-intern "t")
;      Qnil (l-intern "nil")
;      Qwrong_argument (l-intern "wrong-argument"))

; (define-global l-unintern (symbol (o obarray l-obarray))
;   (when (obj? symbol)
;     (set symbol (l-symbol-name symbol)))
;   (unless (= symbol '&kind)
;     (when (has? obarray symbol)
;       (wipe (get obarray symbol))
;       Qt)))

; (define-global l-symbol-plist (symbol)
;   (setenv symbol toplevel: true))

; (define-global l-get (symbol property)
;   (let pl (l-symbol-plist symbol)
;     (get pl property)))

; (define-global l-put (symbol property value)
;   (let pl (l-symbol-plist symbol)
;     (set (get pl property) value)))

