;;;; miller.scm - a markdown generator using line-comments
; This is free and unencumbered software released into the public domain.
;
; Anyone is free to copy, modify, publish, use, compile, sell, or
; distribute this software, either in source code form or as a compiled
; binary, for any purpose, commercial or non-commercial, and by any
; means.
; 
; In jurisdictions that recognize copyright laws, the author or authors
; of this software dedicate any and all copyright interest in the
; software to the public domain. We make this dedication for the benefit
; of the public at large and to the detriment of our heirs and
; successors. We intend this dedication to be an overt act of
; relinquishment in perpetuity of all present and future rights to this
; software under copyright law.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
; OTHER DEALINGS IN THE SOFTWARE.
;
; For more information, please refer to <http://unlicense.org/>

;;; Usage

; syntax:  
;
; | prefix       | description                                    |
; |:-------------|:-----------------------------------------------|
; | ;;;;         | h1 header (#)                                  |
; | ;;;          | h2 header (##)                                 |
; | ;;           | h3 header (###)                                |
; | ;\           | markdown document block (no trim)              |
; | ;            | markdown document block (trim left-whitespace) |
; |              | lisp code block                                |
; | (blank line) | delimit block                                  |

;;; Implementation

;; 1. Using Libraries
; target implementation: [Chicken Scheme 4.10.0](http://www.call-cc.org/)
(use srfi-13 ports extras)

;; 2. Define: Converter
(define (miller path)
  (define (miller)
    (define (mode-blank line) (printf "~%"))
    (define (mode-lisp line)  (printf "~a~%" line))
    (define (mode-markdown line)
      (define (md-pattern s1 s2 fn)
        (and (string-prefix? s1 line)
             (string-append s2 (fn (string-drop line (string-length s1))))))
      (printf "~a~%" (or (md-pattern ";;;;" "# "   string-trim)
                         (md-pattern ";;;"  "## "  string-trim)
                         (md-pattern ";;"   "### " string-trim)
                         (md-pattern ";\\"  ""     identity)
                         (md-pattern ";"    ""     string-trim))))
    (define (move from to)
      (define (from-to? a b) (and (eq? from a) (eq? to b)))
      (cond ((from-to? mode-lisp     mode-markdown) (printf "```~%~%"))
            ((from-to? mode-lisp     mode-blank)    (printf "```~%"))
            ((from-to? mode-markdown mode-lisp)     (printf "~%```scheme~%"))
            ((from-to? mode-blank    mode-lisp)     (printf "```scheme~%"))))
    (define (rule line)
      (cond ((string-prefix? ";" line) mode-markdown)
            ((string-null? line)       mode-blank)
            (else                      mode-lisp)))
    (define (miller line from)
      (let ((to (rule line))) (move from to) (to line) to))
    (move (port-fold miller mode-blank read-line) mode-blank))
  ;; redirect input-output
  (with-input-from-file (string-append path ".scm")
    (cut with-output-to-file (string-append path ".md")
      miller)))

;; 3. Apply target files
; for example: [miller.scm](miller.scm)
(miller "miller")

