#lang racket

(define (say-hello name)
  (displayln (string-append "Hello, " name "!")))

(say-hello "World")

(say-hello "my friend")