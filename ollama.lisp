(in-package #:ollama)

(defvar model-host "http://localhost:11434/api/generate")
(defvar model-name "mistral:v0.3")

(defun ollama-helper (curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        (cdr (assoc :response json-as-list))))))

(defun completions (starter-text)
  (let* ((json-data (with-output-to-string (s)
                      (json:encode-json
                       `(("prompt" . ,starter-text)
                         ("model" . ,model-name)
                         ("stream" . :false))
                       s)))
         (curl-command (format nil "curl ~A -d '~A'" model-host json-data)))
    (print curl-command)
    (ollama-helper curl-command)))

(defun summarize (some-text)
  (completions (concatenate 'string "Summarize: " some-text)))

(defun answer-question (some-text)
  (completions (concatenate 'string "\nQ: " some-text "\nA:")))

(defun embeddings (text)
  (let* ((d
          (concatenate
           'string
           "{\"prompt\":\""
           text
           "\", "
           "\"model\":\"" model-name "\", \"stream\":false}"))
         (curl-command
          (concatenate
           'string
           "curl  http://localhost:11434/api/embeddings "
           " -d '" d "'")))

    (let ((response
           (uiop:run-program
            curl-command
            :output :string)))
      (with-input-from-string
          (s response)
        (let* ((json-as-list (json:decode-json s)))
          (cdar json-as-list))))))

(defun dot-product-recursive (a b)
  (print "dot-product")
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun dot-product (list1 list2)
  (let ((sum 0))
    (loop for x in list1
          for y in list2
          do
               (setf sum (+ sum (* x y))))
    sum))

;; (print (dot-product '(1 2 3) '(4 5 6)))
;; (print (ollama::embeddings "John bought a new car"))

#|

(print (ollama:completions "The President went to Congress"))

(Print (ollama:summarize "Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter.[19] When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows,[20] and is on average the third-brightest natural object in the night sky after the Moon and Venus."))

(print (ollama:answer-question "Where were the 1992 Olympics held?"))
(print (ollama:answer-question "Where is the Valley of Kings?"))
(print (ollama:answer-question "Mary is 30 years old and Bob is 25. Who is older?"))

|#
