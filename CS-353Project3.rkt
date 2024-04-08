;; CS-353 Project 3 / Ryan Lee / 4-7-24 / Made in conjuncture with ChatGPT for the creation, explanation, and debugging of code
#lang racket
(require racket/hash)
(require racket/string)

;; Function to read lines from a file
(define (read-file-lines file)
  (with-input-from-file file
    (lambda ()
      (let loop ((line (read-line)) (lines '()))
        ;; Read each line of the file until the end-of-file marker is reached
        (if (eof-object? line)
            ;; Return the lines in reverse order to maintain original order
            (reverse lines)
            (loop (read-line) (cons line lines)))))))

;; Function to check if a string can be converted to a number
(define (get-number str)
  (cond
    [(string->number str) => values] ; If the string can be converted to a number, return it
    [else #f])) ; Otherwise, return false

;; Function to extract amount from a string
(define (get-amount str)
  (define trim-str (string-trim str))
  ;; Check if the trimmed string matches a pattern for a valid amount
  (if (regexp-match? #rx"^-?[0-9]+(?:\\.[0-9]+)?$" trim-str)
      ;; If it matches, convert it to a number
      (string->number trim-str)
      ;; If it doesn't match, return 0
      (begin 0)))

;; Function to extract merchant from a string
(define (get-merchant str)
  ;; Check if the string is enclosed in double quotes
  (if (and (> (string-length str) 1)
           (char=? (string-ref str 0) #\")
           (char=? (string-ref str (- (string-length str) 1)) #\"))
      ;; If yes, remove the quotes and return the merchant name
      (substring str 1 (- (string-length str) 1)) str))

;; Function to extract payment method details
(define (get-pay-method value)
  (let ((method (string-trim (string-downcase (list-ref value 3)))))
    (cond
      ;; If payment method is cash, return "Cash"
      ((string=? method "cash") (list "Cash" "" ""))
      ;; If payment method is check, extract check number if available, otherwise return "Check"
      ((string=? method "check")
       (let* ((check-number (if (>= (length value) 5) (string-trim (list-ref value 4)) ""))
              (amount-str (if (>= (length value) 6) (string-trim (list-ref value 5)) "0")))
         (list "Check" check-number amount-str)))
      ;; If payment method is credit, extract credit card number if available, otherwise return "Credit"
      ((string=? method "credit")
       (let* ((credit-number (if (>= (length value) 5) (string-trim (list-ref value 4)) ""))
              (amount-str (if (>= (length value) 6) (string-trim (list-ref value 5)) "0")))
         (list "Credit" credit-number amount-str)))
      ;; If payment method is debit, extract debit card number if available, otherwise return "Debit"
      ((string=? method "debit")
       (let* ((debit-number (if (>= (length value) 5) (string-trim (list-ref value 4)) ""))
              (amount-str (if (>= (length value) 6) (string-trim (list-ref value 5)) "0")))
         (list "Debit" debit-number amount-str)))
      ;; If payment method is unknown, return "Unknown"
      (else (list "Unknown" "" "---")))))

;; Function to extract transaction details from a line
(define (get-transactions line transac-num)
  (let* ((value (regexp-split #px"\t" line)) ; Split the line by tab characters
         (type (string->symbol (string-downcase (list-ref value 0)))) ; Extract transaction type
         (acc-num (get-number (list-ref value 1))) ; Extract account number
         (transact-num (if (>= (length value) 3) (list-ref value 2) "")) ; Extract transaction number if available
         (merchant (if (>= (length value) 4) (get-merchant (list-ref value 3)) "")) ; Extract merchant name if available
         (pay-method-type (if (eq? type 'payment) (get-pay-method value) (list "Purchase" "" ""))) ; Extract payment method details if applicable
         (pay-method (first pay-method-type)) ; Extract payment method
         (card-check-num (second pay-method-type)) ; Extract card or check number
         (amount-str (if (>= (length value) 6) (list-ref value 5) (if (>= (length value) 5) (list-ref value 4) ""))) ; Extract transaction amount
         (amount (get-amount amount-str))) ; Convert amount to number
    ;; Return transaction details as a hash table
    (hash 'type type 'transac-num transac-num 'acc-num acc-num 'transact-num transact-num 'merchant merchant 'pay-method pay-method 'card-check-num card-check-num 'amount amount)))

;; Function to get account details from a file
(define (get-accounts file)
  ;; Fold over lines of the file to extract account details
  (foldl (lambda (line acc-hash)
           (let* ((value (regexp-match #px"([0-9]+)\\s+\"([^\"]+)\"\\s+([0-9]+\\.[0-9]+)" line)))
             (if value
                 (let ((acc-num (string->number (second value))) ; Extract account number
                       (bal (string->number (fourth value)))) ; Extract account balance
                   ;; Store account details in a hash table
                   (hash-set acc-hash acc-num (hash 'acct-num acc-num 'custInfo (third value) 'bal bal 'transactions '() 'bal bal))) acc-hash)))
         (hash) ; Start with an empty hash table
         (read-file-lines file))) ; Read lines from the file

;; Function to process transactions from a file
(define (process-transactions transactions)
  (let* ((lines (read-file-lines transactions)) ; Read lines from transactions file
         (transactions (for/list ([line lines] [index (in-naturals)]) ; Process each line to extract transaction details
                         (get-transactions line index)))
         (combine-transact (foldl (lambda (transacts acc) ; Combine transactions for each account
                                        (let ((acc-num (hash-ref transacts 'acc-num))) ; Get account number
                                          (hash-update acc acc-num (curry cons transacts) '()))) ; Add transaction to the list of transactions for the account
                                      (hash) transactions))) combine-transact))

;; Function to adjust amount to have two decimal places
(define (adjust-amount amount)
  (define amount-str (number->string amount))
  (define value (regexp-split #rx"\\.0$" amount-str))
  (define adjusted-str
    (if (null? (cdr value))
        amount-str
        (string-append (car value) ".00"))) adjusted-str)

;; Function to adjust transaction details for display
(define (adjust-transactions transactions acc-num bal)
  (string-append
   (format "~a, Starting Balance: ~a\n" acc-num bal)
   (apply string-append
          (map (lambda (transact)
                 (format "Transaction - Type: ~s\t, Purchase: ~a\t, Payment ~a\t, ~a\t, ~a" (hash-ref transact 'type) (hash-ref transact 'merchant) (hash-ref transact 'payment-method) (hash-ref transact 'timestamp "No Timestamp found") (hash-ref transact 'amount "No Amount Found"))) transactions))))

;; Function to adjust account balances based on transactions
(define (adjust-balances acc transact-hash)
  (if (hash-has-key? acc 'acc-num)
      (let* ((acc-num (hash-ref acc 'acc-num))
             (transactions (hash-ref transact-hash acc-num '()))
             (bal (hash-ref acc 'bal 0))
             (new-bal (foldl (lambda (transact acc-bal)
                                   (let ((amount (hash-ref transact 'amount))
                                         (type (hash-ref transact 'type)))
                                     (case type
                                       ('payment (+ acc-bal amount))
                                       ('purchase (- acc-bal amount))
                                       (else acc-bal))))
                                 bal transactions)))
        (hash 'acc-num acc-num 'cust-info (hash-ref acc 'cust-info) 'new-bal new-bal 'transactions transactions 'bal bal)) acc))

;; Function to update account details with adjusted balances
(define (update-accounts acc transact-hash)
  (foldl (lambda (acc-pair acc-hash)
           (let ((acc-num (car acc-pair))
                 (acc (cdr acc-pair)))
             (hash-set acc-hash acc-num (adjust-balances acc transact-hash))))
         (hash)
         (hash->list acc)))

;; Function to display account details
(define (display-account acc-num acc transactions)
  (let* ((bal (hash-ref acc 'bal 0.00))
         (total-purchases (apply + (map (lambda (transact)
                                          (if (equal? (hash-ref transact 'type) 'purchase)
                                              (hash-ref transact 'amount 0.00)
                                              0.00)) transactions)))
         (total-payments (apply + (map (lambda (transact)
                                         (if (equal? (hash-ref transact 'type) 'payment)
                                             (hash-ref transact 'amount 0.00) 0.00)) transactions)))
         (ending-bal (+ bal total-payments (- total-purchases))))
    
    (printf "STATEMENT OF ACCOUNT\n")
    (printf "~a\t~a\tStarting Balance: ~a\n\n" acc-num (hash-ref acc 'custInfo "Unknown Customer") (adjust-amount bal))
    
    (for-each (lambda (transact)
                (let* ((type (hash-ref transact 'type))
                       (amount (hash-ref transact 'amount 0.00))
                       (pay-method (hash-ref transact 'pay-method ""))
                       (check-num (hash-ref transact 'check-num ""))
                       (card-num (hash-ref transact 'card-num ""))
                       (transact-num (hash-ref transact 'transact-num ""))
                       (merchant (if (equal? type 'purchase) (hash-ref transact 'merchant "") "")))
                  (printf "~a\t" transact-num)
                  (when (equal? type 'purchase)
                    (printf "Purchase: ~a\t" merchant))
                  (printf "Payment: ~a\t" pay-method)
                  (when (not (equal? check-num ""))
                    (printf "Check Number: ~a\t" check-num))
                  (when (not (equal? card-num ""))
                    (printf "Card Number: ~a\t" card-num))
                  (printf "Amount: ~a\n" (adjust-amount amount)))) transactions)
    
    (printf "\nTotal Purchases: ~a\n"(adjust-amount total-purchases))
    (printf "Total Payments: ~a\n" (adjust-amount total-payments))
    (printf "Ending Balance: ~a\n\n*********************************************************\n" (adjust-amount ending-bal))))

;; Function to output account details to a file
(define (output-file file acc combine-transact)
  (with-output-to-file file
    (lambda ()
      (if (hash? acc)
          (begin
            (for-each
             (lambda (acc-pair)
               (let* ((acc-num (car acc-pair))
                      (account (cdr acc-pair))
                      (transactions (hash-ref combine-transact acc-num '())))
                 (display-account acc-num account transactions)))
             (hash->list acc)))
          (printf "No accounts found.")))
    #:exists 'replace))

;; Main function to process files
(let* ((accountFile "ACCOUNTS.txt")
       (transactionFile "TRANSACTIONS.txt")
       (accounts (get-accounts accountFile))
       (combine-transact (process-transactions transactionFile))
       (updated-acc (update-accounts accounts combine-transact)))
  (output-file "STATEMENTS.txt" updated-acc combine-transact))
