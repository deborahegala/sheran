;; Multi-Send Contract with Failure Handling
;; Allows batch transfers with resilient error handling

;; Define SIP-010 trait for fungible tokens
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-EMPTY-RECIPIENT-LIST (err u103))
(define-constant ERR-TRANSFER-FAILED (err u104))

;; Data structures for transfer results
(define-map transfer-results 
  { batch-id: uint, recipient-index: uint }
  { 
    recipient: principal,
    amount: uint,
    success: bool,
    error-code: (optional uint)
  }
)

;; Counter for batch operations
(define-data-var batch-counter uint u0)

;; Helper function to increment batch counter
(define-private (increment-batch-counter)
  (let ((current-batch (var-get batch-counter)))
    (var-set batch-counter (+ current-batch u1))
    (+ current-batch u1)
  )
)

;; Record transfer result (returns bool for success, not a response)
(define-private (record-transfer-result 
  (batch-id uint)
  (index uint)
  (recipient principal)
  (amount uint)
  (result (response bool uint))
)
  (begin
    (match result
      success (map-set transfer-results 
                { batch-id: batch-id, recipient-index: index }
                { 
                  recipient: recipient,
                  amount: amount,
                  success: true,
                  error-code: none
                })
      error-code (map-set transfer-results 
                   { batch-id: batch-id, recipient-index: index }
                   { 
                     recipient: recipient,
                     amount: amount,
                     success: false,
                     error-code: (some error-code)
                   })
    )
    true
  )
)

;; Process single transfer with error handling
(define-private (process-single-transfer
  (transfer-data { recipient: principal, amount: uint })
  (context { sender: principal, batch-id: uint, index: uint })
)
  (let (
    (recipient (get recipient transfer-data))
    (amount (get amount transfer-data))
    (sender (get sender context))
    (batch-id (get batch-id context))
    (index (get index context))
  )
    ;; Attempt the transfer
    (let ((transfer-result (stx-transfer? amount sender recipient)))
      ;; Record the result regardless of success/failure
      (begin
        (record-transfer-result batch-id index recipient amount transfer-result)
        ;; Return updated context with incremented index
        {
          sender: sender,
          batch-id: batch-id,
          index: (+ index u1)
        }
      )
    )
  )
)

;; Multi-send function with failure handling
(define-public (multi-send-stx (recipients (list 100 { recipient: principal, amount: uint })))
  (let (
    (sender tx-sender)
    (batch-id (increment-batch-counter))
    (recipient-count (len recipients))
  )
    ;; Validate input
    (asserts! (> recipient-count u0) ERR-EMPTY-RECIPIENT-LIST)
    
    ;; Process all transfers
    (fold process-single-transfer 
          recipients 
          { sender: sender, batch-id: batch-id, index: u0 })
    
    ;; Return batch ID for result retrieval
    (ok batch-id)
  )
)

;; Get transfer result for specific recipient in a batch
(define-read-only (get-transfer-result (batch-id uint) (recipient-index uint))
  (map-get? transfer-results { batch-id: batch-id, recipient-index: recipient-index })
)

;; Helper to count successful transfers using iteration
(define-private (count-batch-successes (batch-id uint) (total uint) (current-index uint) (success-count uint))
  (let (
    (counter-data { batch-id: batch-id, current-index: current-index, success-count: success-count, total: total })
    (final-data (fold count-single-success 
                     (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 
                           u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39
                           u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59
                           u60 u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73 u74 u75 u76 u77 u78 u79
                           u80 u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91 u92 u93 u94 u95 u96 u97 u98 u99)
                     counter-data))
  )
    (get success-count final-data)
  )
)

;; Helper function for fold-based counting
(define-private (count-single-success 
  (step uint)
  (acc { batch-id: uint, current-index: uint, success-count: uint, total: uint })
)
  (let (
    (batch-id (get batch-id acc))
    (current-index (get current-index acc))
    (success-count (get success-count acc))
    (total (get total acc))
  )
    (if (>= current-index total)
      acc
      (let ((result (get-transfer-result batch-id current-index)))
        (let ((new-count (match result
                           some-result (if (get success some-result) (+ success-count u1) success-count)
                           success-count)))
          {
            batch-id: batch-id,
            current-index: (+ current-index u1),
            success-count: new-count,
            total: total
          }
        )
      )
    )
  )
)

;; Get batch summary (counts successful and failed transfers)
(define-read-only (get-batch-summary (batch-id uint) (total-recipients uint))
  (let (
    (successful-count (count-batch-successes batch-id total-recipients u0 u0))
  )
    {
      batch-id: batch-id,
      total-transfers: total-recipients,
      successful-transfers: successful-count,
      failed-transfers: (- total-recipients successful-count)
    }
  )
)

;; Filter helpers for batch summary
(define-private (is-successful-result (result (optional { recipient: principal, amount: uint, success: bool, error-code: (optional uint) })))
  (match result
    some-result (get success some-result)
    false
  )
)


(define-public (multi-send-ft 
  (token-contract <sip-010-trait>)
  (recipients (list 100 { recipient: principal, amount: uint }))
)
  (let (
    (sender tx-sender)
    (batch-id (increment-batch-counter))
    (recipient-count (len recipients))
  )
    ;; Validate input
    (asserts! (> recipient-count u0) ERR-EMPTY-RECIPIENT-LIST)
    
    ;; Process all transfers with custom token
    (fold process-single-ft-transfer
          recipients
          { sender: sender, batch-id: batch-id, index: u0, token: token-contract })
    
    ;; Return batch ID for result retrieval
    (ok batch-id)
  )
)

;; Process single fungible token transfer (for fold)
(define-private (process-single-ft-transfer
  (transfer-data { recipient: principal, amount: uint })
  (context { sender: principal, batch-id: uint, index: uint, token: <sip-010-trait> })
)
  (let (
    (recipient (get recipient transfer-data))
    (amount (get amount transfer-data))
    (sender (get sender context))
    (batch-id (get batch-id context))
    (index (get index context))
    (token-contract (get token context))
  )
    ;; Attempt the token transfer
    (let ((transfer-result (contract-call? token-contract transfer amount sender recipient none)))
      ;; Record the result
      (record-transfer-result batch-id index recipient amount transfer-result)
      ;; Return updated context
      {
        sender: sender,
        batch-id: batch-id,
        index: (+ index u1),
        token: token-contract
      }
    )
  )
)

;; Helper to build results list using iteration
(define-private (get-batch-results-recursive 
  (batch-id uint) 
  (total uint) 
  (current-index uint) 
  (acc (list 100 (optional { recipient: principal, amount: uint, success: bool, error-code: (optional uint) })))
)
  (let (
    (counter-data { batch-id: batch-id, current-index: current-index, total: total, results: acc })
    (final-data (fold collect-single-result 
                     (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 
                           u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39
                           u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59
                           u60 u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73 u74 u75 u76 u77 u78 u79
                           u80 u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91 u92 u93 u94 u95 u96 u97 u98 u99)
                     counter-data))
  )
    (get results final-data)
  )
)

;; Helper function for fold-based result collection
(define-private (collect-single-result 
  (step uint)
  (acc { batch-id: uint, current-index: uint, total: uint, results: (list 100 (optional { recipient: principal, amount: uint, success: bool, error-code: (optional uint) })) })
)
  (let (
    (batch-id (get batch-id acc))
    (current-index (get current-index acc))
    (total (get total acc))
    (results (get results acc))
  )
    (if (>= current-index total)
      acc
      (let ((result (get-transfer-result batch-id current-index)))
        {
          batch-id: batch-id,
          current-index: (+ current-index u1),
          total: total,
          results: (unwrap-panic (as-max-len? (append results result) u100))
        }
      )
    )
  )
)

;; Emergency function to get all results for a batch (up to 100 recipients)
(define-read-only (get-all-batch-results (batch-id uint) (count uint))
  (get-batch-results-recursive batch-id count u0 (list))
)
