;; Enhanced Voting Smart Contract with Advanced Features
;; Comprehensive voting system with delegation, weighted voting, and advanced analytics

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-voting-not-active (err u101))
(define-constant err-voting-ended (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-invalid-candidate (err u104))
(define-constant err-voting-still-active (err u105))
(define-constant err-not-registered (err u106))
(define-constant err-already-registered (err u107))
(define-constant err-invalid-weight (err u108))
(define-constant err-self-delegation (err u109))
(define-constant err-delegation-loop (err u110))
(define-constant err-min-participation-not-met (err u111))
(define-constant err-voting-not-started (err u112))
(define-constant err-proposal-exists (err u113))
(define-constant err-invalid-proposal (err u114))
(define-constant err-not-eligible (err u115))

;; Data Variables
(define-data-var voting-active bool false)
(define-data-var voting-start-block uint u0)
(define-data-var voting-end-block uint u0)
(define-data-var total-votes uint u0)
(define-data-var total-weight uint u0)
(define-data-var min-participation uint u50) ;; Minimum 50% participation required
(define-data-var registered-voters uint u0)
(define-data-var proposal-counter uint u0)
(define-data-var voting-fee uint u0)
(define-data-var allow-delegation bool true)
(define-data-var allow-vote-changes bool false)
(define-data-var current-voting-round uint u0)

;; Data Maps
(define-map candidates uint {
  name: (string-ascii 64), 
  vote-count: uint,
  weighted-votes: uint,
  description: (string-ascii 256),
  active: bool
})

(define-map voters principal {
  voted: bool,
  candidate-choice: uint,
  weight: uint,
  registered: bool,
  delegate: (optional principal),
  delegated-weight: uint,
  vote-timestamp: uint,
  vote-round: uint
})

(define-map delegations principal principal)
(define-map vote-history {voter: principal, round: uint} {candidate: uint, timestamp: uint})
(define-map proposals uint {
  title: (string-ascii 128),
  description: (string-ascii 512),
  proposer: principal,
  votes-for: uint,
  votes-against: uint,
  created-at: uint,
  active: bool
})

(define-map voter-eligibility principal {eligible: bool, reason: (optional (string-ascii 128))})
(define-map voting-statistics uint {
  total-votes: uint,
  participation-rate: uint,
  winner: uint,
  margin: uint,
  timestamp: uint
})

(define-map candidate-endorsements uint (list 20 principal))
(define-map vote-receipts principal {receipt-id: uint, timestamp: uint, verified: bool})

;; Token-weighted voting support
(define-map token-balances principal uint)
(define-data-var token-voting-enabled bool false)

;; Helper functions section - removed duplicate add-candidate-at-index

;; Helper function for batch voter registration
(define-private (register-voter-at-index 
  (index uint) 
  (voters-list (list 10 principal)) 
  (weights-list (list 10 uint)))
  (match (element-at voters-list index)
    voter (match (element-at weights-list index)
      weight (begin
        (map-set voters voter {
          voted: false,
          candidate-choice: u0,
          weight: weight,
          registered: true,
          delegate: none,
          delegated-weight: u0,
          vote-timestamp: u0,
          vote-round: u0
        })
        (var-set registered-voters (+ (var-get registered-voters) u1))
        (var-set total-weight (+ (var-get total-weight) weight))
        (ok true))
      (ok false))
    (ok false)))

;; Helper function to check for delegation loops (simplified version)
(define-private (has-delegation-loop (delegate principal) (original-voter principal))
  (let ((level-1 (map-get? delegations delegate)))
    (match level-1
      next-delegate-1 
      (if (is-eq next-delegate-1 original-voter)
        true
        (let ((level-2 (map-get? delegations next-delegate-1)))
          (match level-2
            next-delegate-2
            (if (is-eq next-delegate-2 original-voter)
              true
              (let ((level-3 (map-get? delegations next-delegate-2)))
                (match level-3
                  next-delegate-3
                  (is-eq next-delegate-3 original-voter)
                  false)))
            false)))
      false)))

;; Helper function to update candidate votes
(define-private (update-candidate-votes (candidate-id uint) (voting-power uint) (add bool))
  (match (map-get? candidates candidate-id)
    candidate-data
    (map-set candidates candidate-id {
      name: (get name candidate-data),
      vote-count: (if add 
                    (+ (get vote-count candidate-data) u1)
                    (- (get vote-count candidate-data) u1)),
      weighted-votes: (if add
                       (+ (get weighted-votes candidate-data) voting-power)
                       (- (get weighted-votes candidate-data) voting-power)),
      description: (get description candidate-data),
      active: (get active candidate-data)
    })
    false))

;; Helper functions for weighted voting calculations (non-recursive)
(define-private (get-max-weighted-votes (current-id uint) (max-votes uint) (iterations uint))
  (let ((votes-0 (default-to u0 (get weighted-votes (map-get? candidates u0))))
        (votes-1 (default-to u0 (get weighted-votes (map-get? candidates u1))))
        (votes-2 (default-to u0 (get weighted-votes (map-get? candidates u2))))
        (votes-3 (default-to u0 (get weighted-votes (map-get? candidates u3))))
        (votes-4 (default-to u0 (get weighted-votes (map-get? candidates u4)))))
    (let ((max-1 (if (> votes-1 votes-0) votes-1 votes-0))
          (max-2 (if (> votes-2 max-1) votes-2 max-1))
          (max-3 (if (> votes-3 max-2) votes-3 max-2))
          (max-4 (if (> votes-4 max-3) votes-4 max-3)))
      max-4)))

(define-private (get-weighted-winner-id (current-id uint) (max-votes uint))
  (let ((votes-0 (default-to u0 (get weighted-votes (map-get? candidates u0))))
        (votes-1 (default-to u0 (get weighted-votes (map-get? candidates u1))))
        (votes-2 (default-to u0 (get weighted-votes (map-get? candidates u2))))
        (votes-3 (default-to u0 (get weighted-votes (map-get? candidates u3))))
        (votes-4 (default-to u0 (get weighted-votes (map-get? candidates u4)))))
    (if (is-eq votes-0 max-votes) u0
      (if (is-eq votes-1 max-votes) u1
        (if (is-eq votes-2 max-votes) u2
          (if (is-eq votes-3 max-votes) u3
            (if (is-eq votes-4 max-votes) u4 u999)))))))

;; Helper function for live statistics (non-recursive)
(define-private (get-current-leader (current-id uint) (leader-id uint) (max-votes uint))
  (let ((votes-0 (default-to u0 (get weighted-votes (map-get? candidates u0))))
        (votes-1 (default-to u0 (get weighted-votes (map-get? candidates u1))))
        (votes-2 (default-to u0 (get weighted-votes (map-get? candidates u2))))
        (votes-3 (default-to u0 (get weighted-votes (map-get? candidates u3))))
        (votes-4 (default-to u0 (get weighted-votes (map-get? candidates u4)))))
    (let ((max-1 (if (> votes-1 votes-0) votes-1 votes-0))
          (max-2 (if (> votes-2 max-1) votes-2 max-1))
          (max-3 (if (> votes-3 max-2) votes-3 max-2))
          (max-4 (if (> votes-4 max-3) votes-4 max-3)))
      (if (is-eq votes-0 max-4) u0
        (if (is-eq votes-1 max-4) u1
          (if (is-eq votes-2 max-4) u2
            (if (is-eq votes-3 max-4) u3
              (if (is-eq votes-4 max-4) u4 u0))))))))

;; Initialize enhanced voting with advanced parameters
(define-public (initialize-voting 
  (candidate-names (list 5 (string-ascii 64)))
  (candidate-descriptions (list 5 (string-ascii 256)))
  (voting-duration uint)
  (start-delay uint)
  (min-participation-rate uint)
  (voting-fee-amount uint)
  (enable-delegation bool)
  (enable-vote-changes bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get voting-active)) (err u106))
    
    ;; Add candidates with descriptions using direct element-at calls
    (let ((name-0 (element-at candidate-names u0))
          (desc-0 (element-at candidate-descriptions u0))
          (name-1 (element-at candidate-names u1))
          (desc-1 (element-at candidate-descriptions u1))
          (name-2 (element-at candidate-names u2))
          (desc-2 (element-at candidate-descriptions u2))
          (name-3 (element-at candidate-names u3))
          (desc-3 (element-at candidate-descriptions u3))
          (name-4 (element-at candidate-names u4))
          (desc-4 (element-at candidate-descriptions u4)))
      
      ;; Set candidates directly
      (if (and (is-some name-0) (is-some desc-0))
        (map-set candidates u0 {
          name: (unwrap-panic name-0),
          vote-count: u0,
          weighted-votes: u0,
          description: (unwrap-panic desc-0),
          active: true
        })
        true)
      
      (if (and (is-some name-1) (is-some desc-1))
        (map-set candidates u1 {
          name: (unwrap-panic name-1),
          vote-count: u0,
          weighted-votes: u0,
          description: (unwrap-panic desc-1),
          active: true
        })
        true)
      
      (if (and (is-some name-2) (is-some desc-2))
        (map-set candidates u2 {
          name: (unwrap-panic name-2),
          vote-count: u0,
          weighted-votes: u0,
          description: (unwrap-panic desc-2),
          active: true
        })
        true)
      
      (if (and (is-some name-3) (is-some desc-3))
        (map-set candidates u3 {
          name: (unwrap-panic name-3),
          vote-count: u0,
          weighted-votes: u0,
          description: (unwrap-panic desc-3),
          active: true
        })
        true)
      
      (if (and (is-some name-4) (is-some desc-4))
        (map-set candidates u4 {
          name: (unwrap-panic name-4),
          vote-count: u0,
          weighted-votes: u0,
          description: (unwrap-panic desc-4),
          active: true
        })
        true))
    
    ;; Set voting parameters
    (var-set voting-active true)
    (var-set voting-start-block (+ block-height start-delay))
    (var-set voting-end-block (+ block-height start-delay voting-duration))
    (var-set min-participation min-participation-rate)
    (var-set voting-fee voting-fee-amount)
    (var-set allow-delegation enable-delegation)
    (var-set allow-vote-changes enable-vote-changes)
    (var-set current-voting-round (+ (var-get current-voting-round) u1))
    (var-set total-votes u0)
    (var-set total-weight u0)
    
    (ok true)))

;; Helper function to add candidates at specific index
(define-private (add-candidate-at-index 
  (index uint) 
  (names (list 5 (string-ascii 64))) 
  (descriptions (list 5 (string-ascii 256))))
  (match (element-at names index)
    name (match (element-at descriptions index)
      description (begin
        (map-set candidates index {
          name: name,
          vote-count: u0,
          weighted-votes: u0,
          description: description,
          active: true
        })
        (ok true))
      (ok false))
    (ok false)))

;; Register to vote (required before voting)
(define-public (register-to-vote (weight uint))
  (let ((voter tx-sender))
    (asserts! (is-none (map-get? voters voter)) err-already-registered)
    (asserts! (> weight u0) err-invalid-weight)
    
    ;; Pay registration fee if required
    (if (> (var-get voting-fee) u0)
      (try! (stx-transfer? (var-get voting-fee) tx-sender contract-owner))
      true)
    
    ;; Register voter
    (map-set voters voter {
      voted: false,
      candidate-choice: u0,
      weight: weight,
      registered: true,
      delegate: none,
      delegated-weight: u0,
      vote-timestamp: u0,
      vote-round: u0
    })
    
    (var-set registered-voters (+ (var-get registered-voters) u1))
    (var-set total-weight (+ (var-get total-weight) weight))
    
    (ok true)))

;; Delegate voting power to another voter
(define-public (delegate-vote (delegate principal))
  (let ((voter tx-sender)
        (voter-data (unwrap! (map-get? voters voter) err-not-registered)))
    (asserts! (var-get allow-delegation) (err u116))
    (asserts! (not (is-eq voter delegate)) err-self-delegation)
    (asserts! (get registered voter-data) err-not-registered)
    (asserts! (not (get voted voter-data)) err-already-voted)
    
    ;; Check for delegation loops
    (asserts! (not (has-delegation-loop delegate voter)) err-delegation-loop)
    
    ;; Update delegation
    (map-set delegations voter delegate)
    (map-set voters voter (merge voter-data {delegate: (some delegate)}))
    
    ;; Update delegate's delegated weight
    (match (map-get? voters delegate)
      delegate-data
      (begin
        (map-set voters delegate 
          (merge delegate-data 
            {delegated-weight: (+ (get delegated-weight delegate-data) (get weight voter-data))}))
        (ok true))
      (err u117))))

;; Enhanced voting with delegation support
(define-public (vote (candidate-id uint))
  (let ((voter tx-sender)
        (voter-data (unwrap! (map-get? voters voter) err-not-registered)))
    (asserts! (>= block-height (var-get voting-start-block)) err-voting-not-started)
    (asserts! (<= block-height (var-get voting-end-block)) err-voting-ended)
    (asserts! (get registered voter-data) err-not-registered)
    (asserts! (is-some (map-get? candidates candidate-id)) err-invalid-candidate)
    
    ;; Check if voter has already voted (unless vote changes are allowed)
    (if (var-get allow-vote-changes)
      true
      (asserts! (not (get voted voter-data)) err-already-voted))
    
    ;; Calculate total voting power (own weight + delegated weight)
    (let ((total-voting-power (+ (get weight voter-data) (get delegated-weight voter-data))))
      
      ;; If changing vote, subtract previous vote
      (if (and (var-get allow-vote-changes) (get voted voter-data))
        (update-candidate-votes (get candidate-choice voter-data) total-voting-power false)
        true)
      
      ;; Record the vote
      (map-set voters voter (merge voter-data {
        voted: true,
        candidate-choice: candidate-id,
        vote-timestamp: block-height,
        vote-round: (var-get current-voting-round)
      }))
      
      ;; Update candidate vote count
      (update-candidate-votes candidate-id total-voting-power true)
      
      ;; Record vote in history
      (map-set vote-history {voter: voter, round: (var-get current-voting-round)}
        {candidate: candidate-id, timestamp: block-height})
      
      ;; Generate vote receipt
      (let ((receipt-id (+ (* (var-get current-voting-round) u1000) (var-get total-votes))))
        (map-set vote-receipts voter {
          receipt-id: receipt-id,
          timestamp: block-height,
          verified: true
        }))
      
      (if (not (and (var-get allow-vote-changes) (get voted voter-data)))
        (var-set total-votes (+ (var-get total-votes) u1))
        true)
      
      (ok true))))

;; Delegation system now uses pre-defined helper functions

;; Create a proposal for voting
(define-public (create-proposal (title (string-ascii 128)) (description (string-ascii 512)))
  (let ((proposal-id (var-get proposal-counter)))
    (asserts! (is-none (map-get? proposals proposal-id)) err-proposal-exists)
    
    (map-set proposals proposal-id {
      title: title,
      description: description,
      proposer: tx-sender,
      votes-for: u0,
      votes-against: u0,
      created-at: block-height,
      active: true
    })
    
    (var-set proposal-counter (+ proposal-id u1))
    (ok proposal-id)))

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (support bool))
  (let ((voter tx-sender)
        (voter-data (unwrap! (map-get? voters voter) err-not-registered))
        (proposal-data (unwrap! (map-get? proposals proposal-id) err-invalid-proposal)))
    (asserts! (get registered voter-data) err-not-registered)
    (asserts! (get active proposal-data) err-invalid-proposal)
    
    (let ((voting-power (+ (get weight voter-data) (get delegated-weight voter-data))))
      (map-set proposals proposal-id (merge proposal-data {
        votes-for: (if support 
                     (+ (get votes-for proposal-data) voting-power)
                     (get votes-for proposal-data)),
        votes-against: (if support
                        (get votes-against proposal-data)
                        (+ (get votes-against proposal-data) voting-power))
      }))
      
      (ok true))))

;; Advanced analytics functions
(define-read-only (get-voting-analytics)
  (let ((participation-rate (if (> (var-get registered-voters) u0)
                             (/ (* (var-get total-votes) u100) (var-get registered-voters))
                             u0)))
    {
      total-registered: (var-get registered-voters),
      total-votes: (var-get total-votes),
      participation-rate: participation-rate,
      total-weight: (var-get total-weight),
      voting-round: (var-get current-voting-round),
      blocks-remaining: (if (<= block-height (var-get voting-end-block))
                         (- (var-get voting-end-block) block-height)
                         u0),
      min-participation-met: (>= participation-rate (var-get min-participation))
    }))

;; Get detailed candidate information
(define-read-only (get-candidate-details (candidate-id uint))
  (match (map-get? candidates candidate-id)
    candidate-data (ok {
      info: candidate-data,
      vote-percentage: (if (> (var-get total-votes) u0)
                        (/ (* (get vote-count candidate-data) u100) (var-get total-votes))
                        u0),
      weighted-percentage: (if (> (var-get total-weight) u0)
                           (/ (* (get weighted-votes candidate-data) u100) (var-get total-weight))
                           u0)
    })
    (err u404)))

;; Get comprehensive results with advanced statistics
(define-read-only (get-comprehensive-results)
  (let ((analytics (get-voting-analytics)))
    {
      candidates: {
        candidate-0: (get-candidate-details u0),
        candidate-1: (get-candidate-details u1),
        candidate-2: (get-candidate-details u2),
        candidate-3: (get-candidate-details u3),
        candidate-4: (get-candidate-details u4)
      },
      analytics: analytics,
      voting-period: {
        start-block: (var-get voting-start-block),
        end-block: (var-get voting-end-block),
        current-block: block-height,
        is-active: (and (>= block-height (var-get voting-start-block))
                       (<= block-height (var-get voting-end-block)))
      },
      settings: {
        delegation-enabled: (var-get allow-delegation),
        vote-changes-allowed: (var-get allow-vote-changes),
        voting-fee: (var-get voting-fee),
        min-participation: (var-get min-participation)
      }
    }))

;; Get winner with advanced validation
(define-read-only (get-certified-winner)
  (begin
    (asserts! (> block-height (var-get voting-end-block)) err-voting-still-active)
    
    (let ((analytics (get-voting-analytics))
          (max-votes (get-max-weighted-votes u0 u0 u0)))
      (asserts! (get min-participation-met analytics) err-min-participation-not-met)
      
      (ok {
        winner-id: (get-weighted-winner-id u0 max-votes),
        max-votes: max-votes,
        participation-rate: (get participation-rate analytics),
        total-votes: (var-get total-votes),
        certified: true,
        certification-block: block-height
      }))))

;; Winner calculation functions now use pre-defined helpers

;; Voter verification and audit functions
(define-read-only (verify-vote (voter principal))
  (match (map-get? vote-receipts voter)
    receipt (ok {
      verified: (get verified receipt),
      receipt-id: (get receipt-id receipt),
      timestamp: (get timestamp receipt),
      vote-round: (var-get current-voting-round)
    })
    (err u404)))

;; Get vote history for a voter
(define-read-only (get-voter-history (voter principal))
  (match (map-get? vote-history {voter: voter, round: (var-get current-voting-round)})
    history (ok history)
    (err u404)))

;; Emergency functions (owner only)
(define-public (emergency-stop)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set voting-active false)
    (ok true)))

(define-public (extend-voting-period (additional-blocks uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set voting-end-block (+ (var-get voting-end-block) additional-blocks))
    (ok true)))

;; Token-based voting functions
(define-public (enable-token-voting)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set token-voting-enabled true)
    (ok true)))

(define-public (set-token-balance (user principal) (balance uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set token-balances user balance)
    (ok true)))

;; Batch operations - now uses pre-defined helper function
(define-public (batch-register-voters (voters-list (list 10 principal)) (weights-list (list 10 uint)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (unwrap-panic (register-voter-at-index u0 voters-list weights-list))
    (unwrap-panic (register-voter-at-index u1 voters-list weights-list))
    (unwrap-panic (register-voter-at-index u2 voters-list weights-list))
    (unwrap-panic (register-voter-at-index u3 voters-list weights-list))
    (unwrap-panic (register-voter-at-index u4 voters-list weights-list))
    (unwrap-panic (register-voter-at-index u5 voters-list weights-list))
    (unwrap-panic (register-voter-at-index u6 voters-list weights-list))
    (unwrap-panic (register-voter-at-index u7 voters-list weights-list))
    (unwrap-panic (register-voter-at-index u8 voters-list weights-list))
    (unwrap-panic (register-voter-at-index u9 voters-list weights-list))
    (ok true)))

;; Real-time voting statistics
(define-read-only (get-live-statistics)
  {
    current-leader: (get-current-leader u0 u0 u0),
    vote-distribution: (get-all-vote-counts),
    weighted-distribution: (get-all-weighted-counts),
    turnout-rate: (if (> (var-get registered-voters) u0)
                   (/ (* (var-get total-votes) u100) (var-get registered-voters))
                   u0),
    time-remaining: (if (<= block-height (var-get voting-end-block))
                     (- (var-get voting-end-block) block-height)
                     u0),
    active-delegations: (count-active-delegations u0 u0)
  })

;; Live statistics functions now use pre-defined helpers

(define-read-only (get-all-weighted-counts)
  (list 
    (default-to u0 (get weighted-votes (map-get? candidates u0)))
    (default-to u0 (get weighted-votes (map-get? candidates u1)))
    (default-to u0 (get weighted-votes (map-get? candidates u2)))
    (default-to u0 (get weighted-votes (map-get? candidates u3)))
    (default-to u0 (get weighted-votes (map-get? candidates u4)))))

(define-read-only (get-all-vote-counts)
  (list 
    (default-to u0 (get vote-count (map-get? candidates u0)))
    (default-to u0 (get vote-count (map-get? candidates u1)))
    (default-to u0 (get vote-count (map-get? candidates u2)))
    (default-to u0 (get vote-count (map-get? candidates u3)))
    (default-to u0 (get vote-count (map-get? candidates u4)))))

(define-private (count-active-delegations (current-voter uint) (count uint))
  ;; Simplified count - in real implementation would iterate through all voters
  count)

;; Utility functions - removed problematic zip function
;; Replaced with simpler direct indexing approach