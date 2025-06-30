;; Enhanced File Retrieval Smart Contract
;; Comprehensive file management with advanced features

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-FILE-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u102))
(define-constant ERR-INVALID-FILE-ID (err u103))
(define-constant ERR-RETRIEVAL-DISABLED (err u104))
(define-constant ERR-OWNER-ONLY (err u105))
(define-constant ERR-QUOTA-EXCEEDED (err u106))
(define-constant ERR-FILE-EXPIRED (err u107))
(define-constant ERR-BANDWIDTH-EXCEEDED (err u108))
(define-constant ERR-INVALID-SUBSCRIPTION (err u109))
(define-constant ERR-FILE-LOCKED (err u110))
(define-constant ERR-INVALID-SIGNATURE (err u111))
(define-constant ERR-COOLDOWN-ACTIVE (err u112))
(define-constant ERR-INVALID-CATEGORY (err u113))
(define-constant ERR-ENCRYPTION-REQUIRED (err u114))
(define-constant ERR-AUDIT-REQUIRED (err u115))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-FILE-SIZE u1000000000) ;; 1GB in bytes
(define-constant DEFAULT-QUOTA u100) ;; Default monthly quota
(define-constant COOLDOWN-PERIOD u144) ;; ~24 hours in blocks
(define-constant AUDIT-RETENTION-BLOCKS u52560) ;; ~1 year

;; Data variables
(define-data-var retrieval-enabled bool true)
(define-data-var base-retrieval-fee uint u1000)
(define-data-var total-retrievals uint u0)
(define-data-var total-revenue uint u0)
(define-data-var emergency-mode bool false)
(define-data-var contract-version (string-ascii 10) "v2.0.0")
(define-data-var maintenance-mode bool false)
(define-data-var max-concurrent-retrievals uint u100)
(define-data-var current-retrievals uint u0)

;; Enhanced file registry with more metadata
(define-map files
  { file-id: (string-ascii 64) }
  {
    owner: principal,
    size: uint,
    retrieval-fee: uint,
    access-type: (string-ascii 10),
    created-at: uint,
    updated-at: uint,
    total-retrievals: uint,
    expires-at: (optional uint),
    category: (string-ascii 20),
    encryption-enabled: bool,
    checksum: (string-ascii 64),
    mime-type: (string-ascii 50),
    is-locked: bool,
    priority: uint, ;; 1-5 priority levels
    bandwidth-consumed: uint,
    last-accessed: uint,
    tags: (list 10 (string-ascii 20)),
    description: (string-ascii 200)
  }
)

;; Advanced access control with time-based permissions
(define-map file-access
  { file-id: (string-ascii 64), user: principal }
  { 
    authorized: bool, 
    granted-at: uint,
    expires-at: (optional uint),
    max-retrievals: (optional uint),
    retrievals-used: uint,
    permissions: (list 5 (string-ascii 10)) ;; "read", "download", "share", etc.
  }
)

;; Subscription system
(define-map subscriptions
  { user: principal }
  {
    plan: (string-ascii 20), ;; "basic", "premium", "enterprise"
    expires-at: uint,
    quota-remaining: uint,
    bandwidth-remaining: uint,
    features: (list 10 (string-ascii 20)),
    auto-renew: bool,
    payment-token: (optional principal)
  }
)

;; User quotas and limits
(define-map user-quotas
  { user: principal }
  {
    monthly-quota: uint,
    used-this-month: uint,
    bandwidth-quota: uint,
    bandwidth-used: uint,
    last-reset: uint,
    cooldown-until: uint,
    is-premium: bool,
    referral-bonus: uint
  }
)

;; Enhanced retrieval logs with more details
(define-map retrieval-logs
  { log-id: uint }
  {
    file-id: (string-ascii 64),
    user: principal,
    timestamp: uint,
    fee-paid: uint,
    success: bool,
    file-size: uint,
    ip-hash: (optional (string-ascii 64)),
    user-agent: (optional (string-ascii 100)),
    retrieval-method: (string-ascii 20), ;; "direct", "torrent", "cdn"
    bandwidth-used: uint,
    duration: uint,
    error-code: (optional uint)
  }
)

;; File sharing and collaboration
(define-map file-shares
  { share-id: (string-ascii 32) }
  {
    file-id: (string-ascii 64),
    creator: principal,
    created-at: uint,
    expires-at: (optional uint),
    password-protected: bool,
    download-limit: (optional uint),
    downloads-used: uint,
    public-link: bool,
    allowed-users: (list 50 principal)
  }
)

;; Audit trail for compliance
(define-map audit-trail
  { audit-id: uint }
  {
    action: (string-ascii 50),
    user: principal,
    file-id: (optional (string-ascii 64)),
    timestamp: uint,
    details: (string-ascii 200),
    ip-hash: (optional (string-ascii 64)),
    success: bool
  }
)

;; File categories and organization
(define-map categories
  { category-name: (string-ascii 20) }
  {
    description: (string-ascii 100),
    base-fee-multiplier: uint, ;; Percentage multiplier (100 = 1x)
    requires-approval: bool,
    max-file-size: uint,
    allowed-mime-types: (list 20 (string-ascii 50))
  }
)

;; Analytics and reporting
(define-map daily-stats
  { date: uint } ;; Block height representing day
  {
    total-retrievals: uint,
    total-revenue: uint,
    unique-users: uint,
    bandwidth-consumed: uint,
    average-file-size: uint,
    most-popular-file: (optional (string-ascii 64))
  }
)

;; User preferences and settings
(define-map user-preferences
  { user: principal }
  {
    notifications-enabled: bool,
    default-privacy: (string-ascii 10),
    preferred-format: (string-ascii 20),
    language: (string-ascii 10),
    timezone: int,
    auto-delete-after: (optional uint),
    email-digest: bool
  }
)

;; Referral system
(define-map referrals
  { referrer: principal, referee: principal }
  {
    created-at: uint,
    bonus-earned: uint,
    status: (string-ascii 10) ;; "pending", "active", "completed"
  }
)

;; Auto-incrementing counters
(define-data-var next-log-id uint u1)
(define-data-var next-audit-id uint u1)

;; Read-only functions

;; Get comprehensive file information
(define-read-only (get-file-details (file-id (string-ascii 64)))
  (match (map-get? files { file-id: file-id })
    file-data 
    (some {
      file-info: file-data,
      is-expired: (is-file-expired file-id),
      access-count: (get-file-access-count file-id),
      revenue-generated: (get-file-revenue file-id)
    })
    none
  )
)

;; Check if file has expired
(define-read-only (is-file-expired (file-id (string-ascii 64)))
  (match (get-file-info file-id)
    file-data
    (match (get expires-at file-data)
      expiry (>= block-height expiry)
      false
    )
    false
  )
)

;; Get file access count
(define-read-only (get-file-access-count (file-id (string-ascii 64)))
  (match (get-file-info file-id)
    file-data (get total-retrievals file-data)
    u0
  )
)

;; Calculate file revenue
(define-read-only (get-file-revenue (file-id (string-ascii 64)))
  (match (get-file-info file-id)
    file-data (* (get total-retrievals file-data) (get retrieval-fee file-data))
    u0
  )
)

;; Advanced access check with subscription validation
(define-read-only (can-access-file (file-id (string-ascii 64)) (user principal))
  (let (
    (file-data (unwrap! (get-file-info file-id) false))
    (user-quota (get-user-quota user))
    (subscription (get-subscription user))
  )
    (and
      ;; Basic access check
      (has-file-access file-id user)
      ;; File not expired
      (not (is-file-expired file-id))
      ;; File not locked
      (not (get is-locked file-data))
      ;; User has quota remaining
      (> (get quota-remaining subscription) u0)
      ;; User not in cooldown
      (< (get cooldown-until user-quota) block-height)
      ;; Subscription is valid (if required)
      (is-subscription-valid user)
    )
  )
)

;; Get user subscription
(define-read-only (get-subscription (user principal))
  (default-to
    {
      plan: "basic",
      expires-at: u0,
      quota-remaining: DEFAULT-QUOTA,
      bandwidth-remaining: u1000000,
      features: (list),
      auto-renew: false,
      payment-token: none
    }
    (map-get? subscriptions { user: user })
  )
)

;; Check if subscription is valid
(define-read-only (is-subscription-valid (user principal))
  (let ((subscription (get-subscription user)))
    (> (get expires-at subscription) block-height)
  )
)

;; Get user quota information
(define-read-only (get-user-quota (user principal))
  (default-to
    {
      monthly-quota: DEFAULT-QUOTA,
      used-this-month: u0,
      bandwidth-quota: u1000000,
      bandwidth-used: u0,
      last-reset: u0,
      cooldown-until: u0,
      is-premium: false,
      referral-bonus: u0
    }
    (map-get? user-quotas { user: user })
  )
)

;; Get daily analytics
(define-read-only (get-daily-stats (date uint))
  (map-get? daily-stats { date: date })
)

;; Get file share information
(define-read-only (get-share-info (share-id (string-ascii 32)))
  (map-get? file-shares { share-id: share-id })
)

;; Legacy function for compatibility
(define-read-only (get-file-info (file-id (string-ascii 64)))
  (map-get? files { file-id: file-id })
)

;; Legacy function for compatibility
(define-read-only (has-file-access (file-id (string-ascii 64)) (user principal))
  (let ((file-data (unwrap! (get-file-info file-id) false))
        (access-data (map-get? file-access { file-id: file-id, user: user })))
    (or
      (is-eq (get owner file-data) user)
      (is-eq (get access-type file-data) "public")
      (match access-data
        granted 
        (and 
          (get authorized granted)
          ;; Check if access hasn't expired
          (match (get expires-at granted)
            expiry (< block-height expiry)
            true
          )
          ;; Check retrieval limits
          (match (get max-retrievals granted)
            limit (< (get retrievals-used granted) limit)
            true
          )
        )
        false
      )
    )
  )
)

;; Public functions

;; Enhanced file registration with metadata
(define-public (register-file-advanced
  (file-id (string-ascii 64))
  (size uint)
  (retrieval-fee uint)
  (access-type (string-ascii 10))
  (category (string-ascii 20))
  (expires-at (optional uint))
  (mime-type (string-ascii 50))
  (checksum (string-ascii 64))
  (tags (list 10 (string-ascii 20)))
  (description (string-ascii 200)))
  
  (let ((current-block block-height))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (> (len file-id) u0) ERR-INVALID-FILE-ID)
    (asserts! (is-none (get-file-info file-id)) ERR-INVALID-FILE-ID)
    (asserts! (<= size MAX-FILE-SIZE) ERR-INVALID-FILE-ID)
    (asserts! (not (var-get maintenance-mode)) ERR-RETRIEVAL-DISABLED)
    
    ;; Validate category exists
    (asserts! (is-some (map-get? categories { category-name: category })) ERR-INVALID-CATEGORY)
    
    (map-set files
      { file-id: file-id }
      {
        owner: tx-sender,
        size: size,
        retrieval-fee: retrieval-fee,
        access-type: access-type,
        created-at: current-block,
        updated-at: current-block,
        total-retrievals: u0,
        expires-at: expires-at,
        category: category,
        encryption-enabled: false,
        checksum: checksum,
        mime-type: mime-type,
        is-locked: false,
        priority: u3,
        bandwidth-consumed: u0,
        last-accessed: u0,
        tags: tags,
        description: description
      }
    )
    
    ;; Log audit trail
    (create-audit-log "FILE_REGISTERED" tx-sender (some file-id) "Advanced file registration" true)
    
    (ok true)
  )
)

;; Enhanced file retrieval with comprehensive checks
(define-public (retrieve-file-enhanced (file-id (string-ascii 64)) (method (string-ascii 20)))
  (let (
    (file-data (unwrap! (get-file-info file-id) ERR-FILE-NOT-FOUND))
    (user-quota (get-user-quota tx-sender))
    (subscription (get-subscription tx-sender))
    (current-block block-height)
    (retrieval-fee (calculate-dynamic-fee file-id tx-sender))
  )
    ;; Comprehensive validation
    (asserts! (var-get retrieval-enabled) ERR-RETRIEVAL-DISABLED)
    (asserts! (not (var-get maintenance-mode)) ERR-RETRIEVAL-DISABLED)
    (asserts! (not (var-get emergency-mode)) ERR-RETRIEVAL-DISABLED)
    (asserts! (can-access-file file-id tx-sender) ERR-UNAUTHORIZED)
    (asserts! (< (var-get current-retrievals) (var-get max-concurrent-retrievals)) ERR-BANDWIDTH-EXCEEDED)
    
    ;; Check cooldown
    (asserts! (< (get cooldown-until user-quota) current-block) ERR-COOLDOWN-ACTIVE)
    
    ;; Check quota
    (asserts! (> (get quota-remaining subscription) u0) ERR-QUOTA-EXCEEDED)
    
    ;; Handle payment
    (if (> retrieval-fee u0)
      (try! (stx-transfer? retrieval-fee tx-sender CONTRACT-OWNER))
      true
    )
    
    ;; Update concurrent retrievals counter
    (var-set current-retrievals (+ (var-get current-retrievals) u1))
    
    ;; Create detailed log entry
    (let ((log-id (var-get next-log-id)))
      (map-set retrieval-logs
        { log-id: log-id }
        {
          file-id: file-id,
          user: tx-sender,
          timestamp: current-block,
          fee-paid: retrieval-fee,
          success: true,
          file-size: (get size file-data),
          ip-hash: none,
          user-agent: none,
          retrieval-method: method,
          bandwidth-used: (get size file-data),
          duration: u0,
          error-code: none
        }
      )
      (var-set next-log-id (+ log-id u1))
    )
    
    ;; Update file statistics
    (map-set files
      { file-id: file-id }
      (merge file-data { 
        total-retrievals: (+ (get total-retrievals file-data) u1),
        bandwidth-consumed: (+ (get bandwidth-consumed file-data) (get size file-data)),
        last-accessed: current-block
      })
    )
    
    ;; Update user quota
    (update-user-quota tx-sender (get size file-data) retrieval-fee)
    
    ;; Update subscription usage
    (update-subscription-usage tx-sender (get size file-data))
    
    ;; Update access permissions usage
    (update-access-usage file-id tx-sender)
    
    ;; Update global statistics
    (var-set total-retrievals (+ (var-get total-retrievals) u1))
    (var-set total-revenue (+ (var-get total-revenue) retrieval-fee))
    
    ;; Create audit log
    (create-audit-log "FILE_RETRIEVED" tx-sender (some file-id) "Enhanced file retrieval" true)
    
    ;; Decrement concurrent retrievals
    (var-set current-retrievals (- (var-get current-retrievals) u1))
    
    (ok { 
      file-id: file-id, 
      size: (get size file-data),
      fee-paid: retrieval-fee,
      method: method,
      quota-remaining: (- (get quota-remaining subscription) u1),
      block-height: current-block
    })
  )
)

;; Create shareable link for a file
(define-public (create-file-share 
  (file-id (string-ascii 64))
  (share-id (string-ascii 32))
  (expires-at (optional uint))
  (download-limit (optional uint))
  (password-required bool))
  
  (let ((file-data (unwrap! (get-file-info file-id) ERR-FILE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner file-data)) ERR-UNAUTHORIZED)
    (asserts! (is-none (get-share-info share-id)) ERR-INVALID-FILE-ID)
    
    (map-set file-shares
      { share-id: share-id }
      {
        file-id: file-id,
        creator: tx-sender,
        created-at: block-height,
        expires-at: expires-at,
        password-protected: password-required,
        download-limit: download-limit,
        downloads-used: u0,
        public-link: true,
        allowed-users: (list)
      }
    )
    
    (create-audit-log "SHARE_CREATED" tx-sender (some file-id) "File share link created" true)
    (ok share-id)
  )
)

;; Subscribe to a plan
(define-public (subscribe-to-plan 
  (plan (string-ascii 20))
  (duration-blocks uint)
  (payment-amount uint))
  
  (let ((plan-cost (get-plan-cost plan duration-blocks)))
    (asserts! (>= payment-amount plan-cost) ERR-INSUFFICIENT-PAYMENT)
    
    ;; Transfer payment
    (try! (stx-transfer? payment-amount tx-sender CONTRACT-OWNER))
    
    ;; Create/update subscription
    (map-set subscriptions
      { user: tx-sender }
      {
        plan: plan,
        expires-at: (+ block-height duration-blocks),
        quota-remaining: (get-plan-quota plan),
        bandwidth-remaining: (get-plan-bandwidth plan),
        features: (get-plan-features plan),
        auto-renew: false,
        payment-token: none
      }
    )
    
    (create-audit-log "SUBSCRIPTION_CREATED" tx-sender none "User subscribed to plan" true)
    (ok true)
  )
)

;; Add category for file organization
(define-public (add-category 
  (name (string-ascii 20))
  (description (string-ascii 100))
  (fee-multiplier uint)
  (requires-approval bool)
  (max-size uint))
  
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    
    (map-set categories
      { category-name: name }
      {
        description: description,
        base-fee-multiplier: fee-multiplier,
        requires-approval: requires-approval,
        max-file-size: max-size,
        allowed-mime-types: (list)
      }
    )
    (ok true)
  )
)

;; Emergency functions
(define-public (emergency-stop)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set emergency-mode true)
    (create-audit-log "EMERGENCY_STOP" tx-sender none "Emergency mode activated" true)
    (ok true)
  )
)

(define-public (emergency-resume)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set emergency-mode false)
    (create-audit-log "EMERGENCY_RESUME" tx-sender none "Emergency mode deactivated" true)
    (ok true)
  )
)

;; Private helper functions

;; Calculate dynamic fee based on various factors
(define-private (calculate-dynamic-fee (file-id (string-ascii 64)) (user principal))
  (let (
    (file-data (unwrap! (get-file-info file-id) u0))
    (base-fee (get retrieval-fee file-data))
    (category-data (default-to 
      { 
        description: "default",
        base-fee-multiplier: u100,
        requires-approval: false,
        max-file-size: MAX-FILE-SIZE,
        allowed-mime-types: (list)
      }
      (map-get? categories { category-name: (get category file-data) })
    ))
    (user-quota (get-user-quota user))
    (subscription (get-subscription user))
  )
    (if (get is-premium user-quota)
      ;; Premium users get 50% discount
      (/ (* base-fee (get base-fee-multiplier category-data)) u200)
      ;; Regular pricing with category multiplier
      (/ (* base-fee (get base-fee-multiplier category-data)) u100)
    )
  )
)

;; Update user quota after retrieval
(define-private (update-user-quota (user principal) (bandwidth-used uint) (fee-paid uint))
  (let ((current-quota (get-user-quota user)))
    (map-set user-quotas
      { user: user }
      (merge current-quota {
        used-this-month: (+ (get used-this-month current-quota) u1),
        bandwidth-used: (+ (get bandwidth-used current-quota) bandwidth-used)
      })
    )
  )
)

;; Update subscription usage
(define-private (update-subscription-usage (user principal) (bandwidth-used uint))
  (let ((subscription (get-subscription user)))
    (map-set subscriptions
      { user: user }
      (merge subscription {
        quota-remaining: (- (get quota-remaining subscription) u1),
        bandwidth-remaining: (- (get bandwidth-remaining subscription) bandwidth-used)
      })
    )
  )
)

;; Update access usage counters
(define-private (update-access-usage (file-id (string-ascii 64)) (user principal))
  (let ((access-data (map-get? file-access { file-id: file-id, user: user })))
    (match access-data
      current-access
      (map-set file-access
        { file-id: file-id, user: user }
        (merge current-access {
          retrievals-used: (+ (get retrievals-used current-access) u1)
        })
      )
      true
    )
  )
)

;; Create audit log entry
(define-private (create-audit-log 
  (action (string-ascii 50))
  (user principal)
  (file-id (optional (string-ascii 64)))
  (details (string-ascii 200))
  (success bool))
  
  (let ((audit-id (var-get next-audit-id)))
    (map-set audit-trail
      { audit-id: audit-id }
      {
        action: action,
        user: user,
        file-id: file-id,
        timestamp: block-height,
        details: details,
        ip-hash: none,
        success: success
      }
    )
    (var-set next-audit-id (+ audit-id u1))
  )
)

;; Get plan cost calculation
(define-private (get-plan-cost (plan (string-ascii 20)) (duration uint))
  (if (is-eq plan "premium")
    (* duration u10) ;; 10 microSTX per block for premium
    (if (is-eq plan "enterprise")
      (* duration u50) ;; 50 microSTX per block for enterprise
      (* duration u2) ;; 2 microSTX per block for basic
    )
  )
)

;; Get plan quota
(define-private (get-plan-quota (plan (string-ascii 20)))
  (if (is-eq plan "premium")
    u1000
    (if (is-eq plan "enterprise")
      u10000
      u100
    )
  )
)

;; Get plan bandwidth
(define-private (get-plan-bandwidth (plan (string-ascii 20)))
  (if (is-eq plan "premium")
    u10000000 ;; 10MB
    (if (is-eq plan "enterprise")
      u100000000 ;; 100MB
      u1000000 ;; 1MB
    )
  )
)

;; Get plan features
(define-private (get-plan-features (plan (string-ascii 20)))
  (if (is-eq plan "premium")
    (list "priority-support" "bulk-download" "api-access")
    (if (is-eq plan "enterprise")
      (list "priority-support" "bulk-download" "api-access" "custom-integration" "analytics")
      (list "basic-support")
    )
  )
)

;; Legacy compatibility functions
(define-public (register-file 
  (file-id (string-ascii 64))
  (size uint)
  (retrieval-fee uint)
  (access-type (string-ascii 10)))
  (register-file-advanced file-id size retrieval-fee access-type "general" none "application/octet-stream" "" (list) "")
)

(define-read-only (get-retrieval-fee (file-id (string-ascii 64)))
  (match (get-file-info file-id)
    file-data (get retrieval-fee file-data)
    (var-get base-retrieval-fee)
  )
)

(define-public (retrieve-file (file-id (string-ascii 64)))
  (retrieve-file-enhanced file-id "direct")
)

(define-public (grant-file-access 
  (file-id (string-ascii 64))
  (user principal))
  (let ((file-data (unwrap! (get-file-info file-id) ERR-FILE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner file-data)) ERR-UNAUTHORIZED)
    
    (map-set file-access
      { file-id: file-id, user: user }
      { 
        authorized: true, 
        granted-at: block-height,
        expires-at: none,
        max-retrievals: none,
        retrievals-used: u0,
        permissions: (list "read" "download")
      }
    )
    (ok true)
  )
)

(define-public (revoke-file-access 
  (file-id (string-ascii 64))
  (user principal))
  (let ((file-data (unwrap! (get-file-info file-id) ERR-FILE-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner file-data)) ERR-UNAUTHORIZED)
    (map-delete file-access { file-id: file-id, user: user })
    (ok true)
  )
)

;; Administrative functions
(define-public (set-base-retrieval-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set base-retrieval-fee new-fee)
    (ok true)
  )
)

(define-public (set-retrieval-enabled (enabled bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set retrieval-enabled enabled)
    (ok true)
  )
)

(define-public (set-maintenance-mode (enabled bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set maintenance-mode enabled)
    (create-audit-log "MAINTENANCE_MODE" tx-sender none "Maintenance mode toggled" true)
    (ok true)
  )
)

;; Read-only getters for compatibility
(define-read-only (get-user-stats (user principal))
  (let ((quota (get-user-quota user))
        (subscription (get-subscription user)))
    {
      total-retrievals: (get used-this-month quota),
      total-fees-paid: u0, ;; Could be calculated from logs
      last-retrieval: u0,   ;; Could be calculated from logs
      subscription-plan: (get plan subscription),
      quota-remaining: (get quota-remaining subscription)
    }
  )
)

(define-read-only (get-contract-stats)
  {
    total-retrievals: (var-get total-retrievals),
    total-revenue: (var-get total-revenue),
    base-fee: (var-get base-retrieval-fee),
    retrieval-enabled: (var-get retrieval-enabled),
    emergency-mode: (var-get emergency-mode),
    maintenance-mode: (var-get maintenance-mode),
    version: (var-get contract-version),
    current-retrievals: (var-get current-retrievals)
  }
)

(define-read-only (is-retrieval-enabled)
  (and 
    (var-get retrieval-enabled)
    (not (var-get emergency-mode))
    (not (var-get maintenance-mode))
  )
)