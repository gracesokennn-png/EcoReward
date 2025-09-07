;; title: EcoReward - Gamified Environmental Action Platform
;; version: 1.0.0
;; summary: A smart contract for rewarding verified environmental actions with tradeable impact tokens
;; description: This contract enables users to earn EcoTokens by completing verified environmental activities
;;              including cleanup activities, recycling, energy reduction, and biodiversity restoration.
;;              Corporate sponsors can fund reward pools and users can trade their impact tokens.

;; traits
(define-trait sip-010-trait
  (
    ;; Transfer from the caller to a new principal
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    ;; the human readable name of the token
    (get-name () (response (string-ascii 32) uint))
    ;; the ticker symbol, or empty if none
    (get-symbol () (response (string-ascii 32) uint))
    ;; the number of decimals used, e.g. 6 would mean 1_000_000 represents 1 token
    (get-decimals () (response uint uint))
    ;; the balance of the passed principal
    (get-balance (principal) (response uint uint))
    ;; the current total supply (which does not need to be a constant)
    (get-total-supply () (response uint uint))
    ;; an optional URI that represents metadata of this token
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; token definitions
(define-fungible-token eco-token)

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-ACTION (err u103))
(define-constant ERR-ALREADY-VERIFIED (err u104))
(define-constant ERR-VERIFICATION-FAILED (err u105))
(define-constant ERR-SPONSOR-NOT-FOUND (err u106))
(define-constant ERR-INSUFFICIENT-SPONSOR-BALANCE (err u107))
(define-constant ERR-INVALID-AMOUNT (err u108))
(define-constant ERR-ACTION-NOT-FOUND (err u109))

;; Action types
(define-constant ACTION-CLEANUP u1)
(define-constant ACTION-RECYCLING u2)
(define-constant ACTION-ENERGY-REDUCTION u3)
(define-constant ACTION-BIODIVERSITY u4)

;; Reward amounts per action type
(define-constant CLEANUP-REWARD u100)
(define-constant RECYCLING-REWARD u50)
(define-constant ENERGY-REWARD u75)
(define-constant BIODIVERSITY-REWARD u150)

;; data vars
(define-data-var token-name (string-ascii 32) "EcoToken")
(define-data-var token-symbol (string-ascii 32) "ECO")
(define-data-var token-uri (optional (string-utf8 256)) none)
(define-data-var token-decimals uint u6)
(define-data-var total-actions-completed uint u0)
(define-data-var contract-enabled bool true)
(define-data-var current-timestamp uint u1)
(define-data-var next-action-id uint u1)

;; data maps
;; User balances and activity tracking
(define-map user-actions 
  { user: principal, action-id: uint }
  { 
    action-type: uint,
    timestamp: uint,
    location-hash: (buff 32),
    proof-hash: (buff 32),
    verified: bool,
    reward-amount: uint
  }
)

;; Corporate sponsors and their contribution pools
(define-map sponsors
  { sponsor: principal }
  {
    name: (string-utf8 50),
    total-contributed: uint,
    available-balance: uint,
    active: bool
  }
)

;; User statistics
(define-map user-stats
  { user: principal }
  {
    total-actions: uint,
    cleanup-count: uint,
    recycling-count: uint,
    energy-count: uint,
    biodiversity-count: uint,
    total-tokens-earned: uint,
    reputation-score: uint
  }
)

;; Action verification queue
(define-map pending-verifications
  { action-id: uint }
  {
    user: principal,
    verifier: (optional principal),
    submitted-at: uint
  }
)

;; public functions

;; SIP-010 Token Implementation
(define-public (transfer (amount uint) (from principal) (to principal) (memo (optional (buff 34))))
  (begin
    (asserts! (or (is-eq from tx-sender) (is-eq from contract-caller)) ERR-NOT-TOKEN-OWNER)
    (ft-transfer? eco-token amount from to)
  )
)

(define-read-only (get-name)
  (ok (var-get token-name))
)

(define-read-only (get-symbol)
  (ok (var-get token-symbol))
)

(define-read-only (get-decimals)
  (ok (var-get token-decimals))
)

(define-read-only (get-balance (user principal))
  (ok (ft-get-balance eco-token user))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply eco-token))
)

(define-read-only (get-token-uri)
  (ok (var-get token-uri))
)

;; Core EcoReward Functions

;; Submit an environmental action for verification
(define-public (submit-action (action-type uint) (location-hash (buff 32)) (proof-hash (buff 32)))
  (let
    (
      (action-id (var-get next-action-id))
      (reward-amount (get-reward-amount action-type))
    )
    (asserts! (var-get contract-enabled) ERR-INVALID-ACTION)
    (asserts! (> reward-amount u0) ERR-INVALID-ACTION)
    
    ;; Store the action
    (map-set user-actions
      { user: tx-sender, action-id: action-id }
      {
        action-type: action-type,
        timestamp: (var-get current-timestamp),
        location-hash: location-hash,
        proof-hash: proof-hash,
        verified: false,
        reward-amount: reward-amount
      }
    )
    
    ;; Add to pending verifications
    (map-set pending-verifications
      { action-id: action-id }
      {
        user: tx-sender,
        verifier: none,
        submitted-at: (var-get current-timestamp)
      }
    )
    
    ;; Increment action ID
    (var-set next-action-id (+ action-id u1))
    (var-set current-timestamp (+ (var-get current-timestamp) u1))
    
    (ok action-id)
  )
)

;; Verify an action and distribute rewards (only contract owner for now)
(define-public (verify-action (user principal) (action-id uint))
  (let
    (
      (action (unwrap! (map-get? user-actions { user: user, action-id: action-id }) ERR-ACTION-NOT-FOUND))
      (reward-amount (get reward-amount action))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (not (get verified action)) ERR-ALREADY-VERIFIED)
    
    ;; Mark as verified
    (map-set user-actions
      { user: user, action-id: action-id }
      (merge action { verified: true })
    )
    
    ;; Mint reward tokens
    (unwrap! (ft-mint? eco-token reward-amount user) ERR-VERIFICATION-FAILED)
    
    ;; Update user statistics
    (update-user-stats user (get action-type action) reward-amount)
    
    ;; Update global counter
    (var-set total-actions-completed (+ (var-get total-actions-completed) u1))
    
    ;; Remove from pending verifications
    (map-delete pending-verifications { action-id: action-id })
    
    (ok true)
  )
)

;; Corporate sponsor registration
(define-public (register-sponsor (name (string-utf8 50)))
  (begin
    (map-set sponsors
      { sponsor: tx-sender }
      {
        name: name,
        total-contributed: u0,
        available-balance: u0,
        active: true
      }
    )
    (ok true)
  )
)

;; Corporate sponsor contribution (in STX, converted to reward pool)
(define-public (sponsor-contribute (amount uint))
  (let
    (
      (sponsor (unwrap! (map-get? sponsors { sponsor: tx-sender }) ERR-SPONSOR-NOT-FOUND))
    )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (get active sponsor) ERR-SPONSOR-NOT-FOUND)
    
    ;; Transfer STX to contract
    (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) ERR-INSUFFICIENT-BALANCE)
    
    ;; Update sponsor record
    (map-set sponsors
      { sponsor: tx-sender }
      {
        name: (get name sponsor),
        total-contributed: (+ (get total-contributed sponsor) amount),
        available-balance: (+ (get available-balance sponsor) amount),
        active: true
      }
    )
    
    (ok true)
  )
)

;; Trade tokens between users
(define-public (trade-tokens (amount uint) (to principal))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= (ft-get-balance eco-token tx-sender) amount) ERR-INSUFFICIENT-BALANCE)
    
    (ft-transfer? eco-token amount tx-sender to)
  )
)

;; Emergency functions (owner only)
(define-public (toggle-contract (enabled bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set contract-enabled enabled)
    (ok true)
  )
)

(define-public (update-token-uri (new-uri (string-utf8 256)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set token-uri (some new-uri))
    (ok true)
  )
)

;; read only functions

;; Get user action details
(define-read-only (get-user-action (user principal) (action-id uint))
  (map-get? user-actions { user: user, action-id: action-id })
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
  (default-to
    {
      total-actions: u0,
      cleanup-count: u0,
      recycling-count: u0,
      energy-count: u0,
      biodiversity-count: u0,
      total-tokens-earned: u0,
      reputation-score: u0
    }
    (map-get? user-stats { user: user })
  )
)

;; Get sponsor information
(define-read-only (get-sponsor-info (sponsor principal))
  (map-get? sponsors { sponsor: sponsor })
)

;; Get pending verification info
(define-read-only (get-pending-verification (action-id uint))
  (map-get? pending-verifications { action-id: action-id })
)

;; Get total actions completed globally
(define-read-only (get-total-actions)
  (var-get total-actions-completed)
)

;; Get contract status
(define-read-only (get-contract-status)
  {
    enabled: (var-get contract-enabled),
    total-supply: (ft-get-supply eco-token),
    total-actions: (var-get total-actions-completed),
    next-action-id: (var-get next-action-id)
  }
)

;; private functions

;; Get reward amount for action type
(define-private (get-reward-amount (action-type uint))
  (if (is-eq action-type ACTION-CLEANUP)
    CLEANUP-REWARD
    (if (is-eq action-type ACTION-RECYCLING)
      RECYCLING-REWARD
      (if (is-eq action-type ACTION-ENERGY-REDUCTION)
        ENERGY-REWARD
        (if (is-eq action-type ACTION-BIODIVERSITY)
          BIODIVERSITY-REWARD
          u0
        )
      )
    )
  )
)

;; Update user statistics after successful verification
(define-private (update-user-stats (user principal) (action-type uint) (reward-amount uint))
  (let
    (
      (current-stats (get-user-stats user))
      (new-total-actions (+ (get total-actions current-stats) u1))
      (new-total-tokens (+ (get total-tokens-earned current-stats) reward-amount))
      (new-reputation (+ (get reputation-score current-stats) (calculate-reputation-boost action-type)))
    )
    (map-set user-stats
      { user: user }
      {
        total-actions: new-total-actions,
        cleanup-count: (if (is-eq action-type ACTION-CLEANUP)
                        (+ (get cleanup-count current-stats) u1)
                        (get cleanup-count current-stats)),
        recycling-count: (if (is-eq action-type ACTION-RECYCLING)
                          (+ (get recycling-count current-stats) u1)
                          (get recycling-count current-stats)),
        energy-count: (if (is-eq action-type ACTION-ENERGY-REDUCTION)
                       (+ (get energy-count current-stats) u1)
                       (get energy-count current-stats)),
        biodiversity-count: (if (is-eq action-type ACTION-BIODIVERSITY)
                             (+ (get biodiversity-count current-stats) u1)
                             (get biodiversity-count current-stats)),
        total-tokens-earned: new-total-tokens,
        reputation-score: new-reputation
      }
    )
    true
  )
)

;; Calculate reputation boost based on action type
(define-private (calculate-reputation-boost (action-type uint))
  (if (is-eq action-type ACTION-CLEANUP)
    u10
    (if (is-eq action-type ACTION-RECYCLING)
      u5
      (if (is-eq action-type ACTION-ENERGY-REDUCTION)
        u8
        (if (is-eq action-type ACTION-BIODIVERSITY)
          u15
          u0
        )
      )
    )
  )
)