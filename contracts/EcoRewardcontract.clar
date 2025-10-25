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
(define-constant ERR-MAX-SUPPLY-REACHED (err u110))
(define-constant ERR-RATE-LIMIT-EXCEEDED (err u111))
(define-constant ERR-INVALID-VERIFIER (err u112))
(define-constant ERR-UNAUTHORIZED (err u113))
(define-constant ERR-INVALID-INPUT (err u114))
(define-constant ERR-OVERFLOW (err u115))

;; Maximum supply cap (1 billion tokens with 6 decimals)
(define-constant MAX-SUPPLY u1000000000000000)
;; Rate limit: minimum blocks between actions per user
(define-constant MIN-BLOCKS-BETWEEN-ACTIONS u10)
;; Maximum reward amount to prevent overflow
(define-constant MAX-REWARD-AMOUNT u1000000)

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
(define-data-var next-action-id uint u1)

;; Authorized verifiers
(define-map authorized-verifiers principal bool)

;; User rate limiting - track last action block
(define-map user-last-action principal uint)

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
      (current-block stacks-block-height)
      (last-action-block (default-to u0 (map-get? user-last-action tx-sender)))
    )
    ;; Input validation
    (asserts! (var-get contract-enabled) ERR-INVALID-ACTION)
    (asserts! (> reward-amount u0) ERR-INVALID-ACTION)
    (asserts! (<= reward-amount MAX-REWARD-AMOUNT) ERR-INVALID-AMOUNT)
    
    ;; Validate buffer inputs are not empty
    (asserts! (not (is-eq location-hash 0x0000000000000000000000000000000000000000000000000000000000000000)) ERR-INVALID-INPUT)
    (asserts! (not (is-eq proof-hash 0x0000000000000000000000000000000000000000000000000000000000000000)) ERR-INVALID-INPUT)
    
    ;; Rate limiting check
    (asserts! (>= (- current-block last-action-block) MIN-BLOCKS-BETWEEN-ACTIONS) ERR-RATE-LIMIT-EXCEEDED)
    
    ;; Check max supply won't be exceeded
    (asserts! (<= (+ (ft-get-supply eco-token) reward-amount) MAX-SUPPLY) ERR-MAX-SUPPLY-REACHED)
    
    ;; Store the action
    (map-set user-actions
      { user: tx-sender, action-id: action-id }
      {
        action-type: action-type,
        timestamp: current-block,
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
        submitted-at: current-block
      }
    )
    
    ;; Update rate limiting
    (map-set user-last-action tx-sender current-block)
    
    ;; Increment action ID with overflow check
    (asserts! (< action-id u340282366920938463463374607431768211455) ERR-OVERFLOW)
    (var-set next-action-id (+ action-id u1))
    
    (ok action-id)
  )
)

;; Verify an action and distribute rewards (owner or authorized verifiers)
(define-public (verify-action (user principal) (action-id uint))
  (let
    (
      (action (unwrap! (map-get? user-actions { user: user, action-id: action-id }) ERR-ACTION-NOT-FOUND))
      (reward-amount (get reward-amount action))
      (current-supply (ft-get-supply eco-token))
    )
    ;; Authorization check - owner or authorized verifier
    (asserts! (or 
                (is-eq tx-sender CONTRACT-OWNER)
                (default-to false (map-get? authorized-verifiers tx-sender))
              ) ERR-UNAUTHORIZED)
    
    ;; Validation checks
    (asserts! (not (get verified action)) ERR-ALREADY-VERIFIED)
    (asserts! (var-get contract-enabled) ERR-INVALID-ACTION)
    
    ;; Check max supply with overflow protection
    (asserts! (<= reward-amount (- MAX-SUPPLY current-supply)) ERR-MAX-SUPPLY-REACHED)
    
    ;; Update state BEFORE minting (reentrancy protection)
    (map-set user-actions
      { user: user, action-id: action-id }
      (merge action { verified: true })
    )
    
    ;; Remove from pending verifications
    (map-delete pending-verifications { action-id: action-id })
    
    ;; Update global counter with overflow check
    (let ((current-total (var-get total-actions-completed)))
      (asserts! (< current-total u340282366920938463463374607431768211455) ERR-OVERFLOW)
      (var-set total-actions-completed (+ current-total u1))
    )
    
    ;; Update user statistics
    (update-user-stats user (get action-type action) reward-amount)
    
    ;; Mint reward tokens AFTER state updates
    (unwrap! (ft-mint? eco-token reward-amount user) ERR-VERIFICATION-FAILED)
    
    (print {
      event: "action-verified",
      user: user,
      action-id: action-id,
      reward-amount: reward-amount,
      verifier: tx-sender
    })
    
    (ok true)
  )
)

;; Corporate sponsor registration
(define-public (register-sponsor (name (string-utf8 50)))
  (begin
    ;; Validate name is not empty
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    
    (map-set sponsors
      { sponsor: tx-sender }
      {
        name: name,
        total-contributed: u0,
        available-balance: u0,
        active: true
      }
    )
    
    (print {
      event: "sponsor-registered",
      sponsor: tx-sender,
      name: name
    })
    
    (ok true)
  )
)

;; Corporate sponsor contribution (in STX, converted to reward pool)
(define-public (sponsor-contribute (amount uint))
  (let
    (
      (sponsor (unwrap! (map-get? sponsors { sponsor: tx-sender }) ERR-SPONSOR-NOT-FOUND))
      (current-contributed (get total-contributed sponsor))
      (current-balance (get available-balance sponsor))
    )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (get active sponsor) ERR-SPONSOR-NOT-FOUND)
    
    ;; Overflow checks
    (asserts! (<= amount (- u340282366920938463463374607431768211455 current-contributed)) ERR-OVERFLOW)
    (asserts! (<= amount (- u340282366920938463463374607431768211455 current-balance)) ERR-OVERFLOW)
    
    ;; Transfer STX to contract
    (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) ERR-INSUFFICIENT-BALANCE)
    
    ;; Update sponsor record
    (map-set sponsors
      { sponsor: tx-sender }
      {
        name: (get name sponsor),
        total-contributed: (+ current-contributed amount),
        available-balance: (+ current-balance amount),
        active: true
      }
    )
    
    (print {
      event: "sponsor-contribution",
      sponsor: tx-sender,
      amount: amount
    })
    
    (ok true)
  )
)

;; Trade tokens between users
(define-public (trade-tokens (amount uint) (to principal))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (not (is-eq tx-sender to)) ERR-INVALID-INPUT)
    (asserts! (>= (ft-get-balance eco-token tx-sender) amount) ERR-INSUFFICIENT-BALANCE)
    
    (print {
      event: "tokens-traded",
      from: tx-sender,
      to: to,
      amount: amount
    })
    
    (ft-transfer? eco-token amount tx-sender to)
  )
)

;; Sponsor withdrawal function
(define-public (sponsor-withdraw (amount uint))
  (let
    (
      (sponsor-principal tx-sender)
      (sponsor (unwrap! (map-get? sponsors { sponsor: sponsor-principal }) ERR-SPONSOR-NOT-FOUND))
      (available (get available-balance sponsor))
    )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= amount available) ERR-INSUFFICIENT-SPONSOR-BALANCE)
    (asserts! (get active sponsor) ERR-SPONSOR-NOT-FOUND)
    
    ;; Update sponsor balance first (reentrancy protection)
    (map-set sponsors
      { sponsor: sponsor-principal }
      (merge sponsor { available-balance: (- available amount) })
    )
    
    ;; Transfer STX back to sponsor from contract
    (unwrap! (as-contract (stx-transfer? amount tx-sender sponsor-principal)) ERR-INSUFFICIENT-BALANCE)
    
    (print {
      event: "sponsor-withdrawal",
      sponsor: sponsor-principal,
      amount: amount
    })
    
    (ok true)
  )
)

;; Deactivate sponsor (owner only)
(define-public (deactivate-sponsor (sponsor principal))
  (let
    (
      (sponsor-data (unwrap! (map-get? sponsors { sponsor: sponsor }) ERR-SPONSOR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    
    (map-set sponsors
      { sponsor: sponsor }
      (merge sponsor-data { active: false })
    )
    
    (ok true)
  )
)

;; Add authorized verifier (owner only)
(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (not (is-eq verifier CONTRACT-OWNER)) ERR-INVALID-INPUT)
    
    (map-set authorized-verifiers verifier true)
    
    (print {
      event: "verifier-added",
      verifier: verifier
    })
    
    (ok true)
  )
)

;; Remove authorized verifier (owner only)
(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    
    (map-delete authorized-verifiers verifier)
    
    (print {
      event: "verifier-removed",
      verifier: verifier
    })
    
    (ok true)
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
    max-supply: MAX-SUPPLY,
    total-actions: (var-get total-actions-completed),
    next-action-id: (var-get next-action-id),
    current-block: stacks-block-height
  }
)

;; Check if principal is authorized verifier
(define-read-only (is-authorized-verifier (verifier principal))
  (default-to false (map-get? authorized-verifiers verifier))
)

;; Get user's last action block (for rate limiting)
(define-read-only (get-user-last-action (user principal))
  (default-to u0 (map-get? user-last-action user))
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
      (current-total-actions (get total-actions current-stats))
      (current-total-tokens (get total-tokens-earned current-stats))
      (current-reputation (get reputation-score current-stats))
      (reputation-boost (calculate-reputation-boost action-type))
    )
    ;; Overflow checks
    (asserts! (< current-total-actions u340282366920938463463374607431768211455) false)
    (asserts! (<= reward-amount (- u340282366920938463463374607431768211455 current-total-tokens)) false)
    (asserts! (<= reputation-boost (- u340282366920938463463374607431768211455 current-reputation)) false)
    
    (map-set user-stats
      { user: user }
      {
        total-actions: (+ current-total-actions u1),
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
        total-tokens-earned: (+ current-total-tokens reward-amount),
        reputation-score: (+ current-reputation reputation-boost)
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