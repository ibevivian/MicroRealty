;; MicroRealty - Micro Real Estate Fractionalization Platform
;; A smart contract for tokenizing small real estate properties, enabling partial ownership
;; and automated revenue distribution.

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-property-exists (err u101))
(define-constant err-property-not-found (err u102))
(define-constant err-insufficient-tokens (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-property-not-active (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-list-full (err u107))

;; Define data structures
(define-map properties
  { property-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    location: (string-ascii 200),
    total-shares: uint,
    available-shares: uint,
    price-per-share: uint,
    total-revenue: uint,
    is-active: bool,
    date-listed: uint
  }
)

(define-map ownership
  { property-id: uint, owner: principal }
  { shares: uint, revenue-claimed: uint }
)

(define-map property-revenues
  { property-id: uint }
  { total-revenue: uint, last-distribution: uint }
)

;; Define data variables
(define-data-var property-nonce uint u0)
(define-data-var total-properties uint u0)
(define-data-var total-investors uint u0)

;; Define a list to track all property IDs with max capacity of 100
(define-data-var all-property-ids (list 100 uint) (list))

;; Read-only functions

;; Get property details
(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

;; Get ownership details
(define-read-only (get-ownership (property-id uint) (owner principal))
  (map-get? ownership { property-id: property-id, owner: owner })
)

;; Get property revenue details
(define-read-only (get-property-revenue (property-id uint))
  (map-get? property-revenues { property-id: property-id })
)

;; Check if a user has shares in a property
(define-read-only (has-shares (property-id uint) (user principal))
  (match (get-ownership property-id user)
    owner-data (> (get shares owner-data) u0)
    false
  )
)

;; Get user's portfolio (all owned properties)
;; This is a placeholder since we can't directly filter a list in Clarity based on ownership
;; A real implementation would need to track ownership on-chain differently
(define-read-only (get-user-properties (user principal))
  (var-get all-property-ids)
)

;; Helper function to add property ID to the tracking list
(define-private (add-property-to-list (property-id uint))
  (let ((current-list (var-get all-property-ids))
        (current-length (len current-list)))
    (asserts! (< current-length u100) err-list-full)
    
    ;; At this point we know the list has room for one more item
    (let ((new-list (as-max-len? (append current-list property-id) u100)))
      (if (is-some new-list)
        (begin
          (var-set all-property-ids (unwrap-panic new-list))
          (ok true)
        )
        (err err-list-full)
      )
    )
  )
)

;; Calculate user's share of revenue for a property
(define-read-only (calculate-revenue-share (property-id uint) (user principal))
  (let (
    (property (get-property property-id))
    (user-ownership (get-ownership property-id user))
  )
    (match property
      p (match user-ownership
          o (/ (* (get shares o) (get total-revenue p)) (get total-shares p))
          u0
        )
      u0
    )
  )
)

;; Helper function to check if a principal is approved as property manager
(define-private (is-approved-manager (user principal) (property-id uint))
  ;; This would check if the user is in a list of approved managers for this property
  ;; For now, returning false as we haven't implemented manager approval yet
  false
)

;; Public functions

;; Add a new property to the platform
(define-public (list-property 
    (name (string-ascii 100)) 
    (description (string-ascii 500))
    (location (string-ascii 200))
    (total-shares uint)
    (price-per-share uint))
  (let
    (
      (property-id (+ (var-get property-nonce) u1))
      (current-list-length (len (var-get all-property-ids)))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> total-shares u0) err-invalid-amount)
    (asserts! (> price-per-share u0) err-invalid-amount)
    (asserts! (< current-list-length u100) err-list-full)
    
    (map-set properties
      { property-id: property-id }
      {
        name: name,
        description: description,
        location: location,
        total-shares: total-shares,
        available-shares: total-shares,
        price-per-share: price-per-share,
        total-revenue: u0,
        is-active: true,
        date-listed: block-height
      }
    )
    
    (map-set property-revenues
      { property-id: property-id }
      { total-revenue: u0, last-distribution: block-height }
    )
    
    ;; Add to property ID list for tracking
    (try! (add-property-to-list property-id))
    
    (var-set property-nonce property-id)
    (var-set total-properties (+ (var-get total-properties) u1))
    
    (ok property-id)
  )
)

;; Purchase shares of a property
(define-public (purchase-shares (property-id uint) (num-shares uint))
  (let
    (
      (property (unwrap! (get-property property-id) err-property-not-found))
      (price-per-share (get price-per-share property))
      (available-shares (get available-shares property))
      (is-active (get is-active property))
      (total-cost (* num-shares price-per-share))
      (user-ownership (default-to { shares: u0, revenue-claimed: u0 } 
                      (get-ownership property-id tx-sender)))
    )
    
    ;; Verify property is active and has enough shares
    (asserts! is-active err-property-not-active)
    (asserts! (<= num-shares available-shares) err-insufficient-tokens)
    (asserts! (> num-shares u0) err-invalid-amount)
    
    ;; Transfer STX from buyer to contract
    (try! (stx-transfer? total-cost tx-sender contract-owner))
    
    ;; Update property available shares
    (map-set properties
      { property-id: property-id }
      (merge property { available-shares: (- available-shares num-shares) })
    )
    
    ;; Update user ownership
    (map-set ownership
      { property-id: property-id, owner: tx-sender }
      { 
        shares: (+ (get shares user-ownership) num-shares),
        revenue-claimed: (get revenue-claimed user-ownership)
      }
    )
    
    ;; Increment total investors if this is their first purchase
    (if (is-eq (get shares user-ownership) u0)
      (var-set total-investors (+ (var-get total-investors) u1))
      true
    )
    
    (ok true)
  )
)

;; Add revenue to a property
(define-public (add-revenue (property-id uint) (amount uint))
  (let
    (
      (property (unwrap! (get-property property-id) err-property-not-found))
      (property-revenue (unwrap! (get-property-revenue property-id) err-property-not-found))
      (is-active (get is-active property))
      (current-total-revenue (get total-revenue property))
    )
    
    ;; Verify property is active
    (asserts! is-active err-property-not-active)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (or (is-eq tx-sender contract-owner) (is-approved-manager tx-sender property-id)) err-unauthorized)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender contract-owner))
    
    ;; Update property total revenue
    (map-set properties
      { property-id: property-id }
      (merge property { total-revenue: (+ current-total-revenue amount) })
    )
    
    ;; Update property revenue record
    (map-set property-revenues
      { property-id: property-id }
      { 
        total-revenue: (+ (get total-revenue property-revenue) amount),
        last-distribution: block-height
      }
    )
    
    (ok true)
  )
)

;; Claim revenue share as an investor
(define-public (claim-revenue (property-id uint))
  (let
    (
      (property (unwrap! (get-property property-id) err-property-not-found))
      (user-ownership (unwrap! (get-ownership property-id tx-sender) err-insufficient-tokens))
      (user-shares (get shares user-ownership))
      (property-total-shares (get total-shares property))
      (property-total-revenue (get total-revenue property))
      (revenue-already-claimed (get revenue-claimed user-ownership))
      (entitled-total-revenue (/ (* user-shares property-total-revenue) property-total-shares))
      (claimable-amount (- entitled-total-revenue revenue-already-claimed))
    )
    
    ;; Verify user owns shares and there's revenue to claim
    (asserts! (> user-shares u0) err-insufficient-tokens)
    (asserts! (> claimable-amount u0) err-invalid-amount)
    
    ;; Transfer revenue to user
    (try! (as-contract (stx-transfer? claimable-amount contract-owner tx-sender)))
    
    ;; Update user's claimed revenue
    (map-set ownership
      { property-id: property-id, owner: tx-sender }
      (merge user-ownership { revenue-claimed: entitled-total-revenue })
    )
    
    (ok claimable-amount)
  )
)

;; Transfer shares to another user
(define-public (transfer-shares (property-id uint) (recipient principal) (num-shares uint))
  (let
    (
      (property (unwrap! (get-property property-id) err-property-not-found))
      (sender-ownership (unwrap! (get-ownership property-id tx-sender) err-insufficient-tokens))
      (sender-shares (get shares sender-ownership))
      (recipient-ownership (default-to { shares: u0, revenue-claimed: u0 } 
                           (get-ownership property-id recipient)))
      (recipient-revenue-claimed (get revenue-claimed recipient-ownership))
      (recipient-shares (get shares recipient-ownership))
      (property-total-revenue (get total-revenue property))
      (property-total-shares (get total-shares property))
    )
    
    ;; Verify sender has enough shares
    (asserts! (>= sender-shares num-shares) err-insufficient-tokens)
    (asserts! (> num-shares u0) err-invalid-amount)
    
    ;; Calculate proportional revenue claimed for transferred shares
    (let
      (
        (revenue-per-share (if (> property-total-shares u0)
                             (/ property-total-revenue property-total-shares)
                             u0))
        (transferred-revenue-claim (* revenue-per-share num-shares))
      )
      
      ;; Update sender ownership
      (map-set ownership
        { property-id: property-id, owner: tx-sender }
        { 
          shares: (- sender-shares num-shares),
          revenue-claimed: (- (get revenue-claimed sender-ownership) transferred-revenue-claim)
        }
      )
      
      ;; Update recipient ownership
      (map-set ownership
        { property-id: property-id, owner: recipient }
        { 
          shares: (+ recipient-shares num-shares),
          revenue-claimed: (+ recipient-revenue-claimed transferred-revenue-claim)
        }
      )
      
      ;; Increment total investors if this is recipient's first shares
      (if (is-eq recipient-shares u0)
        (var-set total-investors (+ (var-get total-investors) u1))
        true
      )
      
      (ok true)
    )
  )
)

;; Deactivate a property (only by contract owner)
(define-public (deactivate-property (property-id uint))
  (let
    (
      (property (unwrap! (get-property property-id) err-property-not-found))
    )
    
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set properties
      { property-id: property-id }
      (merge property { is-active: false })
    )
    
    (ok true)
  )
)

;; Reactivate a property (only by contract owner)
(define-public (reactivate-property (property-id uint))
  (let
    (
      (property (unwrap! (get-property property-id) err-property-not-found))
    )
    
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set properties
      { property-id: property-id }
      (merge property { is-active: true })
    )
    
    (ok true)
  )
)