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
(define-constant err-null-string (err u107))
(define-constant err-invalid-property-id (err u108))

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

;; Use a map instead of a list to track property IDs
(define-map property-ids uint bool)

;; Define data variables
(define-data-var property-nonce uint u0)
(define-data-var total-properties uint u0)
(define-data-var total-investors uint u0)

;; Helper function to check if a principal is approved as property manager
(define-private (is-approved-manager (user principal) (pid uint))
  ;; This would check if the user is in a list of approved managers for this property
  ;; For now, returning false as we haven't implemented manager approval yet
  false
)

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

;; Check if a property ID exists
(define-read-only (property-exists (property-id uint))
  (default-to false (map-get? property-ids property-id))
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

;; Create a new property (only by contract owner)
(define-public (create-property 
  (name (string-ascii 100)) 
  (description (string-ascii 500)) 
  (location (string-ascii 200))
  (total-shares uint)
  (price-per-share uint)
)
  (begin
    ;; Validate inputs
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> total-shares u0) err-invalid-amount)
    (asserts! (> price-per-share u0) err-invalid-amount)
    (asserts! (not (is-eq name "")) err-null-string)
    (asserts! (not (is-eq location "")) err-null-string)
    (asserts! (not (is-eq description "")) err-null-string)

    ;; Increment property nonce and total properties
    (let ((new-property-id (+ (var-get property-nonce) u1)))
      ;; Add property to properties map
      (map-set properties 
        { property-id: new-property-id }
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

      ;; Mark property ID as existing
      (map-set property-ids new-property-id true)

      ;; Initialize property revenues
      (map-set property-revenues
        { property-id: new-property-id }
        { 
          total-revenue: u0, 
          last-distribution: block-height 
        }
      )

      ;; Update nonce and total properties
      (var-set property-nonce new-property-id)
      (var-set total-properties new-property-id)

      (ok new-property-id)
    )
  )
)

;; Purchase shares of a property
(define-public (purchase-shares (pid uint) (num-shares uint))
  (begin
    ;; Validate inputs and ensure property ID is valid
    (asserts! (< pid (var-get total-properties)) err-invalid-property-id)
    (asserts! (default-to false (map-get? property-ids pid)) err-property-not-found)
    (asserts! (> num-shares u0) err-invalid-amount)
    
    ;; Use the validated property ID
    (let ((property (unwrap! (map-get? properties { property-id: pid }) err-property-not-found)))
      ;; Check if property is active
      (asserts! (get is-active property) err-property-not-active)
      
      (let ((price-per-share (get price-per-share property))
            (available-shares (get available-shares property))
            (total-cost (* num-shares price-per-share))
            (user-ownership (default-to { shares: u0, revenue-claimed: u0 } 
                          (get-ownership pid tx-sender))))
        
        ;; Verify enough shares available
        (asserts! (<= num-shares available-shares) err-insufficient-tokens)
        
        ;; Transfer STX from buyer to contract
        (try! (stx-transfer? total-cost tx-sender contract-owner))
        
        ;; Update property available shares
        (map-set properties
          { property-id: pid }
          (merge property { available-shares: (- available-shares num-shares) })
        )
        
        ;; Update user ownership
        (map-set ownership
          { property-id: pid, owner: tx-sender }
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
  )
)

;; Add revenue to a property
(define-public (add-revenue (pid uint) (amount uint))
  (begin
    ;; Validate inputs and ensure property ID is valid
    (asserts! (< pid (var-get total-properties)) err-invalid-property-id)
    (asserts! (default-to false (map-get? property-ids pid)) err-property-not-found)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Use the validated property ID
    (let ((property (unwrap! (map-get? properties { property-id: pid }) err-property-not-found)))
      ;; Check if property is active
      (asserts! (get is-active property) err-property-not-active)
      
      (let ((property-revenue (unwrap! (get-property-revenue pid) err-property-not-found))
            (current-total-revenue (get total-revenue property)))
        
        ;; Verify authorization
        (asserts! (or (is-eq tx-sender contract-owner) 
                      (is-approved-manager tx-sender pid)) 
                  err-unauthorized)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender contract-owner))
        
        ;; Update property total revenue
        (map-set properties
          { property-id: pid }
          (merge property { total-revenue: (+ current-total-revenue amount) })
        )
        
        ;; Update property revenue record
        (map-set property-revenues
          { property-id: pid }
          { 
            total-revenue: (+ (get total-revenue property-revenue) amount),
            last-distribution: block-height
          }
        )
        
        (ok true)
      )
    )
  )
)

;; Claim revenue share as an investor
(define-public (claim-revenue (pid uint))
  (begin
    ;; Validate property ID is valid
    (asserts! (< pid (var-get total-properties)) err-invalid-property-id)
    (asserts! (default-to false (map-get? property-ids pid)) err-property-not-found)
    
    ;; Use the validated property ID
    (let ((property (unwrap! (map-get? properties { property-id: pid }) err-property-not-found)))
      ;; Check if property is active
      (asserts! (get is-active property) err-property-not-active)
      
      (let ((user-ownership (unwrap! (get-ownership pid tx-sender) err-insufficient-tokens)))
        (let ((user-shares (get shares user-ownership))
              (property-total-shares (get total-shares property))
              (property-total-revenue (get total-revenue property))
              (revenue-already-claimed (get revenue-claimed user-ownership)))
          
          (let ((entitled-total-revenue (/ (* user-shares property-total-revenue) property-total-shares))
                (claimable-amount (- entitled-total-revenue revenue-already-claimed)))
            
            ;; Verify user owns shares and there's revenue to claim
            (asserts! (> user-shares u0) err-insufficient-tokens)
            (asserts! (> claimable-amount u0) err-invalid-amount)
            
            ;; Transfer revenue to user
            (try! (as-contract (stx-transfer? claimable-amount contract-owner tx-sender)))
            
            ;; Update user's claimed revenue
            (map-set ownership
              { property-id: pid, owner: tx-sender }
              { 
                shares: user-shares,
                revenue-claimed: entitled-total-revenue 
              }
            )
            
            (ok claimable-amount)
          )
        )
      )
    )
  )
)

;; Deactivate a property (only by contract owner)
(define-public (deactivate-property (pid uint))
  (begin
    ;; Validate inputs and ownership
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Validate property ID is valid
    (asserts! (< pid (var-get total-properties)) err-invalid-property-id)
    (asserts! (default-to false (map-get? property-ids pid)) err-property-not-found)
    
    ;; Use the validated property ID
    (let ((property (unwrap! (map-get? properties { property-id: pid }) err-property-not-found)))
      ;; Update property status
      (map-set properties
        { property-id: pid }
        (merge property { is-active: false })
      )
      
      (ok true)
    )
  )
)

;; Reactivate a property (only by contract owner)
(define-public (reactivate-property (pid uint))
  (begin
    ;; Validate inputs and ownership
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Validate property ID is valid
    (asserts! (< pid (var-get total-properties)) err-invalid-property-id)
    (asserts! (default-to false (map-get? property-ids pid)) err-property-not-found)
    
    ;; Use the validated property ID
    (let ((property (unwrap! (map-get? properties { property-id: pid }) err-property-not-found)))
      ;; Update property status
      (map-set properties
        { property-id: pid }
        (merge property { is-active: true })
      )
      
      (ok true)
    )
  )
)