;; Algorithmic Wealth Management Contract
;; AI-driven portfolio management with decentralized execution

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_PORTFOLIO_NOT_FOUND (err u103))
(define-constant ERR_INVALID_ALLOCATION (err u104))
(define-constant ERR_REBALANCE_TOO_SOON (err u105))
(define-constant ERR_INVALID_RISK_LEVEL (err u106))

;; Data Variables
(define-data-var total-portfolios uint u0)
(define-data-var management-fee-rate uint u50) ;; 0.5% = 50 basis points
(define-data-var min-rebalance-interval uint u144) ;; ~24 hours in blocks

;; Asset structure
(define-map assets 
  { asset-id: uint }
  {
    symbol: (string-ascii 10),
    price: uint,
    last-update: uint,
    volatility: uint, ;; basis points
    is-active: bool
  }
)

;; Portfolio structure
(define-map portfolios
  { portfolio-id: uint }
  {
    owner: principal,
    total-value: uint,
    risk-level: uint, ;; 1=conservative, 2=moderate, 3=aggressive
    created-at: uint,
    last-rebalanced: uint,
    management-fee-paid: uint,
    is-active: bool
  }
)

;; Portfolio allocations
(define-map portfolio-allocations
  { portfolio-id: uint, asset-id: uint }
  {
    target-percentage: uint, ;; basis points (100 = 1%)
    current-amount: uint,
    current-percentage: uint
  }
)

;; AI model parameters
(define-map ai-models
  { model-id: uint }
  {
    name: (string-ascii 50),
    version: uint,
    accuracy-score: uint, ;; basis points
    is-active: bool,
    last-updated: uint
  }
)

;; Risk profiles
(define-map risk-profiles
  { risk-level: uint }
  {
    max-single-asset: uint, ;; basis points
    max-volatility: uint, ;; basis points
    rebalance-threshold: uint ;; basis points
  }
)

;; Initialize risk profiles
(map-set risk-profiles { risk-level: u1 }
  { max-single-asset: u3000, max-volatility: u500, rebalance-threshold: u200 })
(map-set risk-profiles { risk-level: u2 }
  { max-single-asset: u4000, max-volatility: u1000, rebalance-threshold: u300 })
(map-set risk-profiles { risk-level: u3 }
  { max-single-asset: u5000, max-volatility: u2000, rebalance-threshold: u500 })

;; Public Functions

;; Create new portfolio
(define-public (create-portfolio (risk-level uint) (initial-deposit uint))
  (let ((portfolio-id (+ (var-get total-portfolios) u1)))
    (asserts! (and (>= risk-level u1) (<= risk-level u3)) ERR_INVALID_RISK_LEVEL)
    (asserts! (> initial-deposit u0) ERR_INVALID_AMOUNT)
    
    ;; Transfer initial deposit (assuming STX for simplicity)
    (try! (stx-transfer? initial-deposit tx-sender (as-contract tx-sender)))
    
    ;; Create portfolio
    (map-set portfolios { portfolio-id: portfolio-id }
      {
        owner: tx-sender,
        total-value: initial-deposit,
        risk-level: risk-level,
        created-at: block-height,
        last-rebalanced: block-height,
        management-fee-paid: u0,
        is-active: true
      })
    
    (var-set total-portfolios portfolio-id)
    (ok portfolio-id)
  )
)

;; Add funds to portfolio
(define-public (add-funds (portfolio-id uint) (amount uint))
  (let ((portfolio (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) ERR_PORTFOLIO_NOT_FOUND)))
    (asserts! (is-eq (get owner portfolio) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Transfer funds
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update portfolio value
    (map-set portfolios { portfolio-id: portfolio-id }
      (merge portfolio { total-value: (+ (get total-value portfolio) amount) }))
    
    (ok true)
  )
)

;; Withdraw funds from portfolio
(define-public (withdraw-funds (portfolio-id uint) (amount uint))
  (let ((portfolio (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) ERR_PORTFOLIO_NOT_FOUND)))
    (asserts! (is-eq (get owner portfolio) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (get total-value portfolio)) ERR_INSUFFICIENT_BALANCE)
    
    ;; Calculate management fee
    (let ((fee (calculate-management-fee amount)))
      ;; Transfer funds minus fee
      (try! (as-contract (stx-transfer? (- amount fee) tx-sender tx-sender)))
      
      ;; Update portfolio
      (map-set portfolios { portfolio-id: portfolio-id }
        (merge portfolio { 
          total-value: (- (get total-value portfolio) amount),
          management-fee-paid: (+ (get management-fee-paid portfolio) fee)
        }))
      
      (ok (- amount fee))
    )
  )
)

;; Set portfolio allocation (AI-driven)
(define-public (set-allocation (portfolio-id uint) (asset-id uint) (target-percentage uint))
  (let ((portfolio (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) ERR_PORTFOLIO_NOT_FOUND))
        (asset (unwrap! (map-get? assets { asset-id: asset-id }) ERR_PORTFOLIO_NOT_FOUND)))
    
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED) ;; Only AI system can set allocations
    (asserts! (<= target-percentage u10000) ERR_INVALID_ALLOCATION) ;; Max 100%
    (asserts! (get is-active asset) ERR_INVALID_ALLOCATION)
    
    ;; Validate against risk profile
    (let ((risk-profile (unwrap! (map-get? risk-profiles { risk-level: (get risk-level portfolio) }) ERR_INVALID_RISK_LEVEL)))
      (asserts! (<= target-percentage (get max-single-asset risk-profile)) ERR_INVALID_ALLOCATION)
      
      ;; Set allocation
      (map-set portfolio-allocations { portfolio-id: portfolio-id, asset-id: asset-id }
        {
          target-percentage: target-percentage,
          current-amount: u0,
          current-percentage: u0
        })
      
      (ok true)
    )
  )
)

;; Execute rebalancing
(define-public (rebalance-portfolio (portfolio-id uint))
  (let ((portfolio (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) ERR_PORTFOLIO_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED) ;; Only AI system can rebalance
    (asserts! (>= (- block-height (get last-rebalanced portfolio)) (var-get min-rebalance-interval)) ERR_REBALANCE_TOO_SOON)
    
    ;; Update last rebalanced timestamp
    (map-set portfolios { portfolio-id: portfolio-id }
      (merge portfolio { last-rebalanced: block-height }))
    
    ;; Execute rebalancing logic (simplified)
    (try! (execute-trades portfolio-id))
    
    (ok true)
  )
)

;; Add new asset
(define-public (add-asset (asset-id uint) (symbol (string-ascii 10)) (initial-price uint) (volatility uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> initial-price u0) ERR_INVALID_AMOUNT)
    
    (map-set assets { asset-id: asset-id }
      {
        symbol: symbol,
        price: initial-price,
        last-update: block-height,
        volatility: volatility,
        is-active: true
      })
    
    (ok true)
  )
)

;; Update asset price (oracle integration)
(define-public (update-asset-price (asset-id uint) (new-price uint))
  (let ((asset (unwrap! (map-get? assets { asset-id: asset-id }) ERR_PORTFOLIO_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-price u0) ERR_INVALID_AMOUNT)
    
    (map-set assets { asset-id: asset-id }
      (merge asset { 
        price: new-price,
        last-update: block-height
      }))
    
    (ok true)
  )
)

;; Register AI model
(define-public (register-ai-model (model-id uint) (name (string-ascii 50)) (accuracy-score uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= accuracy-score u10000) ERR_INVALID_AMOUNT)
    
    (map-set ai-models { model-id: model-id }
      {
        name: name,
        version: u1,
        accuracy-score: accuracy-score,
        is-active: true,
        last-updated: block-height
      })
    
    (ok true)
  )
)

;; Private Functions

;; Calculate management fee
(define-private (calculate-management-fee (amount uint))
  (/ (* amount (var-get management-fee-rate)) u10000)
)

;; Execute trades (simplified implementation)
(define-private (execute-trades (portfolio-id uint))
  (let ((portfolio (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) ERR_PORTFOLIO_NOT_FOUND)))
    ;; Simplified trade execution logic
    ;; In production, this would integrate with DEXs
    (ok true)
  )
)

;; Read-only Functions

;; Get portfolio details
(define-read-only (get-portfolio (portfolio-id uint))
  (map-get? portfolios { portfolio-id: portfolio-id })
)

;; Get portfolio allocation
(define-read-only (get-allocation (portfolio-id uint) (asset-id uint))
  (map-get? portfolio-allocations { portfolio-id: portfolio-id, asset-id: asset-id })
)

;; Get asset details
(define-read-only (get-asset (asset-id uint))
  (map-get? assets { asset-id: asset-id })
)

;; Get AI model info
(define-read-only (get-ai-model (model-id uint))
  (map-get? ai-models { model-id: model-id })
)

;; Calculate portfolio performance
(define-read-only (calculate-portfolio-value (portfolio-id uint))
  (let ((portfolio (unwrap! (map-get? portfolios { portfolio-id: portfolio-id }) ERR_PORTFOLIO_NOT_FOUND)))
    (ok (get total-value portfolio))
  )
)

;; Get total portfolios count
(define-read-only (get-total-portfolios)
  (var-get total-portfolios)
)

;; Get management fee rate
(define-read-only (get-management-fee-rate)
  (var-get management-fee-rate)
)