;; Enhanced Yield Farming Contract
;; Advanced implementation with multiple pools, boost mechanics, and governance features

(use-trait fungible-token-trait .sip-010-trait-ft-standard.sip-010-trait)
(use-trait governance-token-trait .governance-token-trait.governance-trait)

;; Error codes with descriptive names
(define-constant ERROR-UNAUTHORIZED-ACCESS (err u1000))
(define-constant ERROR-INVALID-STAKE-AMOUNT (err u1001))
(define-constant ERROR-STAKE-NOT-FOUND (err u1002))
(define-constant ERROR-LOCK-PERIOD-NOT-EXPIRED (err u1003))
(define-constant ERROR-INSUFFICIENT-TOKEN-BALANCE (err u1004))
(define-constant ERROR-STAKING-POOL-NOT-FOUND (err u1005))
(define-constant ERROR-STAKING-POOL-INACTIVE (err u1006))
(define-constant ERROR-MAXIMUM-POOLS-REACHED (err u1007))
(define-constant ERROR-YIELD-BOOST-NOT-ACTIVE (err u1008))
(define-constant ERROR-GOVERNANCE-PROPOSAL-NOT-ACTIVE (err u1009))

;; System Constants
(define-constant MAXIMUM-STAKING-POOLS u10)
(define-constant YIELD-BOOST-MULTIPLIER u150)  ;; 1.5x boost
(define-constant REFERRAL-BONUS-PERCENTAGE u5) ;; 5% referral bonus
(define-constant MAXIMUM-LOCK-DURATION-BONUS u200) ;; 2x max bonus for long-term locking

;; Contract Configuration Variables
(define-data-var contract-administrator principal tx-sender)
(define-data-var base-annual-yield-rate uint u100)
(define-data-var minimum-staking-period uint u1440)
(define-data-var total-staked-tokens uint u0)
(define-data-var last-reward-distribution-block uint block-height)
(define-data-var active-staking-pool-count uint u0)
(define-data-var reward-harvest-cooldown-period uint u100)
(define-data-var governance-token-address principal .governance-token)

;; Enhanced Staking Data Structure
(define-map user-staking-positions
    principal
    {
        staked-amount: uint,
        staking-start-block: uint,
        staking-lock-duration: uint,
        total-rewards-claimed: uint,
        yield-boost-enabled: bool,
        last-harvest-block: uint,
        referral-source: (optional principal)
    }
)

;; Staking Pool Configuration
(define-map staking-pool-configurations
    uint
    {
        staking-token-address: principal,
        reward-token-address: principal,
        annual-percentage-rate: uint,
        total-tokens-staked: uint,
        pool-status-active: bool,
        minimum-stake-amount: uint,
        maximum-stake-amount: uint
    }
)

;; User Pool Participation Tracking
(define-map user-pool-participation
    {participant-address: principal, pool-identifier: uint}
    uint
)

;; Governance Proposal Management
(define-map governance-proposal-details
    uint
    {
        proposal-creator: principal,
        proposal-start-block: uint,
        proposal-end-block: uint,
        proposal-description: (string-utf8 256),
        proposal-executed: bool,
        total-votes-in-favor: uint,
        total-votes-against: uint
    }
)

;; Feature Management Maps
(define-map yield-boost-eligible-nfts principal bool)
(define-map user-referral-earnings principal uint)
(define-map auto-compound-preferences principal bool)

;; Read-only Functions
(define-read-only (get-staking-pool-information (pool-identifier uint))
    (map-get? staking-pool-configurations pool-identifier)
)

(define-read-only (get-user-pool-stake-amount (user-address principal) (pool-identifier uint))
    (map-get? user-pool-participation 
        {participant-address: user-address, pool-identifier: pool-identifier})
)

(define-read-only (calculate-user-yield-boost (user-address principal))
    (let (
        (staking-position (unwrap! (get-user-staking-position user-address) (err u0)))
        (has-eligible-boost-nft (default-to false (map-get? yield-boost-eligible-nfts user-address)))
    )
    (if (and (get yield-boost-enabled staking-position) has-eligible-boost-nft)
        YIELD-BOOST-MULTIPLIER
        u100))
)

(define-read-only (calculate-effective-annual-rate (pool-identifier uint) (user-address principal))
    (let (
        (pool-config (unwrap! (get-staking-pool-information pool-identifier) (err u0)))
        (user-boost-multiplier (calculate-user-yield-boost user-address))
        (lock-duration-multiplier (calculate-lock-duration-bonus user-address))
    )
    (/ (* (get annual-percentage-rate pool-config) 
          (+ user-boost-multiplier lock-duration-multiplier)) 
       u100))
)

;; Enhanced Staking Functions
(define-public (stake-tokens-in-pool (pool-identifier uint) (stake-amount uint) (lock-duration uint))
    (let (
        (pool-config (unwrap! (get-staking-pool-information pool-identifier) 
                             ERROR-STAKING-POOL-NOT-FOUND))
        (current-staked-amount (default-to u0 
            (get-user-pool-stake-amount tx-sender pool-identifier)))
    )
    (asserts! (get pool-status-active pool-config) ERROR-STAKING-POOL-INACTIVE)
    (asserts! (>= stake-amount (get minimum-stake-amount pool-config)) 
              ERROR-INVALID-STAKE-AMOUNT)
    (asserts! (<= (+ stake-amount current-staked-amount) 
                  (get maximum-stake-amount pool-config)) 
              ERROR-INVALID-STAKE-AMOUNT)
    
    ;; Transfer tokens to contract
    (try! (contract-call? .fungible-token-trait transfer 
        stake-amount
        tx-sender
        (as-contract tx-sender)
        none
    ))
    
    ;; Update participation records
    (map-set user-pool-participation 
        {participant-address: tx-sender, pool-identifier: pool-identifier}
        (+ current-staked-amount stake-amount)
    )
    
    ;; Update pool statistics
    (map-set staking-pool-configurations pool-identifier
        (merge pool-config 
            {total-tokens-staked: (+ (get total-tokens-staked pool-config) stake-amount)})
    )
    
    (ok true))
)

;; Auto-compound Configuration
(define-public (configure-auto-compound-preference)
    (begin
        (map-set auto-compound-preferences tx-sender 
            (not (default-to false (map-get? auto-compound-preferences tx-sender))))
        (ok true))
)

(define-public (harvest-rewards-with-compound (pool-identifier uint))
    (let (
        (calculated-rewards (calculate-user-rewards tx-sender))
        (auto-compound-enabled (default-to false 
            (map-get? auto-compound-preferences tx-sender)))
    )
    (if auto-compound-enabled
        (stake-tokens-in-pool pool-identifier calculated-rewards 
                             (var-get minimum-staking-period))
        (claim-staking-rewards))
)

;; Governance System Functions
(define-public (submit-governance-proposal 
    (proposal-description (string-utf8 256)) 
    (voting-duration uint))
    (let (
        (proposal-identifier (var-get proposal-counter))
    )
    (asserts! (>= (contract-call? .governance-token get-balance tx-sender) 
                  (var-get minimum-proposal-token-requirement))
              ERROR-INSUFFICIENT-TOKEN-BALANCE)
    
    (map-set governance-proposal-details proposal-identifier
        {
            proposal-creator: tx-sender,
            proposal-start-block: block-height,
            proposal-end-block: (+ block-height voting-duration),
            proposal-description: proposal-description,
            proposal-executed: false,
            total-votes-in-favor: u0,
            total-votes-against: u0
        }
    )
    
    (var-set proposal-counter (+ proposal-identifier u1))
    (ok proposal-identifier))
)

;; NFT Boost System Functions
(define-public (register-yield-boost-nft (nft-contract-address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-administrator)) 
                 ERROR-UNAUTHORIZED-ACCESS)
        (map-set yield-boost-eligible-nfts nft-contract-address true)
        (ok true))
)

(define-public (activate-yield-boost)
    (let (
        (staking-position (unwrap! (get-user-staking-position tx-sender) 
                                  ERROR-STAKE-NOT-FOUND))
        (has-eligible-nft (default-to false 
            (map-get? yield-boost-eligible-nfts tx-sender)))
    )
    (asserts! has-eligible-nft ERROR-YIELD-BOOST-NOT-ACTIVE)
    
    (map-set user-staking-positions tx-sender
        (merge staking-position {yield-boost-enabled: true}))
    (ok true))
)

;; Private Helper Functions
(define-private (calculate-lock-duration-bonus (user-address principal))
    (let (
        (staking-position (unwrap! (get-user-staking-position user-address) (err u0)))
        (lock-duration (get staking-lock-duration staking-position))
        (base-yield-multiplier u100)
    )
    (if (>= lock-duration (* (var-get minimum-staking-period) u4))
        MAXIMUM-LOCK-DURATION-BONUS
        (+ base-yield-multiplier 
           (/ (* (- MAXIMUM-LOCK-DURATION_BONUS base-yield-multiplier) 
                 lock-duration)
              (* (var-get minimum-staking-period) u4)))))
)

;; Pool Management Functions
(define-public (suspend-staking-pool (pool-identifier uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-administrator)) 
                 ERROR-UNAUTHORIZED-ACCESS)
        (let ((pool-config (unwrap! (get-staking-pool-information pool-identifier) 
                                   ERROR-STAKING-POOL-NOT-FOUND)))
            (map-set staking-pool-configurations pool-identifier
                (merge pool-config {pool-status-active: false}))
            (ok true)))
)

(define-public (reactivate-staking-pool (pool-identifier uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-administrator)) 
                 ERROR-UNAUTHORIZED-ACCESS)
        (let ((pool-config (unwrap! (get-staking-pool-information pool-identifier) 
                                   ERROR-STAKING-POOL-NOT-FOUND)))
            (map-set staking-pool-configurations pool-identifier
                (merge pool-config {pool-status-active: true}))
            (ok true)))
)