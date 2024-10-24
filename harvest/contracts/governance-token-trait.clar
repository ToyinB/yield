;; Governance Token Trait Definition
;; File: contracts/governance-token-trait.clar

(define-trait governance-trait
    (
        ;; Get the voting power of a principal
        (get-voting-power (principal) (response uint uint))

        ;; Get the total voting power
        (get-total-voting-power () (response uint uint))

        ;; Lock tokens for voting
        (lock-tokens (uint uint) (response bool uint))

        ;; Get balance of tokens owned by principal
        (get-balance (principal) (response uint uint))
    )
)