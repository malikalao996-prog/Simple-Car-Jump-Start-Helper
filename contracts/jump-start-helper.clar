;; Simple Car Jump-Start Helper
;; Roadside assistance coordination for dead battery emergencies

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_ALREADY_ASSIGNED (err u103))

;; Helper status enum
(define-constant STATUS_AVAILABLE u1)
(define-constant STATUS_BUSY u2)
(define-constant STATUS_OFFLINE u3)

;; Request status enum
(define-constant REQUEST_PENDING u1)
(define-constant REQUEST_ASSIGNED u2)
(define-constant REQUEST_COMPLETED u3)
(define-constant REQUEST_CANCELLED u4)

;; Helper registration data
(define-map helpers principal {
    status: uint,
    equipment: (string-ascii 200),
    location: (string-ascii 100),
    rating: uint,
    completed-jobs: uint,
    registered-at: uint
})

;; Emergency requests
(define-map requests uint {
    requester: principal,
    location: (string-ascii 100),
    description: (string-ascii 300),
    assigned-helper: (optional principal),
    status: uint,
    created-at: uint,
    completed-at: (optional uint)
})

(define-data-var next-request-id uint u1)

;; Register as a helper
(define-public (register-helper (equipment (string-ascii 200)) (location (string-ascii 100)))
    (begin
        (map-set helpers tx-sender {
            status: STATUS_AVAILABLE,
            equipment: equipment,
            location: location,
            rating: u5,
            completed-jobs: u0,
            registered-at: stacks-block-height
        })
        (ok true)
    )
)

;; Update helper status
(define-public (update-helper-status (status uint))
    (let ((helper (unwrap! (map-get? helpers tx-sender) ERR_NOT_FOUND)))
        (asserts! (or (is-eq status STATUS_AVAILABLE)
                     (is-eq status STATUS_BUSY)
                     (is-eq status STATUS_OFFLINE)) ERR_INVALID_STATUS)
        (map-set helpers tx-sender (merge helper { status: status }))
        (ok true)
    )
)

;; Create assistance request
(define-public (create-request (location (string-ascii 100)) (description (string-ascii 300)))
    (let ((request-id (var-get next-request-id)))
        (map-set requests request-id {
            requester: tx-sender,
            location: location,
            description: description,
            assigned-helper: none,
            status: REQUEST_PENDING,
            created-at: stacks-block-height,
            completed-at: none
        })
        (var-set next-request-id (+ request-id u1))
        (ok request-id)
    )
)

;; Accept assistance request (helper only)
(define-public (accept-request (request-id uint))
    (let ((request (unwrap! (map-get? requests request-id) ERR_NOT_FOUND))
          (helper (unwrap! (map-get? helpers tx-sender) ERR_NOT_FOUND)))
        (asserts! (is-eq (get status helper) STATUS_AVAILABLE) ERR_INVALID_STATUS)
        (asserts! (is-eq (get status request) REQUEST_PENDING) ERR_ALREADY_ASSIGNED)

        ;; Update request with assigned helper
        (map-set requests request-id (merge request {
            assigned-helper: (some tx-sender),
            status: REQUEST_ASSIGNED
        }))

        ;; Update helper status to busy
        (map-set helpers tx-sender (merge helper { status: STATUS_BUSY }))
        (ok true)
    )
)

;; Complete assistance job
(define-public (complete-request (request-id uint))
    (let ((request (unwrap! (map-get? requests request-id) ERR_NOT_FOUND))
          (helper (unwrap! (map-get? helpers tx-sender) ERR_NOT_FOUND)))
        (asserts! (is-eq (some tx-sender) (get assigned-helper request)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status request) REQUEST_ASSIGNED) ERR_INVALID_STATUS)

        ;; Update request as completed
        (map-set requests request-id (merge request {
            status: REQUEST_COMPLETED,
            completed-at: (some stacks-block-height)
        }))

        ;; Update helper stats and status
        (map-set helpers tx-sender (merge helper {
            status: STATUS_AVAILABLE,
            completed-jobs: (+ (get completed-jobs helper) u1)
        }))
        (ok true)
    )
)

;; Cancel request (requester only)
(define-public (cancel-request (request-id uint))
    (let ((request (unwrap! (map-get? requests request-id) ERR_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get requester request)) ERR_UNAUTHORIZED)
        (asserts! (not (is-eq (get status request) REQUEST_COMPLETED)) ERR_INVALID_STATUS)

        ;; Free up assigned helper if any
        (match (get assigned-helper request)
            assigned-helper-principal
                (let ((helper (unwrap! (map-get? helpers assigned-helper-principal) ERR_NOT_FOUND)))
                    (map-set helpers assigned-helper-principal (merge helper { status: STATUS_AVAILABLE }))
                )
            true
        )

        (map-set requests request-id (merge request { status: REQUEST_CANCELLED }))
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-helper (helper-principal principal))
    (map-get? helpers helper-principal)
)

(define-read-only (get-request (request-id uint))
    (map-get? requests request-id)
)

(define-read-only (get-available-helpers)
    (ok "Use off-chain indexing for available helpers list")
)
