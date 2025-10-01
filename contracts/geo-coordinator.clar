;; geo-coordinator
;; Climate Engineering Coordination - Global geoengineering projects with consensus mechanisms

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-PROJECT-NOT-ACTIVE (err u105))
(define-constant ERR-CONSENSUS-REQUIRED (err u106))
(define-constant ERR-VERIFICATION-FAILED (err u107))
(define-constant ERR-FUNDING-INSUFFICIENT (err u108))
(define-constant ERR-IMPACT-DISPUTED (err u109))
(define-constant ERR-EMERGENCY-PROTOCOL (err u110))

;; data maps and vars
(define-map climate-projects
  { project-id: uint }
  {
    coordinator: principal,
    project-name: (string-ascii 100),
    project-type: (string-ascii 50),
    target-location: (string-ascii 100),
    estimated-impact: uint,
    required-funding: uint,
    current-funding: uint,
    consensus-score: uint,
    status: (string-ascii 30),
    created-at: uint,
    start-date: (optional uint),
    completion-date: (optional uint)
  }
)

(define-map consensus-votes
  { project-id: uint, voter: principal }
  {
    vote-type: (string-ascii 20),
    vote-weight: uint,
    voting-power: uint,
    justification: (string-ascii 200),
    voted-at: uint,
    voter-credentials: (string-ascii 50)
  }
)

(define-map impact-measurements
  { project-id: uint, measurement-id: uint }
  {
    measurement-type: (string-ascii 50),
    measured-value: uint,
    expected-value: uint,
    variance-percentage: uint,
    measurement-date: uint,
    verifier: principal,
    verification-method: (string-ascii 50),
    disputed: bool
  }
)

(define-map carbon-removal-claims
  { claim-id: uint }
  {
    project-id: uint,
    claimant: principal,
    co2-removed-tons: uint,
    verification-method: (string-ascii 50),
    verification-score: uint,
    claim-date: uint,
    verified: bool,
    credits-issued: uint
  }
)

(define-map climate-credits
  { project-id: uint, holder: principal }
  {
    credit-amount: uint,
    credit-type: (string-ascii 30),
    issue-date: uint,
    expiry-date: (optional uint),
    tradeable: bool,
    verification-hash: (string-ascii 128)
  }
)

(define-map international-authorities
  { authority-id: uint }
  {
    authority-name: (string-ascii 100),
    country-code: (string-ascii 10),
    representative: principal,
    voting-power: uint,
    specialization: (string-ascii 50),
    active: bool,
    registered-at: uint
  }
)

(define-map emergency-protocols
  { emergency-id: uint }
  {
    trigger-condition: (string-ascii 100),
    response-action: (string-ascii 200),
    activated: bool,
    activation-date: (optional uint),
    deactivation-date: (optional uint),
    authority-threshold: uint
  }
)

;; Global counters and vars
(define-data-var next-project-id uint u1)
(define-data-var next-measurement-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var next-authority-id uint u1)
(define-data-var next-emergency-id uint u1)
(define-data-var total-active-projects uint u0)
(define-data-var total-co2-removed uint u0)
(define-data-var global-climate-fund uint u0)
(define-data-var consensus-threshold uint u60) ;; 60% consensus required

;; private functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (get-project-by-id (project-id uint))
  (map-get? climate-projects { project-id: project-id })
)

(define-private (calculate-consensus-score (project-id uint))
  ;; Simplified consensus calculation - in reality would aggregate all votes
  (let ((base-score u50))
    (if (> project-id u5)
      (+ base-score u20) ;; Higher consensus for newer projects
      base-score
    )
  )
)

(define-private (is-consensus-achieved (project-id uint))
  (let ((project (unwrap! (get-project-by-id project-id) false)))
    (>= (get consensus-score project) (var-get consensus-threshold))
  )
)

(define-private (update-climate-fund (amount uint) (add bool))
  (if add
    (var-set global-climate-fund (+ (var-get global-climate-fund) amount))
    (if (>= (var-get global-climate-fund) amount)
      (var-set global-climate-fund (- (var-get global-climate-fund) amount))
      false
    )
  )
)

(define-private (calculate-impact-variance (measured uint) (expected uint))
  (if (> expected u0)
    (if (>= measured expected)
      (/ (* (- measured expected) u100) expected)
      (/ (* (- expected measured) u100) expected)
    )
    u0
  )
)

;; public functions
(define-public (register-climate-project
    (project-name (string-ascii 100))
    (project-type (string-ascii 50))
    (target-location (string-ascii 100))
    (estimated-impact uint)
    (required-funding uint)
  )
  (let
    (
      (project-id (var-get next-project-id))
    )
    (asserts! (> estimated-impact u0) ERR-INVALID-AMOUNT)
    (asserts! (> required-funding u0) ERR-INVALID-AMOUNT)
    
    (map-set climate-projects
      { project-id: project-id }
      {
        coordinator: tx-sender,
        project-name: project-name,
        project-type: project-type,
        target-location: target-location,
        estimated-impact: estimated-impact,
        required-funding: required-funding,
        current-funding: u0,
        consensus-score: u0,
        status: "proposed",
        created-at: block-height,
        start-date: none,
        completion-date: none
      }
    )
    
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (vote-on-project
    (project-id uint)
    (vote-type (string-ascii 20))
    (vote-weight uint)
    (justification (string-ascii 200))
  )
  (let
    (
      (project (unwrap! (get-project-by-id project-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get status project) "proposed") ERR-PROJECT-NOT-ACTIVE)
    (asserts! (<= vote-weight u100) ERR-INVALID-AMOUNT)
    
    (map-set consensus-votes
      { project-id: project-id, voter: tx-sender }
      {
        vote-type: vote-type,
        vote-weight: vote-weight,
        voting-power: u10, ;; Default voting power
        justification: justification,
        voted-at: block-height,
        voter-credentials: "verified"
      }
    )
    
    ;; Update consensus score
    (let ((new-consensus-score (calculate-consensus-score project-id)))
      (map-set climate-projects
        { project-id: project-id }
        (merge project { consensus-score: new-consensus-score })
      )
      
      ;; Check if consensus achieved
      (if (>= new-consensus-score (var-get consensus-threshold))
        (map-set climate-projects
          { project-id: project-id }
          (merge project { 
            status: "approved",
            consensus-score: new-consensus-score
          })
        )
        true
      )
    )
    
    (ok true)
  )
)

(define-public (fund-climate-project
    (project-id uint)
    (funding-amount uint)
  )
  (let
    (
      (project (unwrap! (get-project-by-id project-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get status project) "approved") ERR-CONSENSUS-REQUIRED)
    (asserts! (> funding-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= (+ (get current-funding project) funding-amount) 
                  (get required-funding project)) ERR-INVALID-AMOUNT)
    
    (map-set climate-projects
      { project-id: project-id }
      (merge project { current-funding: (+ (get current-funding project) funding-amount) })
    )
    
    ;; Check if fully funded
    (if (>= (+ (get current-funding project) funding-amount) (get required-funding project))
      (begin
        (map-set climate-projects
          { project-id: project-id }
          (merge project { 
            status: "active",
            start-date: (some block-height),
            current-funding: (+ (get current-funding project) funding-amount)
          })
        )
        (var-set total-active-projects (+ (var-get total-active-projects) u1))
      )
      true
    )
    
    (update-climate-fund funding-amount true)
    (ok true)
  )
)

(define-public (record-impact-measurement
    (project-id uint)
    (measurement-type (string-ascii 50))
    (measured-value uint)
    (expected-value uint)
    (verification-method (string-ascii 50))
  )
  (let
    (
      (measurement-id (var-get next-measurement-id))
      (project (unwrap! (get-project-by-id project-id) ERR-NOT-FOUND))
      (variance (calculate-impact-variance measured-value expected-value))
    )
    (asserts! (is-eq (get status project) "active") ERR-PROJECT-NOT-ACTIVE)
    (asserts! (or 
      (is-eq (get coordinator project) tx-sender)
      (is-contract-owner)
    ) ERR-UNAUTHORIZED)
    
    (map-set impact-measurements
      { project-id: project-id, measurement-id: measurement-id }
      {
        measurement-type: measurement-type,
        measured-value: measured-value,
        expected-value: expected-value,
        variance-percentage: variance,
        measurement-date: block-height,
        verifier: tx-sender,
        verification-method: verification-method,
        disputed: false
      }
    )
    
    (var-set next-measurement-id (+ measurement-id u1))
    (ok measurement-id)
  )
)

(define-public (submit-carbon-removal-claim
    (project-id uint)
    (co2-removed-tons uint)
    (verification-method (string-ascii 50))
  )
  (let
    (
      (claim-id (var-get next-claim-id))
      (project (unwrap! (get-project-by-id project-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get status project) "active") ERR-PROJECT-NOT-ACTIVE)
    (asserts! (> co2-removed-tons u0) ERR-INVALID-AMOUNT)
    
    (map-set carbon-removal-claims
      { claim-id: claim-id }
      {
        project-id: project-id,
        claimant: tx-sender,
        co2-removed-tons: co2-removed-tons,
        verification-method: verification-method,
        verification-score: u85, ;; Default verification score
        claim-date: block-height,
        verified: false,
        credits-issued: u0
      }
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (verify-carbon-claim
    (claim-id uint)
    (verification-score uint)
  )
  (let
    (
      (claim (unwrap! (map-get? carbon-removal-claims { claim-id: claim-id }) ERR-NOT-FOUND))
    )
    (asserts! (is-contract-owner) ERR-UNAUTHORIZED) ;; Only authorized verifiers
    (asserts! (<= verification-score u100) ERR-INVALID-AMOUNT)
    
    (if (>= verification-score u70) ;; 70% verification threshold
      (begin
        ;; Issue climate credits
        (let ((credits-to-issue (get co2-removed-tons claim)))
          (map-set carbon-removal-claims
            { claim-id: claim-id }
            (merge claim { 
              verified: true,
              verification-score: verification-score,
              credits-issued: credits-to-issue
            })
          )
          
          ;; Update global CO2 removed
          (var-set total-co2-removed (+ (var-get total-co2-removed) credits-to-issue))
          
          ;; Issue credits to claimant
          (map-set climate-credits
            { project-id: (get project-id claim), holder: (get claimant claim) }
            {
              credit-amount: credits-to-issue,
              credit-type: "carbon-removal",
              issue-date: block-height,
              expiry-date: none,
              tradeable: true,
              verification-hash: (concat "verified-" (int-to-ascii claim-id))
            }
          )
        )
      )
      (map-set carbon-removal-claims
        { claim-id: claim-id }
        (merge claim { verification-score: verification-score })
      )
    )
    
    (ok true)
  )
)

(define-public (complete-project
    (project-id uint)
    (final-impact uint)
  )
  (let
    (
      (project (unwrap! (get-project-by-id project-id) ERR-NOT-FOUND))
    )
    (asserts! (is-eq (get coordinator project) tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status project) "active") ERR-PROJECT-NOT-ACTIVE)
    
    (map-set climate-projects
      { project-id: project-id }
      (merge project {
        status: "completed",
        completion-date: (some block-height),
        estimated-impact: final-impact
      })
    )
    
    (var-set total-active-projects (- (var-get total-active-projects) u1))
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-project-details (project-id uint))
  (map-get? climate-projects { project-id: project-id })
)

(define-read-only (get-consensus-vote (project-id uint) (voter principal))
  (map-get? consensus-votes { project-id: project-id, voter: voter })
)

(define-read-only (get-impact-measurement (project-id uint) (measurement-id uint))
  (map-get? impact-measurements { project-id: project-id, measurement-id: measurement-id })
)

(define-read-only (get-carbon-claim (claim-id uint))
  (map-get? carbon-removal-claims { claim-id: claim-id })
)

(define-read-only (get-climate-credits (project-id uint) (holder principal))
  (map-get? climate-credits { project-id: project-id, holder: holder })
)

(define-read-only (get-global-stats)
  {
    total-active-projects: (var-get total-active-projects),
    total-co2-removed: (var-get total-co2-removed),
    global-climate-fund: (var-get global-climate-fund),
    consensus-threshold: (var-get consensus-threshold),
    next-project-id: (var-get next-project-id)
  }
)

(define-read-only (project-has-consensus (project-id uint))
  (is-consensus-achieved project-id)
)
