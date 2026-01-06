;;; FITCYL.LSP
;;; Plugin for approximating a cylinder from two point sets (sections)
;;; 

(defun c:FITCYL (/ pts1 pts2 circ1 circ2 axis-z axis-x axis-y 
                   center1 center2 mat-wcs-to-ucs new-pts1 new-pts2
                   new-circ1 new-circ2 final-c1 final-c2 final-r
                   mat-ucs-to-wcs old-osmode old-3dosmode old-cmdecho
                   global-center pts1-local pts2-local doc
                   *error*)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Error Handler
  (defun *error* (msg)
    (if old-osmode (setvar "OSMODE" old-osmode))
    (if old-3dosmode (setvar "3DOSMODE" old-3dosmode))
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho))
    (command "_.UCS" "_Previous") ; Restore UCS
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (vla-EndUndoMark doc) ; End Undo Group
    (princ)
  )
  
  ;; Start Undo Group
  (vla-StartUndoMark doc)

  ;; 1. Get Points (These are WCS from entget)
  (prompt "\nSelect points for First Section:")
  (setq pts1 (get-points-sel))
  (if (< (length pts1) 3)
    (progn (alert "Need at least 3 points for section 1.") (exit))
  )
  
  (prompt "\nSelect points for Second Section:")
  (setq pts2 (get-points-sel))
  (if (< (length pts2) 3)
    (progn (alert "Need at least 3 points for section 2.") (exit))
  )

  ;; 1.5 Coordinate Normalization (Shift to Global Center) to improve precision
  (setq global-center (get-centroid (append pts1 pts2)))
  (setq pts1-local (mapcar '(lambda (p) (mapcar '- p global-center)) pts1))
  (setq pts2-local (mapcar '(lambda (p) (mapcar '- p global-center)) pts2))
  
  ;; Replace original points with local ones for calculation
  (setq pts1 pts1-local)
  (setq pts2 pts2-local)

  ;; 2. First Approximation (3D Circle Fit)
  (prompt "\nCalculating first approximation...")
  (setq circ1 (fit-circle-3d pts1))
  (setq circ2 (fit-circle-3d pts2))
  
  (setq center1 (car circ1)) ; (x y z)
  (setq center2 (car circ2))
  
  ;; 3. Define Coordinate System (Z axis = C1 -> C2)
  (setq axis-z (normalize (mapcar '- center2 center1)))
  
  ;; Arbitrary X axis perpendicular to Z
  (setq axis-x (arbitrary-axis axis-z))
  (setq axis-y (cross-product axis-z axis-x))
  
  ;; Transformation Matrix (WCS -> Local)
  ;; Origin is center1 (arbitrary, but useful)
  (setq mat-wcs-to-ucs (make-transform-matrix center1 axis-x axis-y axis-z))
  
  ;; 4. Second Approximation (Project to local XY and fit)
  (prompt "\nCalculating second approximation...")
  (setq new-pts1 (mapcar '(lambda (p) (transform-point p mat-wcs-to-ucs)) pts1))
  (setq new-pts2 (mapcar '(lambda (p) (transform-point p mat-wcs-to-ucs)) pts2))
  
  ;; Fit 2D circles in the local XY plane
  (setq new-circ1 (fit-circle-2d-with-z new-pts1)) ;; Returns ((x y z) R)
  (setq new-circ2 (fit-circle-2d-with-z new-pts2))
  
  ;; 5. Construct Cylinder Parameters
  ;; Average Radius
  (setq final-r (/ (+ (cadr new-circ1) (cadr new-circ2)) 2.0))
  
  ;; Transform local centers back to Calculation System
  (setq mat-ucs-to-wcs (invert-matrix mat-wcs-to-ucs))
  (setq final-c1 (transform-point (car new-circ1) mat-ucs-to-wcs))
  (setq final-c2 (transform-point (car new-circ2) mat-ucs-to-wcs))
  
  ;; Shift back to Global Coordinates
  (setq final-c1 (mapcar '+ final-c1 global-center))
  (setq final-c2 (mapcar '+ final-c2 global-center))

  ;; 6. Build Cylinder
  ;; Save Settings
  (setq old-osmode (getvar "OSMODE"))
  (setq old-3dosmode (getvar "3DOSMODE"))
  (setq old-cmdecho (getvar "CMDECHO"))
  
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "3DOSMODE" 0)
  
  ;; Switch to WCS to ensure coordinates match
  (command "_.UCS" "_World")
  
  ;; Using command: _cylinder -> Center -> Radius -> Axis Endpoint -> P2
  (command "_cylinder" "_non" final-c1 final-r "_a" "_non" final-c2)
  
  ;; Restore Settings
  (command "_.UCS" "_Previous")
  (setvar "OSMODE" old-osmode)
  (setvar "3DOSMODE" old-3dosmode)
  (setvar "CMDECHO" old-cmdecho)
  
  (vla-EndUndoMark doc) ; End Undo Group
  
  (princ "\nCylinder created.")
  (princ)
)

(defun c:FITCON (/ pts1 pts2 circ1 circ2 axis-z axis-x axis-y 
                   center1 center2 mat-wcs-to-ucs new-pts1 new-pts2
                   new-circ1 new-circ2 final-c1 final-c2 final-r1 final-r2
                   mat-ucs-to-wcs old-osmode old-3dosmode old-cmdecho
                   global-center pts1-local pts2-local doc
                   *error*)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Error Handler
  (defun *error* (msg)
    (if old-osmode (setvar "OSMODE" old-osmode))
    (if old-3dosmode (setvar "3DOSMODE" old-3dosmode))
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho))
    (command "_.UCS" "_Previous") ; Restore UCS
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (vla-EndUndoMark doc) ; End Undo Group
    (princ)
  )
  
  ;; Start Undo Group
  (vla-StartUndoMark doc)

  ;; 1. Get Points
  (prompt "\nSelect points for Base Section (Start):")
  (setq pts1 (get-points-sel))
  (if (< (length pts1) 3)
    (progn (alert "Need at least 3 points for section 1.") (exit))
  )
  
  (prompt "\nSelect points for Top Section (End):")
  (setq pts2 (get-points-sel))
  (if (< (length pts2) 3)
    (progn (alert "Need at least 3 points for section 2.") (exit))
  )

  ;; 1.5 Coordinate Normalization
  (setq global-center (get-centroid (append pts1 pts2)))
  (setq pts1-local (mapcar '(lambda (p) (mapcar '- p global-center)) pts1))
  (setq pts2-local (mapcar '(lambda (p) (mapcar '- p global-center)) pts2))
  
  (setq pts1 pts1-local)
  (setq pts2 pts2-local)

  ;; 2. First Approximation
  (prompt "\nCalculating first approximation...")
  (setq circ1 (fit-circle-3d pts1))
  (setq circ2 (fit-circle-3d pts2))
  
  (setq center1 (car circ1))
  (setq center2 (car circ2))
  
  ;; 3. Define Coordinate System
  (setq axis-z (normalize (mapcar '- center2 center1)))
  (setq axis-x (arbitrary-axis axis-z))
  (setq axis-y (cross-product axis-z axis-x))
  
  (setq mat-wcs-to-ucs (make-transform-matrix center1 axis-x axis-y axis-z))
  
  ;; 4. Second Approximation
  (prompt "\nCalculating second approximation...")
  (setq new-pts1 (mapcar '(lambda (p) (transform-point p mat-wcs-to-ucs)) pts1))
  (setq new-pts2 (mapcar '(lambda (p) (transform-point p mat-wcs-to-ucs)) pts2))
  
  (setq new-circ1 (fit-circle-2d-with-z new-pts1))
  (setq new-circ2 (fit-circle-2d-with-z new-pts2))
  
  ;; 5. Construct Cone Parameters
  ;; Individual Radii
  (setq final-r1 (cadr new-circ1))
  (setq final-r2 (cadr new-circ2))
  
  ;; Transform local centers back
  (setq mat-ucs-to-wcs (invert-matrix mat-wcs-to-ucs))
  (setq final-c1 (transform-point (car new-circ1) mat-ucs-to-wcs))
  (setq final-c2 (transform-point (car new-circ2) mat-ucs-to-wcs))
  
  ;; Shift back to Global
  (setq final-c1 (mapcar '+ final-c1 global-center))
  (setq final-c2 (mapcar '+ final-c2 global-center))

  ;; 6. Build Cone
  (setq old-osmode (getvar "OSMODE"))
  (setq old-3dosmode (getvar "3DOSMODE"))
  (setq old-cmdecho (getvar "CMDECHO"))
  
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "3DOSMODE" 0)
  
  (command "_.UCS" "_World")
  
  ;; _cone -> Center -> Radius -> Axis Endpoint -> Radius(Top)
  (command "_cone" "_non" final-c1 final-r1 "_t" final-r2 "_a" "_non" final-c2)
  
  (command "_.UCS" "_Previous")
  (setvar "OSMODE" old-osmode)
  (setvar "3DOSMODE" old-3dosmode)
  (setvar "CMDECHO" old-cmdecho)
  
  (vla-EndUndoMark doc)
  
  (princ "\nCone created.")
  (princ)
)

(defun c:FITCIRS (/ pts1 pts2 circ1 circ2 axis-z axis-x axis-y 
                    center1 center2 mat-wcs-to-ucs new-pts1 new-pts2
                    new-circ1 new-circ2 final-c1 final-c2 final-r1 final-r2
                    mat-ucs-to-wcs old-osmode old-3dosmode old-cmdecho
                    global-center pts1-local pts2-local doc axis-vec
                    *error*)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Error Handler
  (defun *error* (msg)
    (if old-osmode (setvar "OSMODE" old-osmode))
    (if old-3dosmode (setvar "3DOSMODE" old-3dosmode))
    (if old-cmdecho (setvar "CMDECHO" old-cmdecho))
    (command "_.UCS" "_Previous") ; Restore UCS
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError: " msg))
    )
    (vla-EndUndoMark doc) ; End Undo Group
    (princ)
  )
  
  ;; Start Undo Group
  (vla-StartUndoMark doc)

  ;; 1. Get Points
  (prompt "\nSelect points for Base Section (Start):")
  (setq pts1 (get-points-sel))
  (if (< (length pts1) 3)
    (progn (alert "Need at least 3 points for section 1.") (exit))
  )
  
  (prompt "\nSelect points for Top Section (End):")
  (setq pts2 (get-points-sel))
  (if (< (length pts2) 3)
    (progn (alert "Need at least 3 points for section 2.") (exit))
  )

  ;; 1.5 Coordinate Normalization
  (setq global-center (get-centroid (append pts1 pts2)))
  (setq pts1-local (mapcar '(lambda (p) (mapcar '- p global-center)) pts1))
  (setq pts2-local (mapcar '(lambda (p) (mapcar '- p global-center)) pts2))
  
  (setq pts1 pts1-local)
  (setq pts2 pts2-local)

  ;; 2. First Approximation
  (prompt "\nCalculating first approximation...")
  (setq circ1 (fit-circle-3d pts1))
  (setq circ2 (fit-circle-3d pts2))
  
  (setq center1 (car circ1))
  (setq center2 (car circ2))
  
  ;; 3. Define Coordinate System
  (setq axis-z (normalize (mapcar '- center2 center1)))
  (setq axis-x (arbitrary-axis axis-z))
  (setq axis-y (cross-product axis-z axis-x))
  
  (setq mat-wcs-to-ucs (make-transform-matrix center1 axis-x axis-y axis-z))
  
  ;; 4. Second Approximation
  (prompt "\nCalculating second approximation...")
  (setq new-pts1 (mapcar '(lambda (p) (transform-point p mat-wcs-to-ucs)) pts1))
  (setq new-pts2 (mapcar '(lambda (p) (transform-point p mat-wcs-to-ucs)) pts2))
  
  (setq new-circ1 (fit-circle-2d-with-z new-pts1))
  (setq new-circ2 (fit-circle-2d-with-z new-pts2))
  
  ;; 5. Construct Geometry Parameters
  (setq final-r1 (cadr new-circ1))
  (setq final-r2 (cadr new-circ2))
  
  ;; Transform local centers back
  (setq mat-ucs-to-wcs (invert-matrix mat-wcs-to-ucs))
  (setq final-c1 (transform-point (car new-circ1) mat-ucs-to-wcs))
  (setq final-c2 (transform-point (car new-circ2) mat-ucs-to-wcs))
  
  ;; Shift back to Global
  (setq final-c1 (mapcar '+ final-c1 global-center))
  (setq final-c2 (mapcar '+ final-c2 global-center))
  
  ;; Calculate Axis Vector for Circle Normals
  ;; Normal for Circle 1 points to Circle 2 (or vice versa, to face each other)
  ;; Axis from C1 to C2
  (setq axis-vec (mapcar '- final-c2 final-c1))
  (setq axis-vec (normalize axis-vec))

  ;; 6. Build Geometry (Circles and Line)
  (setq old-osmode (getvar "OSMODE"))
  (setq old-3dosmode (getvar "3DOSMODE"))
  (setq old-cmdecho (getvar "CMDECHO"))
  
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (setvar "3DOSMODE" 0)
  
  (command "_.UCS" "_World")
  
  ;; Draw Line connecting centers (In WCS)
  (entmake (list (cons 0 "LINE")
                 (cons 10 final-c1)
                 (cons 11 final-c2)))
  
  ;; Draw Circle 1
  ;; For a Circle with non-standard Extrusion Vector (210), 
  ;; Center Point (10) must be in OCS (Object Coordinate System), not WCS.
  ;; We must transform the center from WCS to OCS.
  (entmake (list (cons 0 "CIRCLE")
                 (cons 10 (trans final-c1 0 axis-vec)) ; WCS -> OCS
                 (cons 40 final-r1)
                 (cons 210 axis-vec)))
                 
  ;; Draw Circle 2
  ;; For the second circle, the normal is opposite (-Axis)
  (entmake (list (cons 0 "CIRCLE")
                 (cons 10 (trans final-c2 0 (mapcar '- axis-vec))) ; WCS -> OCS
                 (cons 40 final-r2)
                 (cons 210 (mapcar '- axis-vec))))

  (command "_.UCS" "_Previous")
  (setvar "OSMODE" old-osmode)
  (setvar "3DOSMODE" old-3dosmode)
  (setvar "CMDECHO" old-cmdecho)
  
  (vla-EndUndoMark doc)
  
  (princ "\nCircles and axis line created.")
  (princ)
)


(defun get-centroid (pts / n cx cy cz)
  (setq n (length pts))
  (setq cx (/ (apply '+ (mapcar 'car pts)) (float n)))
  (setq cy (/ (apply '+ (mapcar 'cadr pts)) (float n)))
  (setq cz (/ (apply '+ (mapcar 'caddr pts)) (float n)))
  (list cx cy cz)
)


;;; ============================================================
;;; MATH UTILITIES
;;; ============================================================

(defun get-points-sel (/ ss i ename elist pts)
  (prompt "\nSelect POINT objects: ")
  (setq ss (ssget '((0 . "POINT"))))
  (setq pts '())
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i))
        (setq elist (entget ename))
        (setq pts (cons (cdr (assoc 10 elist)) pts))
        (setq i (1+ i))
      )
    )
    (prompt "\nNo points selected.")
  )
  (reverse pts)
)

;; Vector Operations
(defun normalize (v / len)
  (setq len (sqrt (apply '+ (mapcar '* v v))))
  (if (equal len 0.0 1e-9) v (mapcar '(lambda (x) (/ x len)) v))
)

(defun cross-product (a b)
  (list
    (- (* (cadr a) (caddr b)) (* (caddr a) (cadr b)))
    (- (* (caddr a) (car b)) (* (car a) (caddr b)))
    (- (* (car a) (cadr b)) (* (cadr a) (car b)))
  )
)

(defun dot-product (a b)
  (apply '+ (mapcar '* a b))
)

(defun arbitrary-axis (n)
  (if (< (abs (car n)) (abs (cadr n)))
    (normalize (cross-product n '(1.0 0.0 0.0)))
    (normalize (cross-product n '(0.0 1.0 0.0)))
  )
)

;; Matrix Operations (4x4)
;; Point is (x y z), treated as (x y z 1)
(defun transform-point (pt mat / x y z r1 r2 r3)
  (setq x (car pt)) 
  (setq y (cadr pt)) 
  (setq z (caddr pt))
  (setq r1 (nth 0 mat)) 
  (setq r2 (nth 1 mat)) 
  (setq r3 (nth 2 mat))
  (list
    (+ (* x (nth 0 r1)) (* y (nth 1 r1)) (* z (nth 2 r1)) (nth 3 r1))
    (+ (* x (nth 0 r2)) (* y (nth 1 r2)) (* z (nth 2 r2)) (nth 3 r2))
    (+ (* x (nth 0 r3)) (* y (nth 1 r3)) (* z (nth 2 r3)) (nth 3 r3))
  )
)

;; Construct WCS -> UCS matrix
(defun make-transform-matrix (org x y z / ox oy oz)
  (setq ox (car org)) 
  (setq oy (cadr org)) 
  (setq oz (caddr org))
  (list
    (list (car x) (cadr x) (caddr x) (- (dot-product x org)))
    (list (car y) (cadr y) (caddr y) (- (dot-product y org)))
    (list (car z) (cadr z) (caddr z) (- (dot-product z org)))
    '(0.0 0.0 0.0 1.0)
  )
)

;; Invert Matrix
(defun invert-matrix (m / r1 r2 r3 tx ty tz nx ny nz t-vec ntx nty ntz)
  (setq r1 (nth 0 m)) 
  (setq r2 (nth 1 m)) 
  (setq r3 (nth 2 m))
  (setq tx (nth 3 r1)) 
  (setq ty (nth 3 r2)) 
  (setq tz (nth 3 r3))
  (setq nx (list (nth 0 r1) (nth 0 r2) (nth 0 r3)))
  (setq ny (list (nth 1 r1) (nth 1 r2) (nth 1 r3)))
  (setq nz (list (nth 2 r1) (nth 2 r2) (nth 2 r3)))
  (setq t-vec (list tx ty tz))
  (setq ntx (- (dot-product nx t-vec)))
  (setq nty (- (dot-product ny t-vec)))
  (setq ntz (- (dot-product nz t-vec)))
  (list
    (append nx (list ntx))
    (append ny (list nty))
    (append nz (list ntz))
    '(0.0 0.0 0.0 1.0)
  )
)

;;; ============================================================
;;; FITTING LOGIC
;;; ============================================================

(defun fit-circle-3d (pts / plane centroid normal axis-u axis-v pts-2d circ-2d uc vc rad center-3d)
  (setq plane (fit-plane pts))
  (setq centroid (car plane))
  (setq normal (cadr plane))
  (setq axis-u (arbitrary-axis normal))
  (setq axis-v (cross-product normal axis-u))
  
  ;; Project to UV
  (setq pts-2d (mapcar '(lambda (p / vec)
                          (setq vec (mapcar '- p centroid))
                          (list (dot-product vec axis-u)
                                (dot-product vec axis-v)))
                        pts))
  
  ;; Fit 2D Circle
  (setq circ-2d (fit-circle-2d pts-2d))
  (setq uc (car (car circ-2d)))
  (setq vc (cadr (car circ-2d)))
  (setq rad (cadr circ-2d))
  
  ;; Transform center back to 3D
  (setq center-3d (mapcar '+ centroid
                          (mapcar '+ (mapcar '(lambda (x) (* x uc)) axis-u)
                                     (mapcar '(lambda (x) (* x vc)) axis-v))))
  (list center-3d rad)
)

;; Fit Plane (PCA Method)
(defun fit-plane (pts / n cx cy cz centroid xx xy xz yy yz zz cov v1 l1 cov2 v2 normal)
  (setq n (length pts))
  (setq cx (/ (apply '+ (mapcar 'car pts)) (float n)))
  (setq cy (/ (apply '+ (mapcar 'cadr pts)) (float n)))
  (setq cz (/ (apply '+ (mapcar 'caddr pts)) (float n)))
  (setq centroid (list cx cy cz))
  
  (setq xx 0.0 xy 0.0 xz 0.0 yy 0.0 yz 0.0 zz 0.0)
  
  (foreach p pts
    (setq dx (- (car p) cx))
    (setq dy (- (cadr p) cy))
    (setq dz (- (caddr p) cz))
    (setq xx (+ xx (* dx dx)))
    (setq xy (+ xy (* dx dy)))
    (setq xz (+ xz (* dx dz)))
    (setq yy (+ yy (* dy dy)))
    (setq yz (+ yz (* dy dz)))
    (setq zz (+ zz (* dz dz)))
  )
  
  ;; Covariance Matrix
  (setq cov (list (list xx xy xz) (list xy yy yz) (list xz yz zz)))
  
  ;; 1. Find Primary Axis
  (setq v1 (power-iteration cov 20))
  (setq l1 (v-m-v cov v1))
  
  ;; 2. Deflate Matrix
  (setq cov2 (deflate-matrix cov v1 l1))
  
  ;; 3. Find Secondary Axis
  (setq v2 (power-iteration cov2 20))
  
  ;; 4. Normal is Cross Product
  (setq normal (normalize (cross-product v1 v2)))
  
  (list centroid normal)
)

(defun power-iteration (m iter / v)
  (setq v '(1.0 1.0 1.0))
  (repeat iter
    (setq v (mxv m v))
    (setq v (normalize v))
  )
  v
)

(defun mxv (m v)
  (list
    (dot-product (nth 0 m) v)
    (dot-product (nth 1 m) v)
    (dot-product (nth 2 m) v)
  )
)

(defun v-m-v (m v)
  (dot-product v (mxv m v))
)

(defun deflate-matrix (m v lam / vx vy vz)
  (setq vx (car v)) 
  (setq vy (cadr v)) 
  (setq vz (caddr v))
  (list
    (list (- (nth 0 (nth 0 m)) (* lam vx vx))
          (- (nth 1 (nth 0 m)) (* lam vx vy))
          (- (nth 2 (nth 0 m)) (* lam vx vz)))
    (list (- (nth 0 (nth 1 m)) (* lam vy vx))
          (- (nth 1 (nth 1 m)) (* lam vy vy))
          (- (nth 2 (nth 1 m)) (* lam vy vz)))
    (list (- (nth 0 (nth 2 m)) (* lam vz vx))
          (- (nth 1 (nth 2 m)) (* lam vz vy))
          (- (nth 2 (nth 2 m)) (* lam vz vz)))
  )
)


;; Fit 2D Circle (Kasa Method)
(defun fit-circle-2d (pts / sum-x sum-y sum-x2 sum-y2 sum-xy sum-x3 sum-y3 sum-xy2 sum-x2y n mtm mty sol A B C xc yc rr rad)
  (setq sum-x 0.0 sum-y 0.0 sum-x2 0.0 sum-y2 0.0 
        sum-xy 0.0 sum-x3 0.0 sum-y3 0.0 sum-xy2 0.0 sum-x2y 0.0)
  (setq n (length pts))
  
  (foreach p pts
    (setq x (car p)) 
    (setq y (cadr p)) 
    ;; (z (if (caddr p) (caddr p) 0.0)) - unused here
    (setq sum-x (+ sum-x x))
    (setq sum-y (+ sum-y y))
    (setq sum-x2 (+ sum-x2 (* x x)))
    (setq sum-y2 (+ sum-y2 (* y y)))
    (setq sum-xy (+ sum-xy (* x y)))
    (setq sum-x3 (+ sum-x3 (* x x x)))
    (setq sum-y3 (+ sum-y3 (* y y y)))
    (setq sum-xy2 (+ sum-xy2 (* x y y)))
    (setq sum-x2y (+ sum-x2y (* x x y)))
  )
  
  (setq mtm (list
              (list sum-x2 sum-xy sum-x)
              (list sum-xy sum-y2 sum-y)
              (list sum-x  sum-y  (float n))))
  
  (setq mty (list
              (+ sum-x3 sum-xy2)
              (+ sum-x2y sum-y3)
              (+ sum-x2 sum-y2)))
  
  (setq sol (solve-linear-3x3 mtm mty))
  
  (if sol
    (progn
      (setq A (car sol))
      (setq B (cadr sol))
      (setq C (caddr sol))
      (setq xc (/ A 2.0))
      (setq yc (/ B 2.0))
      (setq rr (+ C (* xc xc) (* yc yc)))
      (setq rad (sqrt (abs rr)))
      (list (list xc yc) rad)
    )
    (list '(0.0 0.0) 0.0) ; Fail safe
  )
)

(defun solve-linear-3x3 (mat vec / d d1 d2 d3)
  (setq d (det-3x3 mat))
  (if (equal d 0.0 1e-9)
    nil
    (progn
      (setq d1 (det-3x3 (replace-col mat 0 vec)))
      (setq d2 (det-3x3 (replace-col mat 1 vec)))
      (setq d3 (det-3x3 (replace-col mat 2 vec)))
      (list (/ d1 d) (/ d2 d) (/ d3 d))
    )
  )
)

(defun det-3x3 (m / r1 r2 r3)
  (setq r1 (nth 0 m)) 
  (setq r2 (nth 1 m)) 
  (setq r3 (nth 2 m))
  (+ (* (nth 0 r1) (- (* (nth 1 r2) (nth 2 r3)) (* (nth 2 r2) (nth 1 r3))))
     (* (nth 1 r1) (- (* (nth 2 r2) (nth 0 r3)) (* (nth 0 r2) (nth 2 r3)))) ; Minus term
     (* (nth 2 r1) (- (* (nth 0 r2) (nth 1 r3)) (* (nth 1 r2) (nth 0 r3))))
  )
)

(defun replace-nth (n val lst)
  (if (= n 0)
    (cons val (cdr lst))
    (cons (car lst) (replace-nth (1- n) val (cdr lst)))
  )
)

(defun replace-col (mat col vec)
  (list
    (replace-nth col (nth 0 vec) (nth 0 mat))
    (replace-nth col (nth 1 vec) (nth 1 mat))
    (replace-nth col (nth 2 vec) (nth 2 mat))
  )
)

(defun fit-circle-2d-with-z (pts / circ center rad avg-z)
  (setq circ (fit-circle-2d pts))
  (setq center (car circ))
  (setq rad (cadr circ))
  (setq avg-z (/ (apply '+ (mapcar 'caddr pts)) (float (length pts))))
  (list (list (car center) (cadr center) avg-z) rad)
)

(princ "\nFITCYL command loaded. Type FITCYL to run.")
(princ)
