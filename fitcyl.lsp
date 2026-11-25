;;; =========================================================================
;;; FITCYL.LSP  (refactored)
;;; Аппроксимация цилиндра, конуса и окружностей по точкам
;;;
;;; Команды:
;;;   FITCYL    - цилиндр по двум сечениям
;;;   FITCON    - конус по двум сечениям
;;;   FITCIRS   - две окружности + ось
;;;   FITCIR3D  - произвольная окружность в 3D
;;;   FITCIR2D  - окружность, параллельная плоскости XY
;;;
;;; Особенности:
;;;   - Нормализация данных
;;;   - Трёхэтапный алгоритм + опциональный 4-й этап с фиксированными радиусами
;;;   - Прозрачный выбор точек
;;;   - Минимальное дублирование кода
;;; =========================================================================

(vl-load-com)

;;; =========================================================================
;;; ОБЩИЕ ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ УПРАВЛЕНИЯ
;;; =========================================================================

(defun fit:save-vars ( / )
  ;; Сохраняет системные переменные и возвращает список
  (list
    (getvar "OSMODE")
    (getvar "3DOSMODE")
    (getvar "CMDECHO")
  )
)

(defun fit:restore-vars (vars)
  ;; Восстанавливает системные переменные
  (if vars
    (progn
      (setvar "OSMODE"   (nth 0 vars))
      (setvar "3DOSMODE" (nth 1 vars))
      (setvar "CMDECHO"  (nth 2 vars))
    )
  )
)

(defun fit:prepare-build ( / )
  ;; Обнуляет привязки и CMDECHO перед построением
  (setvar "CMDECHO"  0)
  (setvar "OSMODE"   0)
  (setvar "3DOSMODE" 0)
)

(defun fit:start-undo (doc)
  (vla-StartUndoMark doc)
)

(defun fit:end-undo (doc)
  (vla-EndUndoMark doc)
)

;;; Универсальный обработчик ошибок
;;; Использует глобальные *fit-old-vars* и *fit-ucs-changed*
(defun fit:*error* (msg)
  (fit:restore-vars *fit-old-vars*)
  (if (and (boundp '*fit-ucs-changed*) *fit-ucs-changed*)
    (command-s "_.UCS" "_Previous")
  )
  (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
    (princ (strcat "\nError: " msg))
  )
  (if (boundp '*fit-doc*)
    (fit:end-undo *fit-doc*)
  )
  (princ)
)

;;; =========================================================================
;;; ЗАПРОС РАДИУСОВ
;;; =========================================================================

(defun fit:ask-radii-2 (r1 r2 / prompt-str input comma-pos r1-new r2-new)
  ;; Запрос двух радиусов.
  ;; Возвращает (r1-new r2-new) или nil, если пользователь ничего не ввёл.
  (setq prompt-str (strcat "\nВведите радиусы <"
                           (rtos r1 2 4) "," (rtos r2 2 4) ">: "))
  (setq input (getstring prompt-str))
  (if (or (null input) (= input ""))
    nil
    (progn
      (setq comma-pos (vl-string-search "," input))
      (if comma-pos
        (progn
          (setq r1-new (atof (substr input 1 comma-pos)))
          (setq r2-new (atof (substr input (+ comma-pos 2))))
        )
        (progn
          (setq r1-new (atof input)
                r2-new r1-new)
        )
      )
      (if (and (> r1-new 0.0) (> r2-new 0.0))
        (list r1-new r2-new)
        nil
      )
    )
  )
)

(defun fit:ask-radius-1 (r / prompt-str input r-new)
  ;; Запрос одного радиуса.
  ;; Возвращает новое значение или nil.
  (setq prompt-str (strcat "\nВведите радиус <" (rtos r 2 4) ">: "))
  (setq input (getstring prompt-str))
  (if (or (null input) (= input ""))
    nil
    (progn
      (setq r-new (atof input))
      (if (> r-new 0.0) r-new nil)
    )
  )
)

;;; =========================================================================
;;; МАТЕМАТИКА (без изменений по смыслу, только организация)
;;; =========================================================================

(defun get-centroid (pts / n)
  (setq n (float (length pts)))
  (list
    (/ (apply '+ (mapcar 'car   pts)) n)
    (/ (apply '+ (mapcar 'cadr  pts)) n)
    (/ (apply '+ (mapcar 'caddr pts)) n)
  )
)

(defun get-points-sel (/ ss i ename elist pts)
  (setq ss (ssget "_I" '((0 . "POINT"))))
  (if (null ss)
    (progn
      (prompt "\nSelect POINT objects: ")
      (setq ss (ssget '((0 . "POINT"))))
    )
  )
  (setq pts '())
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i)
              elist (entget ename)
              pts   (cons (cdr (assoc 10 elist)) pts)
              i     (1+ i))
      )
    )
  )
  (reverse pts)
)

(defun normalize (v / len)
  (setq len (sqrt (apply '+ (mapcar '* v v))))
  (if (equal len 0.0 1e-9) v (mapcar '(lambda (x) (/ x len)) v))
)

(defun vec-length (v)
  (sqrt (apply '+ (mapcar '* v v)))
)

(defun dist-3d (a b)
  (vec-length (mapcar '- a b))
)

(defun cross-product (a b)
  (list
    (- (* (cadr a) (caddr b)) (* (caddr a) (cadr b)))
    (- (* (caddr a) (car b))  (* (car a)   (caddr b)))
    (- (* (car a)   (cadr b)) (* (cadr a)  (car b)))
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

(defun transform-point (pt mat / x y z r1 r2 r3)
  (setq x (car pt) y (cadr pt) z (caddr pt)
        r1 (nth 0 mat) r2 (nth 1 mat) r3 (nth 2 mat))
  (list
    (+ (* x (nth 0 r1)) (* y (nth 1 r1)) (* z (nth 2 r1)) (nth 3 r1))
    (+ (* x (nth 0 r2)) (* y (nth 1 r2)) (* z (nth 2 r2)) (nth 3 r2))
    (+ (* x (nth 0 r3)) (* y (nth 1 r3)) (* z (nth 2 r3)) (nth 3 r3))
  )
)

(defun make-transform-matrix (org x y z)
  (list
    (list (car x) (cadr x) (caddr x) (- (dot-product x org)))
    (list (car y) (cadr y) (caddr y) (- (dot-product y org)))
    (list (car z) (cadr z) (caddr z) (- (dot-product z org)))
    '(0.0 0.0 0.0 1.0)
  )
)

(defun invert-matrix (m / r1 r2 r3 tx ty tz nx ny nz t-vec)
  (setq r1 (nth 0 m) r2 (nth 1 m) r3 (nth 2 m)
        tx (nth 3 r1) ty (nth 3 r2) tz (nth 3 r3)
        nx (list (nth 0 r1) (nth 0 r2) (nth 0 r3))
        ny (list (nth 1 r1) (nth 1 r2) (nth 1 r3))
        nz (list (nth 2 r1) (nth 2 r2) (nth 2 r3))
        t-vec (list tx ty tz))
  (list
    (append nx (list (- (dot-product nx t-vec))))
    (append ny (list (- (dot-product ny t-vec))))
    (append nz (list (- (dot-product nz t-vec))))
    '(0.0 0.0 0.0 1.0)
  )
)

(defun det-3x3 (m / r1 r2 r3)
  (setq r1 (nth 0 m) r2 (nth 1 m) r3 (nth 2 m))
  (+ (* (nth 0 r1) (- (* (nth 1 r2) (nth 2 r3)) (* (nth 2 r2) (nth 1 r3))))
     (* (nth 1 r1) (- (* (nth 2 r2) (nth 0 r3)) (* (nth 0 r2) (nth 2 r3))))
     (* (nth 2 r1) (- (* (nth 0 r2) (nth 1 r3)) (* (nth 1 r2) (nth 0 r3)))))
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

(defun solve-linear-3x3 (mat vec / d)
  (setq d (det-3x3 mat))
  (if (equal d 0.0 1e-9)
    nil
    (list
      (/ (det-3x3 (replace-col mat 0 vec)) d)
      (/ (det-3x3 (replace-col mat 1 vec)) d)
      (/ (det-3x3 (replace-col mat 2 vec)) d)
    )
  )
)

;;; ----- Аппроксимация плоскости -----
(defun power-iteration (m iter / v)
  (setq v '(1.0 1.0 1.0))
  (repeat iter (setq v (normalize (mxv m v))))
  v
)

(defun mxv (m v)
  (list (dot-product (nth 0 m) v)
        (dot-product (nth 1 m) v)
        (dot-product (nth 2 m) v))
)

(defun v-m-v (m v)
  (dot-product v (mxv m v))
)

(defun deflate-matrix (m v lam / vx vy vz)
  (setq vx (car v) vy (cadr v) vz (caddr v))
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

(defun fit-plane (pts / n centroid xx xy xz yy yz zz cov v1 l1 cov2 v2)
  (setq n (float (length pts))
        centroid (get-centroid pts)
        xx 0.0 xy 0.0 xz 0.0 yy 0.0 yz 0.0 zz 0.0)
  (foreach p pts
    (setq dx (- (car p)   (car centroid))
          dy (- (cadr p)  (cadr centroid))
          dz (- (caddr p) (caddr centroid)))
    (setq xx (+ xx (* dx dx)) xy (+ xy (* dx dy)) xz (+ xz (* dx dz))
          yy (+ yy (* dy dy)) yz (+ yz (* dy dz)) zz (+ zz (* dz dz)))
  )
  (setq cov (list (list xx xy xz) (list xy yy yz) (list xz yz zz))
        v1  (power-iteration cov 20)
        l1  (v-m-v cov v1)
        cov2 (deflate-matrix cov v1 l1)
        v2  (power-iteration cov2 20))
  (list centroid (normalize (cross-product v1 v2)))
)

;;; ----- Аппроксимация окружности 2D -----
(defun fit-circle-2d-Kasa (pts / n mean-x mean-y pts-n
                               sum-x sum-y sum-x2 sum-y2 sum-xy
                               sum-x3 sum-y3 sum-xy2 sum-x2y
                               mtm mty sol A B C xc yc rad)
  (setq n (length pts))
  (if (< n 3) (list '(0.0 0.0) 0.0)
    (progn
      (setq mean-x (/ (apply '+ (mapcar 'car  pts)) (float n))
            mean-y (/ (apply '+ (mapcar 'cadr pts)) (float n))
            pts-n  (mapcar '(lambda (p) (list (- (car p) mean-x) (- (cadr p) mean-y))) pts)
            sum-x 0.0 sum-y 0.0 sum-x2 0.0 sum-y2 0.0 sum-xy 0.0
            sum-x3 0.0 sum-y3 0.0 sum-xy2 0.0 sum-x2y 0.0)
      (foreach p pts-n
        (setq x (car p) y (cadr p))
        (setq sum-x  (+ sum-x x)  sum-y  (+ sum-y y)
              sum-x2 (+ sum-x2 (* x x)) sum-y2 (+ sum-y2 (* y y)) sum-xy (+ sum-xy (* x y))
              sum-x3 (+ sum-x3 (* x x x)) sum-y3 (+ sum-y3 (* y y y))
              sum-xy2 (+ sum-xy2 (* x y y)) sum-x2y (+ sum-x2y (* x x y)))
      )
      (setq mtm (list (list sum-x2 sum-xy sum-x)
                      (list sum-xy sum-y2 sum-y)
                      (list sum-x  sum-y  (float n)))
            mty (list (+ sum-x3 sum-xy2) (+ sum-x2y sum-y3) (+ sum-x2 sum-y2))
            sol (solve-linear-3x3 mtm mty))
      (if (null sol)
        (list (list mean-x mean-y) 0.0)
        (progn
          (setq A (car sol) B (cadr sol) C (caddr sol)
                xc (/ A 2.0) yc (/ B 2.0)
                rad (sqrt (abs (+ C (* xc xc) (* yc yc)))))
          (list (list (+ xc mean-x) (+ yc mean-y)) rad)
        )
      )
    )
  )
)

(defun fit-circle-2d-MNK (pts / n mean-x mean-y pts-n init xc yc rad
                               max-iter tol iter converged
                               dx dy dist ux uy res
                               a11 a12 a13 a22 a23 a33 b1 b2 b3 sol dxc dyc dr)
  (setq n (length pts))
  (if (< n 3) (list '(0.0 0.0) 0.0)
    (progn
      (setq mean-x (/ (apply '+ (mapcar 'car  pts)) (float n))
            mean-y (/ (apply '+ (mapcar 'cadr pts)) (float n))
            pts-n  (mapcar '(lambda (p) (list (- (car p) mean-x) (- (cadr p) mean-y))) pts)
            init   (fit-circle-2d-Kasa pts-n)
            xc     (car  (car init))
            yc     (cadr (car init))
            rad    (cadr init)
            max-iter 10 tol 1e-10 iter 0 converged nil)
      (while (and (< iter max-iter) (not converged))
        (setq a11 0.0 a12 0.0 a13 0.0 a22 0.0 a23 0.0 a33 0.0 b1 0.0 b2 0.0 b3 0.0)
        (foreach p pts-n
          (setq dx (- (car p) xc) dy (- (cadr p) yc)
                dist (sqrt (+ (* dx dx) (* dy dy))))
          (if (> dist 1e-12)
            (progn
              (setq ux (/ dx dist) uy (/ dy dist) res (- dist rad))
              (setq a11 (+ a11 (* ux ux)) a12 (+ a12 (* ux uy)) a13 (+ a13 ux)
                    a22 (+ a22 (* uy uy)) a23 (+ a23 uy) a33 (+ a33 1.0)
                    b1 (+ b1 (* ux res)) b2 (+ b2 (* uy res)) b3 (+ b3 res))
            )
          )
        )
        (setq sol (solve-linear-3x3
                    (list (list a11 a12 a13) (list a12 a22 a23) (list a13 a23 a33))
                    (list b1 b2 b3)))
        (if sol
          (progn
            (setq dxc (car sol) dyc (cadr sol) dr (caddr sol)
                  xc (+ xc dxc) yc (+ yc dyc) rad (+ rad dr))
            (if (< (max (abs dxc) (abs dyc) (abs dr)) tol) (setq converged T))
          )
          (setq iter max-iter)
        )
        (setq iter (1+ iter))
      )
      (list (list (+ xc mean-x) (+ yc mean-y)) rad)
    )
  )
)

(defun fit-circle-2d-fixed-R (pts radius /
                              n mean-x mean-y pts-n xc yc
                              max-iter tol iter converged
                              dx dy dist ux uy res
                              a11 a12 a22 b1 b2 det dxc dyc)
  (setq n (length pts))
  (if (or (< n 3) (<= radius 0.0))
    (list '(0.0 0.0) radius)
    (progn
      (setq mean-x (/ (apply '+ (mapcar 'car  pts)) (float n))
            mean-y (/ (apply '+ (mapcar 'cadr pts)) (float n))
            pts-n  (mapcar '(lambda (p) (list (- (car p) mean-x) (- (cadr p) mean-y))) pts)
            init   (fit-circle-2d-Kasa pts-n)
            xc     (car  (car init))
            yc     (cadr (car init))
            max-iter 15 tol 1e-10 iter 0 converged nil)
      (while (and (< iter max-iter) (not converged))
        (setq a11 0.0 a12 0.0 a22 0.0 b1 0.0 b2 0.0)
        (foreach p pts-n
          (setq dx (- (car p) xc) dy (- (cadr p) yc)
                dist (sqrt (+ (* dx dx) (* dy dy))))
          (if (> dist 1e-12)
            (progn
              (setq ux (/ dx dist) uy (/ dy dist) res (- dist radius))
              (setq a11 (+ a11 (* ux ux)) a12 (+ a12 (* ux uy)) a22 (+ a22 (* uy uy))
                    b1 (+ b1 (* ux res)) b2 (+ b2 (* uy res)))
            )
          )
        )
        (setq det (- (* a11 a22) (* a12 a12)))
        (if (equal det 0.0 1e-12)
          (setq iter max-iter)
          (progn
            (setq dxc (/ (- (* a22 b1) (* a12 b2)) det)
                  dyc (/ (- (* a11 b2) (* a12 b1)) det)
                  xc  (+ xc dxc) yc (+ yc dyc))
            (if (< (max (abs dxc) (abs dyc)) tol) (setq converged T))
          )
        )
        (setq iter (1+ iter))
      )
      (list (list (+ xc mean-x) (+ yc mean-y)) radius)
    )
  )
)

(defun fit-circle-2d-with-z (pts method / circ center rad avg-z)
  (cond
    ((= method 'KASA) (setq circ (fit-circle-2d-Kasa pts)))
    ((= method 'MNK)  (setq circ (fit-circle-2d-MNK  pts)))
    (T                (setq circ (fit-circle-2d-Kasa pts)))
  )
  (setq center (car circ)
        rad    (cadr circ)
        avg-z  (/ (apply '+ (mapcar 'caddr pts)) (float (length pts))))
  (list (list (car center) (cadr center) avg-z) rad)
)

(defun fit-circle-2d-with-z-fixed (pts radius / circ center avg-z)
  (setq circ   (fit-circle-2d-fixed-R pts radius)
        center (car circ)
        avg-z  (/ (apply '+ (mapcar 'caddr pts)) (float (length pts))))
  (list (list (car center) (cadr center) avg-z) radius)
)

;;; ----- 3D окружность -----
(defun fit-circle-3d-full (pts / plane centroid normal axis-u axis-v pts-2d circ-2d uc vc rad center-3d)
  (setq plane    (fit-plane pts)
        centroid (car plane)
        normal   (cadr plane)
        axis-u   (arbitrary-axis normal)
        axis-v   (cross-product normal axis-u)
        pts-2d   (mapcar '(lambda (p / vec)
                            (setq vec (mapcar '- p centroid))
                            (list (dot-product vec axis-u) (dot-product vec axis-v)))
                         pts)
        circ-2d  (fit-circle-2d-MNK pts-2d)
        uc (car (car circ-2d)) vc (cadr (car circ-2d)) rad (cadr circ-2d)
        center-3d (mapcar '+ centroid
                          (mapcar '+ (mapcar '(lambda (x) (* x uc)) axis-u)
                                     (mapcar '(lambda (x) (* x vc)) axis-v))))
  (list center-3d rad normal)
)

;;; ----- Трёхэтапный solve-3pass -----
(defun solve-3pass (pts1 pts2 /
                    tol-kasa tol-mnk max-iter global-center pts1-l pts2-l
                    c1 c2 prev-c1 prev-c2 prev-r1 prev-r2
                    axis-z axis-x axis-y mat-wcs-to-ucs mat-ucs-to-wcs
                    pts1-u pts2-u circ1-u circ2-u c1-new c2-new r1 r2 diff i stage)
  (setq tol-kasa 0.005 tol-mnk 0.0001 max-iter 80
        global-center (get-centroid (append pts1 pts2))
        pts1-l (mapcar '(lambda (p) (mapcar '- p global-center)) pts1)
        pts2-l (mapcar '(lambda (p) (mapcar '- p global-center)) pts2)
        c1 (get-centroid pts1-l) c2 (get-centroid pts2-l)
        prev-c1 nil stage 2 i 0)
  (while (and (< i max-iter) (< stage 4))
    (setq axis-z (mapcar '- c2 c1))
    (if (equal (vec-length axis-z) 0.0 1e-12)
      (setq axis-z '(0.0 0.0 1.0))
      (setq axis-z (normalize axis-z)))
    (setq axis-x (arbitrary-axis axis-z)
          axis-y (cross-product axis-z axis-x)
          mat-wcs-to-ucs (make-transform-matrix c1 axis-x axis-y axis-z)
          pts1-u (mapcar '(lambda (p) (transform-point p mat-wcs-to-ucs)) pts1-l)
          pts2-u (mapcar '(lambda (p) (transform-point p mat-wcs-to-ucs)) pts2-l))
    (if (= stage 2)
      (setq circ1-u (fit-circle-2d-with-z pts1-u 'KASA)
            circ2-u (fit-circle-2d-with-z pts2-u 'KASA))
      (setq circ1-u (fit-circle-2d-with-z pts1-u 'MNK)
            circ2-u (fit-circle-2d-with-z pts2-u 'MNK)))
    (setq r1 (cadr circ1-u) r2 (cadr circ2-u)
          mat-ucs-to-wcs (invert-matrix mat-wcs-to-ucs)
          c1-new (transform-point (car circ1-u) mat-ucs-to-wcs)
          c2-new (transform-point (car circ2-u) mat-ucs-to-wcs))
    (if prev-c1
      (progn
        (setq diff (max (dist-3d c1-new prev-c1) (dist-3d c2-new prev-c2)
                        (abs (- r1 prev-r1)) (abs (- r2 prev-r2))))
        (cond
          ((and (= stage 2) (< diff tol-kasa)) (setq stage 3 prev-c1 nil))
          ((and (= stage 3) (< diff tol-mnk))  (setq stage 4))
        )
      )
    )
    (setq prev-c1 c1-new prev-c2 c2-new prev-r1 r1 prev-r2 r2
          c1 c1-new c2 c2-new i (1+ i))
  )
  (list (mapcar '+ prev-c1 global-center)
        (mapcar '+ prev-c2 global-center)
        prev-r1 prev-r2)
)

;;; ----- Уточнение с фиксированными радиусами -----
(defun refine-with-fixed-radii (pts1 pts2 r1 r2 /
                                global-center pts1-l pts2-l
                                c1 c2 prev-c1 prev-c2
                                axis-z axis-x axis-y
                                mat-wcs-to-ucs mat-ucs-to-wcs
                                pts1-u pts2-u circ1-u circ2-u
                                c1-new c2-new diff i max-iter tol)
  (setq max-iter 40 tol 1e-6
        global-center (get-centroid (append pts1 pts2))
        pts1-l (mapcar '(lambda (p) (mapcar '- p global-center)) pts1)
        pts2-l (mapcar '(lambda (p) (mapcar '- p global-center)) pts2)
        c1 (get-centroid pts1-l) c2 (get-centroid pts2-l)
        prev-c1 nil i 0)
  (while (< i max-iter)
    (setq axis-z (mapcar '- c2 c1))
    (if (equal (vec-length axis-z) 0.0 1e-12)
      (setq axis-z '(0.0 0.0 1.0))
      (setq axis-z (normalize axis-z)))
    (setq axis-x (arbitrary-axis axis-z)
          axis-y (cross-product axis-z axis-x)
          mat-wcs-to-ucs (make-transform-matrix c1 axis-x axis-y axis-z)
          pts1-u (mapcar '(lambda (p) (transform-point p mat-wcs-to-ucs)) pts1-l)
          pts2-u (mapcar '(lambda (p) (transform-point p mat-wcs-to-ucs)) pts2-l)
          circ1-u (fit-circle-2d-with-z-fixed pts1-u r1)
          circ2-u (fit-circle-2d-with-z-fixed pts2-u r2)
          mat-ucs-to-wcs (invert-matrix mat-wcs-to-ucs)
          c1-new (transform-point (car circ1-u) mat-ucs-to-wcs)
          c2-new (transform-point (car circ2-u) mat-ucs-to-wcs))
    (if prev-c1
      (progn
        (setq diff (max (dist-3d c1-new prev-c1) (dist-3d c2-new prev-c2)))
        (if (< diff tol) (setq i max-iter))
      )
    )
    (setq prev-c1 c1-new prev-c2 c2-new c1 c1-new c2 c2-new i (1+ i))
  )
  (list (mapcar '+ prev-c1 global-center)
        (mapcar '+ prev-c2 global-center)
        r1 r2)
)

;;; Вспомогательная функция: уточнение 3D-окружности с фиксированным радиусом
(defun fit-circle-3d-fixed-R (pts radius /
                              plane centroid normal
                              axis-u axis-v
                              pts-2d res-2d uc vc
                              center-3d)
  (setq plane    (fit-plane pts)
        centroid (car plane)
        normal   (cadr plane)
        axis-u   (arbitrary-axis normal)
        axis-v   (cross-product normal axis-u))

  ;; Проекция точек на плоскость (в UV-координаты)
  (setq pts-2d
    (mapcar
      '(lambda (p / vec)
         (setq vec (mapcar '- p centroid))
         (list (dot-product vec axis-u)
               (dot-product vec axis-v))
       )
      pts))

  ;; Подгонка центра в 2D с фиксированным радиусом
  (setq res-2d (fit-circle-2d-fixed-R pts-2d radius)
        uc     (car  (car res-2d))
        vc     (cadr (car res-2d)))

  ;; Возвращаем центр в 3D
  (setq center-3d
    (mapcar '+ centroid
            (mapcar '+ (mapcar '(lambda (x) (* x uc)) axis-u)
                       (mapcar '(lambda (x) (* x vc)) axis-v))))

  (list center-3d radius normal)
)

;;; =========================================================================
;;; КОМАНДЫ
;;; =========================================================================

(defun c:FITCYL (/ pts1 pts2 res final-c1 final-c2 r1 r2 final-r
                  new-radii r-fixed *fit-doc* *fit-old-vars* *fit-ucs-changed*)

  (setq *fit-doc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *error* fit:*error*)
  (fit:start-undo *fit-doc*)

  (prompt "\nSelect points for First Section:")
  (setq pts1 (get-points-sel))
  (if (< (length pts1) 3) (progn (alert "Need at least 3 points for section 1.") (exit)))

  (prompt "\nSelect points for Second Section:")
  (setq pts2 (get-points-sel))
  (if (< (length pts2) 3) (progn (alert "Need at least 3 points for section 2.") (exit)))

  (prompt "\nCalculating cylinder parameters (iterative)...")
  (setq res (solve-3pass pts1 pts2)
        final-c1 (nth 0 res) final-c2 (nth 1 res)
        r1 (nth 2 res) r2 (nth 3 res)
        final-r (/ (+ r1 r2) 2.0))

  ;; Запрос радиуса
  (setq r-fixed (fit:ask-radius-1 final-r))
  (if r-fixed
    (progn
      (prompt "\nУточнение с заданным радиусом...")
      (setq res (refine-with-fixed-radii pts1 pts2 r-fixed r-fixed)
            final-c1 (nth 0 res) final-c2 (nth 1 res)
            final-r  r-fixed)
    )
  )

  ;; Построение
  (setq *fit-old-vars* (fit:save-vars)
        *fit-ucs-changed* nil)
  (fit:prepare-build)
  (command "_.UCS" "_World")
  (setq *fit-ucs-changed* T)
  (command "_cylinder" "_non" final-c1 final-r "_a" "_non" final-c2)
  (command "_.UCS" "_Previous")
  (setq *fit-ucs-changed* nil)
  (fit:restore-vars *fit-old-vars*)
  (fit:end-undo *fit-doc*)
  (princ "\nCylinder created.")
  (princ)
)

(defun c:FITCON (/ pts1 pts2 res final-c1 final-c2 final-r1 final-r2
                  new-radii *fit-doc* *fit-old-vars* *fit-ucs-changed*)

  (setq *fit-doc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *error* fit:*error*)
  (fit:start-undo *fit-doc*)

  (prompt "\nSelect points for Base Section (Start):")
  (setq pts1 (get-points-sel))
  (if (< (length pts1) 3) (progn (alert "Need at least 3 points for section 1.") (exit)))

  (prompt "\nSelect points for Top Section (End):")
  (setq pts2 (get-points-sel))
  (if (< (length pts2) 3) (progn (alert "Need at least 3 points for section 2.") (exit)))

  (prompt "\nCalculating cone parameters (iterative)...")
  (setq res (solve-3pass pts1 pts2)
        final-c1 (nth 0 res) final-c2 (nth 1 res)
        final-r1 (nth 2 res) final-r2 (nth 3 res))

  ;; Запрос радиусов
  (setq new-radii (fit:ask-radii-2 final-r1 final-r2))
  (if new-radii
    (progn
      (prompt "\nУточнение с заданными радиусами...")
      (setq res (refine-with-fixed-radii pts1 pts2 (car new-radii) (cadr new-radii))
            final-c1 (nth 0 res) final-c2 (nth 1 res)
            final-r1 (nth 2 res) final-r2 (nth 3 res))
    )
  )

  ;; Построение
  (setq *fit-old-vars* (fit:save-vars)
        *fit-ucs-changed* nil)
  (fit:prepare-build)
  (command "_.UCS" "_World")
  (setq *fit-ucs-changed* T)
  (command "_cone" "_non" final-c1 final-r1 "_t" final-r2 "_a" "_non" final-c2)
  (command "_.UCS" "_Previous")
  (setq *fit-ucs-changed* nil)
  (fit:restore-vars *fit-old-vars*)
  (fit:end-undo *fit-doc*)
  (princ "\nCone created.")
  (princ)
)

(defun c:FITCIRS (/ pts1 pts2 res final-c1 final-c2 final-r1 final-r2 axis-vec
                   new-radii *fit-doc* *fit-old-vars* *fit-ucs-changed*)

  (setq *fit-doc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *error* fit:*error*)
  (fit:start-undo *fit-doc*)

  (prompt "\nSelect points for Base Section (Start):")
  (setq pts1 (get-points-sel))
  (if (< (length pts1) 3) (progn (alert "Need at least 3 points for section 1.") (exit)))

  (prompt "\nSelect points for Top Section (End):")
  (setq pts2 (get-points-sel))
  (if (< (length pts2) 3) (progn (alert "Need at least 3 points for section 2.") (exit)))

  (prompt "\nCalculating circle parameters (iterative)...")
  (setq res (solve-3pass pts1 pts2)
        final-c1 (nth 0 res) final-c2 (nth 1 res)
        final-r1 (nth 2 res) final-r2 (nth 3 res))

  ;; Запрос радиусов
  (setq new-radii (fit:ask-radii-2 final-r1 final-r2))
  (if new-radii
    (progn
      (prompt "\nУточнение с заданными радиусами...")
      (setq res (refine-with-fixed-radii pts1 pts2 (car new-radii) (cadr new-radii))
            final-c1 (nth 0 res) final-c2 (nth 1 res)
            final-r1 (nth 2 res) final-r2 (nth 3 res))
    )
  )

  (setq axis-vec (normalize (mapcar '- final-c2 final-c1)))

  ;; Построение
  (setq *fit-old-vars* (fit:save-vars)
        *fit-ucs-changed* nil)
  (fit:prepare-build)
  (command "_.UCS" "_World")
  (setq *fit-ucs-changed* T)

  (entmake (list (cons 0 "LINE") (cons 10 final-c1) (cons 11 final-c2)))
  (entmake (list (cons 0 "CIRCLE")
                 (cons 10 (trans final-c1 0 axis-vec))
                 (cons 40 final-r1)
                 (cons 210 axis-vec)))
  (entmake (list (cons 0 "CIRCLE")
                 (cons 10 (trans final-c2 0 (mapcar '- axis-vec)))
                 (cons 40 final-r2)
                 (cons 210 (mapcar '- axis-vec))))

  (command "_.UCS" "_Previous")
  (setq *fit-ucs-changed* nil)
  (fit:restore-vars *fit-old-vars*)
  (fit:end-undo *fit-doc*)
  (princ "\nCircles and axis line created.")
  (princ)
)

(defun c:FITCIR3D (/ pts res global-center pts-local center rad normal
                    r-fixed *fit-doc* *fit-old-vars* *fit-ucs-changed*)

  (setq *fit-doc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *error* fit:*error*)
  (fit:start-undo *fit-doc*)

  (prompt "\nSelect points for Circle:")
  (setq pts (get-points-sel))
  (if (< (length pts) 3)
    (progn (alert "Need at least 3 points.") (exit))
  )

  ;; Обычный расчёт (свободный радиус)
  (setq global-center (get-centroid pts)
        pts-local     (mapcar '(lambda (p) (mapcar '- p global-center)) pts)
        res           (fit-circle-3d-full pts-local)
        center        (mapcar '+ (nth 0 res) global-center)
        rad           (nth 1 res)
        normal        (nth 2 res))

  ;; ----- Запрос фиксированного радиуса -----
  (setq r-fixed (fit:ask-radius-1 rad))
  (if r-fixed
    (progn
      (prompt "\nУточнение с заданным радиусом (геометрический МНК)...")
      (setq res    (fit-circle-3d-fixed-R pts r-fixed)
            center (nth 0 res)
            rad    (nth 1 res)
            normal (nth 2 res))
    )
  )

  ;; Построение
  (setq *fit-old-vars*    (fit:save-vars)
        *fit-ucs-changed* nil)
  (fit:prepare-build)
  (command "_.UCS" "_World")
  (setq *fit-ucs-changed* T)

  (entmake (list (cons 0   "CIRCLE")
                 (cons 10  (trans center 0 normal))
                 (cons 40  rad)
                 (cons 210 normal)))

  (command "_.UCS" "_Previous")
  (setq *fit-ucs-changed* nil)
  (fit:restore-vars *fit-old-vars*)
  (fit:end-undo *fit-doc*)

  (princ (strcat "\nCircle created. Radius = " (rtos rad 2 4)))
  (princ)
)

(defun c:FITCIR2D (/ pts n pts-xy avg-z res center rad r-fixed
                    *fit-doc* *fit-old-vars*)

  (setq *fit-doc* (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *error* fit:*error*)
  (fit:start-undo *fit-doc*)

  (prompt "\nSelect points for XY-circle:")
  (setq pts (get-points-sel)
        n (length pts))
  (if (< n 3) (progn (alert "Need at least 3 points.") (exit)))

  (setq avg-z (/ (apply '+ (mapcar 'caddr pts)) (float n))
        pts-xy (mapcar '(lambda (p) (list (car p) (cadr p))) pts)
        res (fit-circle-2d-MNK pts-xy)
        center (car res)
        rad (cadr res)
        center (list (car center) (cadr center) avg-z))

  ;; Запрос радиуса
  (setq r-fixed (fit:ask-radius-1 rad))
  (if r-fixed
    (progn
      (prompt "\nУточнение с заданным радиусом...")
      (setq res (fit-circle-2d-fixed-R pts-xy r-fixed)
            center (list (car (car res)) (cadr (car res)) avg-z)
            rad r-fixed)
    )
  )

  ;; Построение
  (setq *fit-old-vars* (fit:save-vars))
  (fit:prepare-build)
  (entmake (list (cons 0 "CIRCLE")
                 (cons 10 center)
                 (cons 40 rad)
                 (cons 210 '(0.0 0.0 1.0))))
  (fit:restore-vars *fit-old-vars*)
  (fit:end-undo *fit-doc*)
  (princ (strcat "\nXY-circle created. Radius = " (rtos rad 2 4)
                 ", Z = " (rtos avg-z 2 4)))
  (princ)
)

;;; =========================================================================
(princ "\nFITCYL commands loaded: FITCYL, FITCON, FITCIRS, FITCIR3D, FITCIR2D")
(princ)
