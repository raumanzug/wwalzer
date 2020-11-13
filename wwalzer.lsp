; Wuerfelt mit zwei Wuerfeln.
; Result: Eine Wuerfelzahl zwischen 2 und 12.
(defun dice ()
 (let ((r (random 36)))
  (+ 2 (rem r 6) (/ r 6)) ))

; Wuerfelt sechzehn mal mit zwei Wuerfeln.
; Result: Liste mit Wuerfelzahlen jeweils zwischen 2 und 12.
(defun dice15 ()
 (vector
  (dice) (dice) (dice) (dice) (dice) (dice) (dice)
  (dice) (dice) (dice) (dice) (dice) (dice) (dice) (dice) ))

(defun note (pitch)
 (lambda (dur)
  (mult
   (osc pitch dur)
   (env 0.05 0.1 0.5 1.0 0.5 0.4) )))

(defun akkord (&rest pitches)
 (lambda (dur)
  (apply (function sim)
   (mapcar
    (lambda (component)
     (funcall (note component) dur) )
    pitches ))))

(defun triller (na nb)
 (lambda (dur)
  (let ((dur1 (/ dur 17.0)))
   (seq
    (funcall na dur1)
    (funcall nb dur1)
    (funcall na dur1)
    (funcall nb dur1)
    (funcall na dur1)
    (funcall nb dur1)
    (funcall na dur1)
    (funcall nb dur1)
    (funcall na dur1)
    (funcall nb dur1)
    (funcall na dur1)
    (funcall nb dur1)
    (funcall na dur1)
    (funcall nb dur1)
    (funcall na dur1)
    (funcall nb dur1)
    (funcall na dur1) ))))

(defun d5-triller ()
 (triller (note d5) (note ds5)) )

(defun e5-triller ()
 (triller (note e5) (note f5)) )

(defun f5-triller ()
 (triller (note f5) (note fs5)) )

(defun a4-mordent ()
 (lambda (dur)
  (let ((dur1 (/ dur 4.0)))
   (seq
    (funcall (note bf4) dur1)
    (funcall (note a4) dur1)
    (funcall (note gs4) dur1)
    (funcall (note a4) dur1) ))))

(defun pause ()
 (lambda (dur) (s-rest dur)) )

(defun line-2-1 (pitches)
 (seq
  (funcall (aref pitches 0) q)
  (funcall (aref pitches 1) i) ))

(defun line-2-2 (pitches)
 (seq
  (funcall (aref pitches 0) i)
  (funcall (aref pitches 1) q) ))

(defun line-3 (pitches)
 (seq
  (funcall (aref pitches 0) i)
  (funcall (aref pitches 1) i)
  (funcall (aref pitches 2) i) ))

(defun line-3-s (pitches)
 (seq
  (funcall (aref pitches 0) q)
  (funcall (aref pitches 1) s)
  (funcall (aref pitches 2) s) ))

(defun line-4-1 (pitches)
 (seq
  (funcall (aref pitches 0) s)
  (funcall (aref pitches 1) s)
  (funcall (aref pitches 2) i)
  (funcall (aref pitches 3) i) ))

(defun line-4-1-portato-3 (pitches)
 (line-4-1 pitches) )

(defun line-4-2 (pitches)
 (seq
  (funcall (aref pitches 0) i)
  (funcall (aref pitches 1) s)
  (funcall (aref pitches 2) s)
  (funcall (aref pitches 3) i) ))

(defun line-5-1 (pitches)
 (seq
  (funcall (aref pitches 0) i)
  (funcall (aref pitches 1) s)
  (funcall (aref pitches 2) s)
  (funcall (aref pitches 3) s)
  (funcall (aref pitches 4) s) ))

(defun line-5-3 (pitches)
 (seq
  (funcall (aref pitches 0) s)
  (funcall (aref pitches 1) s)
  (funcall (aref pitches 2) i)
  (funcall (aref pitches 3) s)
  (funcall (aref pitches 4) s) ))

(defun line-5-5 (pitches)
 (seq
  (funcall (aref pitches 0) s)
  (funcall (aref pitches 1) s)
  (funcall (aref pitches 2) s)
  (funcall (aref pitches 3) s)
  (funcall (aref pitches 4) i) ))

(defun line-6 (pitches)
 (seq
  (funcall (aref pitches 0) s)
  (funcall (aref pitches 1) s)
  (funcall (aref pitches 2) s)
  (funcall (aref pitches 3) s)
  (funcall (aref pitches 4) s)
  (funcall (aref pitches 5) s) ))

(setq takt-no8
 (sim
  (line-2-1
   (vector
    (akkord g4 b4 d5 g5)
    (pause) ))
  (line-5-1
   (vector
    (note g2)
    (note g3) (note f3) (note e3) (note d3) ))))

(setq takt-no16
 (sim
  (line-2-1
   (vector
    (akkord g4 b4 d5 g5)
    (pause) ))
  (line-5-1
   (vector
    (note g2)
    (note b3) (note g3) (note fs3) (note e3) ))))

(defun pat-1 (notes)
 (sim
  (line-3
   (vector
    (note (aref notes 0))
    (note (aref notes 1))
    (note (aref notes 2)) ))
  (line-6
   (vector
    (akkord c3 e3) (note g3)
    (akkord c3 e3) (note g3)
    (akkord c3 e3) (note g3) ))))

(defun pat-2 (notes)
 (sim
  (line-3
   (vector
    (akkord (aref notes 1) (aref notes 2))
    (akkord (aref notes 1) (aref notes 2))
    (akkord (aref notes 1) (aref notes 2)) ))
  (line-3
   (vector
    (note (aref notes 0))
    (note (aref notes 0))
    (note (aref notes 0)) ))))

(defun pat-3 ()
 (sim
  (line-2-1 (vector (note c5) (pause)))
  (line-3 (vector (note c3) (note g2) (note c2))) ))

(setq takt-1
 (sim
  (line-3 (vector (note f5) (note d5) (note g5)))
  (line-3 (vector (note f3) (note d3) (note g3))) ))

(setq takt-2
 (sim
  (line-5-1
   (vector
    (note g4)
    (note fs4) (note g4) (note b4) (note g5) ))
  (line-2-1 (vector (akkord b2 g3) (pause))) ))

(setq takt-3
 (sim
  (line-3 (vector (note g5) (note c5) (note e5)))
  (line-2-1 (vector (akkord c2 e2) (pause))) ))

(setq takt-4
 (sim
  (line-2-2 (vector (note g5) (d5-triller)))
  (line-4-1 (vector (note g2) (note b2) (note g3) (note b2))) ))

(setq takt-6
 (sim
  (line-3 (vector (note g4) (note c5) (note e5)))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-7
 (sim
  (line-6
   (vector
    (note e5) (note c5) (note e5) (note g5) (note c6) (note g5) ))
  (line-2-1 (vector (akkord c3 g3) (pause))) ))

(setq takt-8 (pat-3))

(setq takt-9
 (sim
  (line-3
   (vector
    (akkord c5 e5) (akkord b4 d5) (pause) ))
  (line-2-1 (vector (note g3) (note g2))) ))

(setq takt-10
 (sim
  (line-6
   (vector
    (note b4) (note a4) (note b4) (note c5) (note d5) (note b4) ))
  (line-2-1 (vector (note g3) (pause))) ))

(setq takt-11
 (sim
  (line-6
   (vector
    (note e5) (note c5) (note b4) (note a4) (note g4) (note fs4) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-12 (pat-2 (vector c3 e4 c5)))

(setq takt-13
 (sim
  (line-3 (vector (note c5) (note g4) (note e3))) 
  (line-2-1 (vector (akkord e3 g3) (pause))) ))

(setq takt-14 (pat-3))

(setq takt-15
 (sim
  (line-4-2
   (vector
    (note e5) (note g5) (note e5) (note c5) ))
  (line-2-1 (vector (akkord c3 g3) (akkord c3 e3))) ))

(setq takt-16
 (sim
  (line-3 (vector (note a5) (note fs5) (note d5)))
  (line-2-1 (vector (akkord d3 fs3) (akkord c3 fs3))) ))

(setq takt-17
 (sim
  (line-6
   (vector
    (note c5) (note g4) (note c5) (note e5) (note g4) (note c5) ))
  (line-2-1 (vector (akkord e3 g3) (pause))) ))

(setq takt-18
 (sim
  (line-3 (vector (note g4) (note c5) (note e5)))
  (line-2-1 (vector (akkord c3 e3) (akkord c3 g3))) ))

(setq takt-19
 (sim
  (line-4-1
   (vector
    (note e5) (note c5) (note e5) (note g5) ))
  (line-2-1 (vector (akkord c3 g3) (akkord c3 e3))) ))

(setq takt-20
 (sim
  (line-4-2
   (vector
    (note g5) (note b5) (note d6) (note d5) ))
  (line-2-1 (vector (note b2) (pause))) ))

(setq takt-21
 (sim
  (line-6
   (vector
    (note c5) (note e5) (note g5) (note d5) (note a4) (note fs5) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-22
 (sim
  (line-3 (vector (note e5) (note c5) (note g4)))
  (line-2-1 (vector (note c3) (pause))) ))

(setq takt-23
 (sim
  (line-6
   (vector
    (note f5) (note e5) (note d5) (note e5) (note f5) (note g5) ))
  (line-6
   (vector
    (note f3) (note e3) (note d3) (note e3) (note f3) (note g3) ))))

(setq takt-25
 (sim
  (line-6
   (vector
    (note d4) (note fs4) (note a4) (note d5) (note fs5) (note a5) ))
  (line-2-1 (vector (note d3) (note c3))) ))

(setq takt-26
 (sim
  (line-3 (vector (akkord c4 e4) (akkord c4 e4) (akkord c4 e4)))
  (line-6
   (vector
    (note c3) (note e3) (note g3) (note e3) (note c4) (note c3) ))))

(setq takt-27
 (sim
  (line-6
   (vector
    (note f5) (note e5) (note f5) (note d5) (note c5) (note b4) ))
  (line-2-1 (vector (akkord g3 b3) (pause))) ))

(setq takt-28
 (sim
  (line-6
   (vector
    (note fs5) (note d5) (note a4) (note a5) (note fs5) (note d5) ))
  (line-2-1 (vector (akkord c3 a3) (pause))) ))

(setq takt-29
 (sim
  (line-5-1
   (vector
    (note b4) (note d5) (note g5) (note d5) (note b4) ))
  (line-2-1 (vector (note g3) (note g2))) ))

(setq takt-31
 (sim
  (line-4-1-portato-3
   (vector
    (note e5) (note c5) (note g4) (note e5) ))
  (line-2-1 (vector (akkord c3 a3) (akkord c3 a3))) ))

(setq takt-32
 (sim
  (line-3 (vector (note g4) (note c5) (note e5)))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-34
 (sim
  (line-5-5
   (vector (note e5) (note c5) (note d5) (note b4) (note g4)) )
  (line-2-1 (vector (note g3) (pause))) ))

(setq takt-35
 (sim
  (line-3 (vector (note a4) (note d5) (note fs5)))
  (line-2-1 (vector (akkord d3 fs3) (akkord d3 a3))) ))

(setq takt-36
 (sim
  (line-6
   (vector
    (note a4) (note e5) (note d5) (note g5) (note fs5) (note a5) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-37
 (sim
  (line-5-5
   (vector
    (note g5) (note b5) (note g5) (note d5) (note b4) ))
  (line-2-1 (vector (akkord b2 d3) (pause))) )) 

(setq takt-38 (pat-1 (vector c5 g4 e5)))

(setq takt-39
 (sim
  (line-3 (vector (note g5) (note g4) (note g4)))
  (line-6
   (vector
    (note b2) (note d3) (note g3) (note d3) (note b2) (note g2) ))))

(setq takt-40
 (sim
  (line-6
   (vector
    (note c5) (note b4) (note c5) (note e5) (note g4) (note c5) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-41
 (sim
  (line-5-5
   (vector
    (note c5) (note b4) (note c5) (note e5) (note g4) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-42
 (sim
  (line-6
   (vector
    (note b4) (note c5) (note d5) (note b4) (note a4) (note g4) ))
  (line-2-1
   (vector (note g2) (pause)) )))

(setq takt-43
 (sim
  (line-5-1
   (vector
    (note g5) (note f5) (note e5) (note d5) (note d5) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-44
 (sim
  (line-5-1
   (vector
    (note a4) (note f5) (note d5) (note a4) (note b4) ))
  (line-2-1 (vector (note f3) (note g3))) ))

(setq takt-45
 (sim
  (line-6
   (vector
    (note c5) (note b4) (note c5) (note g4) (note e4) (note c4) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-46
 (sim
  (line-5-1
   (vector
    (note g5) (note b5) (note g5) (note d5) (note b4) ))
  (line-2-1 (vector (akkord b2 d3) (pause))) ))

(setq takt-47
 (sim
  (line-4-2
   (vector
    (note g5) (note g5) (note d5) (note b5) ))
  (line-2-1 (vector (akkord b2 d3) (pause))) ))

(setq takt-48
 (sim
  (line-5-1
   (vector
    (note e5) (note c5) (note e5) (note g5) (note c6) ))
  (line-2-1 (vector (akkord c3 g3) (akkord c3 e3))) ))
 
(setq takt-49 (pat-1 (vector e5 c5 g4)))

(setq takt-50
 (sim
  (line-4-2
   (vector
    (note c5) (note e5) (note c5) (note g4) ))
  (line-2-1 (vector (akkord e3 g3) (pause))) ))

(setq takt-51
 (sim
  (line-6
   (vector
    (note c5) (note g4) (note e5) (note c5) (note g5) (note e5) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-52
 (sim
  (line-6
   (vector
    (note d5) (note cs5) (note d5) (note f5) (note g5) (note b5) ))
  (line-2-1 (vector (note f3) (note g3))) ))

(setq takt-53
 (sim
  (line-4-2
   (vector
    (akkord c4 e4) (akkord c4 e4) (akkord d4 f4) (akkord e4 g4) ))
  (line-2-1 (vector (note c3) (pause))) ))

(setq takt-54 (pat-2 (vector c3 e4 c5)))

(setq takt-55
 (sim
  (line-3 (vector (note g5) (note b5) (note d5)))
  (line-2-1 (vector (akkord b2 d3) (pause))) ))

(setq takt-56
 (sim
  (line-4-1
   (vector (note d5) (note b4) (note g4) (pause)) )
  (line-2-1 (vector (akkord g2 g3) (note g3))) ))

(setq takt-57 (pat-1 (vector e5 c5 g4)))
(setq takt-58 (pat-1 (vector g5 e5 c5)))
(setq takt-59 (pat-1 (vector g5 c5 e5)))

(setq takt-60
 (sim
  (line-5-1
   (vector (note g5) (note f5) (note e5) (note d5) (note c5)) )
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-61
 (sim
  (line-4-2 (vector (note c5) (note e5) (note c5) (note g5)))
  (line-2-1 (vector (akkord e3 g3) (pause))) ))

(setq takt-62
 (sim
  (line-6
   (vector
    (note e5) (note c5) (note b4) (note g4) (note a4) (note fs4) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-63
 (sim
  (line-5-5
   (vector
    (note e5) (note c5) (note b4) (note c5) (note g4)))
  (line-2-1 (vector (note c3) (pause))) ))

(setq takt-64
 (sim
  (line-6
   (vector
    (note e5) (note g5) (note c6) (note g5) (note e5) (note c5) ))
  (line-2-1 (vector (akkord c3 g3) (akkord c3 g3))) ))

(setq takt-65
 (sim
  (line-4-1 (vector (note d5) (note a4) (note a5) (note fs5)))
  (line-2-1 (vector (akkord d3 fs3) (pause))) ))

(setq takt-66
 (sim
  (line-3 (vector (note fs5) (note a5) (note fs5)))
  (line-3 (vector (akkord d3 a3) (akkord d3 fs3) (akkord c3 d3))) ))

(setq takt-67
 (sim
  (line-6
   (vector
    (note c5) (note b4) (note c5) (note e5) (note g4) (note c5) ))
  (line-2-1 (vector (akkord c3 e3) (akkord e3 g3))) ))

(setq takt-68
 (sim
  (line-5-1
   (vector
    (note g5) (note b5) (note g5) (note d5) (note g5) ))
  (line-2-1 (vector (note b2) (pause))) ))

(setq takt-69
 (sim
  (line-3 (vector (note g5) (note e5) (note c5)))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-70
 (sim
  (line-5-1
   (vector
    (note fs5) (note a5) (note fs5) (note d5) (note fs5) ))
  (line-2-1 (vector (note d3) (note c3))) ))

(setq takt-71
 (sim
  (line-5-5
   (vector
    (note g5) (note b5) (note d6) (note b5) (note g5) ))
  (line-2-1 (vector (akkord b2 d3) (akkord b2 d3))) ))

(setq takt-72
 (sim
  (line-6
   (vector
    (note f5) (note e5) (note d5) (note c5) (note b4) (note d5) ))
  (line-2-1 (vector (note f3) (note g3))) ))


(setq takt-73 (pat-1 (vector g5 e5 c5)))

(setq takt-74
 (sim
  (line-6
   (vector
    (note c6) (note b5) (note c6) (note g5) (note e5) (note c5) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-75 (pat-2 (vector c3 d5 fs5)))

(setq takt-76
 (sim
  (line-6
   (vector
    (note c6) (note b5) (note c6) (note g5) (note e5) (note c5) ))
  (line-2-1 (vector (akkord c3 e3) (akkord c3 g3))) ))

(setq takt-77
 (sim
  (line-4-1 (vector (note g5) (note b5) (note g5) (note d5)))
  (line-2-1 (vector (akkord b2 d3) (akkord b2 g3))) ))

(setq takt-78
 (sim
  (line-3 (vector (note c5) (note c4) (pause)))
  (line-2-1 (vector (note c3) (note c2))) ))

(setq takt-79 (pat-3))

(setq takt-80
 (sim
  (line-3 (vector (note d5) (a4-mordent) (note fs5)))
  (line-2-1 (vector (note c3) (pause))) ))

(setq takt-82
 (sim
  (line-4-1
   (vector
    (note d5) (note b4) (note g4) (note g5) ))
  (line-2-1 (vector (akkord b2 g3) (akkord b2 d3))) ))

(setq takt-83 (pat-3))

(setq takt-84
 (sim
  (line-6
   (vector
    (note c5) (note g4) (note e5) (note c5) (note g5) (note e5) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-85
 (sim
  (line-3 (vector (note c5) (note e5) (note g4)))
  (line-2-1 (vector (akkord e3 g3) (pause))) ))

(setq takt-86
 (sim
  (line-4-2
   (vector (note d5) (note d5) (note g5) (note b5)) )
  (line-2-1 (vector (akkord b2 g3) (pause))) ))

(setq takt-87
 (sim
  (line-3 (vector (note g5) (note c5) (note e5)))
  (line-2-1 (vector (akkord c3 e3) (akkord c3 g3))) ))

(setq takt-88
 (sim
  (line-6
   (vector
    (note g5) (note d5) (note g5) (note b5) (note g5) (note d5) ))
  (line-2-1
   (vector (akkord b2 d3) (akkord b2 d3)) )))

(setq takt-89
 (sim
  (line-4-1 (vector (note f5) (note e5) (note d5) (note f5)))
  (line-4-1 (vector (note f5) (note e5) (note d5) (note f5))) ))

(setq takt-90
 (sim
  (line-6
   (vector
    (note fs5) (note a5) (note d6) (note a5) (note fs5) (note a5) ))
  (line-2-1 (vector (akkord c3 a3) (akkord c3 a3))) ))

(setq takt-92
 (sim
  (line-4-2 (vector (akkord b4 d5) (note g5) (note b5) (note d5)))
  (line-2-1 (vector (akkord b2 g3) (note g3))) ))

(setq takt-93 (pat-3))

(setq takt-95
 (sim
  (line-3 (vector (note g5) (note e5) (note c5)))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-96
 (sim
  (line-3 (vector (note e5) (note c5) (note g4)))
  (line-2-1 (vector (note c3) (pause))) ))

(setq takt-97
 (sim
  (line-6
   (vector
    (note g5) (note fs5) (note g5) (note d5) (note b4) (note g4) ))
  (line-2-1 (vector (akkord b2 d3) (akkord b2 g3))) ))

(setq takt-98 (pat-1 (vector c5 g4 e5)))

(setq takt-99
 (sim
  (line-3 (vector (note fs5) (note a5) (note d5)))
  (line-2-1 (vector (akkord c3 a3) (akkord c3 a3))) ))

(setq takt-101
 (sim
  (line-6
   (vector
    (note e5) (note d5) (note e5) (note g5) (note c6) (note g5) ))
  (line-2-1 (vector (akkord c3 g3) (akkord c3 e3))) ))

(setq takt-102
 (sim
  (line-4-1 (vector (note fs5) (note d5) (note a4) (note fs5)))
  (line-2-1 (vector (akkord c3 a3) (akkord c3 a3))) ))

(setq takt-103
 (sim
  (line-5-5 (vector (note c5) (note e5) (note c5) (note g4) (note e4)))
  (line-2-1 (vector (akkord e3 g3) (pause))) ))

(setq takt-104
 (sim
  (line-6
   (vector
    (note e5) (note d5) (note e5) (note g5) (note c6) (note g5) ))
  (line-2-1 (vector (note c3) (pause))) ))

(setq takt-105
 (sim
  (line-5-1
   (vector
    (note fs5) (note a5) (note fs5) (note d5) (note fs5) ))
  (line-2-1 (vector (note c3) (pause))) ))

(setq takt-106
 (sim
  (line-5-1
   (vector
    (note a4) (note d5) (note c5) (note b4) (note a4) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-108
 (sim
  (line-3 (vector (note e5) (note g5) (note c6)))
  (line-2-1 (vector (akkord c3 a3) (akkord c3 e3))) ))

(setq takt-109
 (sim
  (line-6
   (vector
    (note d5) (note f5) (note d5) (note f5) (note b4) (note d5) ))
  (line-2-1 (vector (akkord f3 a3) (akkord g3 d4))) ))

(setq takt-110
 (sim
  (line-6
   (vector
    (akkord b4 d5) (akkord a4 c5) (akkord a4 c5)
    (akkord g4 b4) (akkord g4 b4) (akkord fs4 a4) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-111 (pat-3))
(setq takt-112 (pat-1 (vector e5 c5 g4)))

(setq takt-113
 (sim
  (line-3 (vector (note f5) (note d5) (note b5)))
  (line-2-1 (vector (akkord g3 b3) (pause))) ))

(setq takt-114 (pat-2 (vector g3 b4 d5)))

(setq takt-115
 (sim
  (line-6
   (vector
    (note c5) (note g4) (note e5) (note c5) (note g5) (note e5) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-116
 (sim
  (line-6
   (vector
    (note d5) (note f5) (note a5) (note f5) (note d5) (note b4) ))
  (line-2-1 (vector (note f3) (note g3))) ))

(setq takt-117
 (sim
  (line-6
   (vector
    (note d5) (note a4) (note d5) (note fs5) (note a5) (note fs5) ))
  (line-2-1 (vector (akkord d3 fs3) (pause))) ))

(setq takt-118
 (sim
  (line-6
   (vector
    (note e5) (note a5) (note g5) (note b5) (note fs5) (note a5) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-119
 (sim
  (line-6
   (vector
    (note e5) (note c5) (note g5) (note e5) (note c6) (note g5) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-120
 (sim
  (line-5-1
   (vector
    (note d6) (note a5) (note fs5) (note d5) (note a4) ))
  (line-2-1 (vector (akkord d3 fs3) (akkord c3 fs3))) ))

(setq takt-121
 (sim
  (line-4-2 (vector (note g5) (note b5) (note g5) (note d5)))
  (line-2-1 (vector (akkord b2 g3) (pause))) ))

(setq takt-122
 (sim
  (line-5-5
   (vector
    (note g5) (note fs5) (note g5) (note b5) (note d5) ))
  (line-3 (vector (akkord b2 d3) (akkord b2 d3) (akkord b2 d3))) ))

(setq takt-124 (pat-2 (vector c3 e4 c5)))

(setq takt-125
 (sim
  (line-5-5
   (vector
    (note g5) (note e5) (note d5) (note b4) (note g4) ))
  (line-3 (vector (note g3) (note g2) (pause))) ))

(setq takt-126
 (sim
  (line-6
   (vector
    (note c5) (note g4) (note c5) (note e5) (note g5) (akkord c5 e5) ))
  (line-3-s (vector (note e3) (note e3) (note c3))) ))

(setq takt-128
 (sim
  (line-3 (vector (note b4) (note d5) (note g5)))
  (line-2-1 (vector (note g2) (pause))) ))

(setq takt-129
 (sim
  (line-5-5
   (vector
    (note a5) (note g5) (note fs5) (note g5) (note d5) ))
  (line-3 (vector (akkord b2 d3) (akkord b2 d3) (akkord b2 g3))) ))

(setq takt-130 (pat-2 (vector c3 e4 c5)))
(setq takt-131 (pat-3))

(setq takt-132
 (sim
  (line-4-2
   (vector (akkord c5 e5) (akkord b4 d4) (akkord g4 b4) (note g4) ))
  (line-3 (vector (note g3) (note g2) (pause))) ))

(setq takt-133
 (sim
  (line-5-1
   (vector
    (note d5) (note g5) (note d5) (note b4) (note d5) ))
  (line-2-1 (vector (akkord b2 g3) (pause))) ))

(setq takt-134
 (sim
  (line-6
   (vector
    (note a4) (note e5) (akkord b4 d5) (akkord a4 c5)
    (akkord g4 b4) (akkord fs4 b4) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-135
 (sim
  (line-4-2
   (vector
    (note fs5) (note fs5) (note d5) (note a5) ))
  (line-3 (vector (akkord c3 d3) (akkord c3 d3) (akkord c3 d3))) ))

(setq takt-136
 (sim
  (line-6
   (vector
    (note c6) (note b5) (note c6) (note g5) (note e5) (note c5) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-137 (pat-1 (vector c5 g4 e5)))

(setq takt-138
 (sim
  (line-2-2 (vector (akkord a4 d5 fs5) (f5-triller)))
  (line-6
   (vector
    (note d2) (note d3) (note cs3) (note d3) (note c3) (note d3) ))))

(setq takt-139
 (sim
  (line-5-5
   (vector
    (note g5) (note b5) (note g5) (note b5) (note d5) ))
  (line-2-1 (vector (note b2) (pause))) ))

(setq takt-140
 (sim
  (line-4-2 (vector (note a4) (note a4) (note d5) (note fs5)))
  (line-3 (vector (akkord c3 fs3) (akkord c3 fs3) (akkord c3 fs3))) ))

(setq takt-141
 (sim
  (line-6
   (vector (note d5) (note e5) (note f5) (note d5) (note c5) (note b4)) )
  (line-2-1 (vector (akkord b2 g3) (note g2))) ))

(setq takt-142
 (sim
  (line-3 (vector (note c5) (note g4) (note e5)))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-143
 (sim
  (line-4-2 (vector (note g5) (note d5) (note b4) (note g4)))
  (line-2-1 (vector (akkord b2 d3) (akkord b2 d3))) ))

(setq takt-144 (pat-1 (vector g5 c5 e5)))

(setq takt-145
 (sim
  (line-6
   (vector
    (note d5) (note f5) (note a4) (note d5) (note b4) (note d5) ))
  (line-2-1 (vector (note f3) (note g3))) ))

(setq takt-146
 (sim
  (line-3 (vector (akkord fs4 d5) (akkord d5 fs5) (akkord fs5 a5)))
  (line-3 (vector (note c3) (note c3) (note c3))) ))

(setq takt-147
 (sim
  (line-6
   (vector
    (note e5) (note c6) (note b5) (note g5) (note a5) (note fs5) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-148
 (sim
  (line-6
   (vector
    (note c6) (note b5) (note c6) (note g5) (note e5) (note c5) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-149
 (sim
  (line-4-1 (vector (note f5) (note d5) (note a4) (note b4)))
  (line-2-1 (vector (note f3) (note g3))) ))

(setq takt-150
 (sim
  (line-2-2 (vector (akkord g4 c5 e5) (e5-triller)))
  (line-6
   (vector
    (note c3) (note b2) (note c3) (note d3) (note e3) (note fs3) ))))

(setq takt-151 (pat-3))

(setq takt-152
 (sim
  (line-5-1
   (vector
    (note g5) (note f5) (note e5) (note d5) (note c5) ))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-153
 (sim
  (line-6
   (vector
    (note d5) (note a4) (note fs5) (note d5) (note a5) (note fs5) ))
  (line-2-1 (vector (note c3) (pause))) ))

(setq takt-154
 (sim
  (line-6
   (vector
    (note d5) (note cs5) (note d5) (note fs5) (note a5) (note fs5) ))
  (line-2-1 (vector (note c3) (pause))) ))

(setq takt-155
 (sim
  (line-6
   (vector
    (note g5) (note b5) (note g5) (note d5) (note b4) (note g4) ))
  (line-2-1 (vector (akkord b2 d3) (pause))) ))

(setq takt-156
 (sim
  (line-5-5
   (vector
    (note c5) (note g4) (note e5) (note c5) (note g5) ))
  (line-2-1 (vector (akkord e3 g3) (pause))) ))

(setq takt-157
 (sim
  (line-6
   (vector
    (note e5) (note d5) (note e5) (note g5) (note c6) (note g5) ))
  (line-2-1 (vector (note c3) (pause))) ))

(setq takt-158
 (sim
  (line-5-1
   (vector
    (note b4) (note d5) (note b4) (note a4) (note g4) ))
  (line-2-1 (vector (note g2) (pause))) ))

(setq takt-159
 (sim
  (line-6
   (vector
    (note e5) (note g5) (note d5) (note c5) (note b4) (note a4) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-160
 (sim
  (line-6
   (vector
    (note c5) (note b4) (note c5) (note e5) (note g4) (note c5) ))
  (line-2-1 (vector (akkord c3 e3) (akkord c3 e3))) ))
 
(setq takt-161 (pat-2 (vector c3 fs4 d5)))

(setq takt-162
 (sim
  (line-6
   (vector
    (note e5) (note d5) (note e5) (note g5) (note c6) (note g5) ))
  (line-2-1 (vector (akkord c3 g3) (akkord c3 e3))) ))

(setq takt-163
 (sim
  (line-6
   (vector
    (note g5) (note fs5) (note g5) (note d5) (note b4) (note g4) ))
  (line-2-1 (vector (akkord b2 d3) (pause))) ))

(setq takt-164
 (sim
  (line-2-2 (vector (note d5) (note g4)))
  (line-6
   (vector
    (note g3) (note fs3) (note g3) (note d3) (note b2) (note g2) ))))

(setq takt-165
 (sim
  (line-3 (vector (note d5) (note b4) (note g4)))
  (line-2-1 (vector (note b2) (pause))) ))

(setq takt-166
 (sim
  (line-5-5
   (vector
    (note d5) (note b5) (note g5) (note d5) (note b4) ))
  (line-2-1 (vector (akkord g3 b3) (pause))) ))

(setq takt-167
 (sim
  (line-4-2 (vector (note c5) (note c5) (note d5) (note e5)))
  (line-2-1 (vector (akkord c3 e3) (pause))) ))

(setq takt-168
 (sim
  (line-5-1 (vector (note g5) (note f5) (note e5) (note d5) (note c5)))
  (line-2-1 (vector (akkord c3 e3) (akkord e3 g3))) ))

(setq takt-169
 (sim
  (line-6
   (vector
    (note e5) (note g5) (note d5) (note g5) (note a4) (note fs5) ))
  (line-3 (vector (note c3) (note d3) (note d2))) ))

(setq takt-170 (pat-3))

(setq takt-171
 (sim
  (line-6
   (vector
    (note b4) (note c5) (note d5) (note e5) (note f5) (note d5) ))
  (line-2-1 (vector (akkord g2 g3) (akkord b2 g3))) ))

(setq takt-172 (pat-3))

(setq takt-173
 (sim
  (line-5-3
   (vector
    (note f5) (note a5) (note a4) (note b4) (note d5) ))
  (line-2-1 (vector (note f3) (note g3))) ))

(setq takt-174 (pat-1 (vector g4 c5 e5)))

(setq takt-175
 (sim
  (line-5-5
   (vector
    (note e5) (note c5) (note b4) (note d5) (note g5) ))
  (line-3 (vector (note g3) (note g2) (pause))) ))

(setq takt-176
 (sim
  (line-6
   (vector
    (note a5) (note g5) (note b5) (note g5) (note d5) (note g5) ))
  (line-2-1 (vector (akkord b2 d3) (akkord b2 d3))) ))

(setq takt-table
 (vector
  (vector
   takt-96  takt-22  takt-141 takt-41  takt-105
   takt-122 takt-11  takt-70  takt-121 takt-26
   takt-9   takt-112 takt-49  takt-109 takt-14 )
  (vector
   takt-32  takt-6   takt-128 takt-63  takt-146
   takt-46  takt-134 takt-117 takt-39  takt-126
   takt-56  takt-174 takt-18  takt-116 takt-83 )
  (vector
   takt-69  takt-95  takt-158 takt-13  takt-153
   takt-55  takt-110 takt-66  takt-139 takt-15
   takt-132 takt-73  takt-58  takt-145 takt-79 )
  (vector
   takt-40  takt-17  takt-114 takt-85  takt-161
   takt-2   takt-159 takt-90  takt-176 takt-7
   takt-34  takt-67  takt-160 takt-52  takt-170 )
  (vector
   takt-148 takt-74  takt-163 takt-45  takt-80
   takt-97  takt-36  takt-25  takt-143 takt-64
   takt-125 takt-76  takt-136 takt-1   takt-93 )
  (vector
   takt-104 takt-157 takt-27  takt-167 takt-154
   takt-68  takt-118 takt-138 takt-71  takt-150
   takt-29  takt-101 takt-162 takt-23  takt-151 )
  (vector
   takt-152 takt-60  takt-171 takt-53  takt-99
   takt-133 takt-21  takt-16  takt-155 takt-57
   takt-175 takt-43  takt-168 takt-89  takt-172 )
  (vector
   takt-119 takt-84  takt-114 takt-50  takt-140
   takt-86  takt-169 takt-120 takt-88  takt-48
   takt-166 takt-51  takt-115 takt-72  takt-111 )
  (vector
   takt-98  takt-142 takt-42  takt-156 takt-75
   takt-129 takt-62  takt-65  takt-77  takt-19
   takt-82  takt-137 takt-38  takt-149 takt-8 )
  (vector
   takt-3   takt-87  takt-165 takt-61  takt-135
   takt-47  takt-147 takt-102 takt-4   takt-31
   takt-164 takt-144 takt-59  takt-173 takt-78 )
  (vector
   takt-54  takt-130 takt-10  takt-103 takt-28
   takt-37  takt-106 takt-35  takt-20  takt-108
   takt-92  takt-12  takt-124 takt-44  takt-131 )))

(defun sel-takt (dices taktno)
 (aref (aref takt-table (- (aref dices taktno) 2)) taktno) )

(defun compute-wwalzer (dices)
 (seq
  (sel-takt dices 0)
  (sel-takt dices 1)
  (sel-takt dices 2)
  (sel-takt dices 3)
  (sel-takt dices 4)
  (sel-takt dices 5)
  (sel-takt dices 6)
  takt-no8
  (sel-takt dices 0)
  (sel-takt dices 1)
  (sel-takt dices 2)
  (sel-takt dices 3)
  (sel-takt dices 4)
  (sel-takt dices 5)
  (sel-takt dices 6)
  takt-no16
  (sel-takt dices 7)
  (sel-takt dices 8)
  (sel-takt dices 9)
  (sel-takt dices 10)
  (sel-takt dices 11)
  (sel-takt dices 12)
  (sel-takt dices 13)
  (sel-takt dices 14) ))

(defun dice-wwalzer () (compute-wwalzer (dice15)))

(play (dice-wwalzer))
(exit)

