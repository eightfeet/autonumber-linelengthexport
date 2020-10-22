;Copyright (c) 2020 eightfeet <bajiao000000@gmail.com>
;д�����ƣ�ף��������

;����ÿ������߳����嵥��excel��

;�������Y������
(defun sortListofSublistsbyY (lstOfSublists)
    (vl-sort lstOfSublists '(lambda (el1 el2) (< (cadr el1)  (cadr el2))))
    ;(vl-sort lstOfSublists '(lambda (el1 el2) (< (cadr el1)  (cadr el2))))
)

;ѡ����list
(defun SelectionSetToList (ssSelections / intCount lstReturn)
(if (and ssSelections
          (= (type ssSelections) 'PICKSET)
     )
  (repeat (setq intCount (sslength ssSelections))
   (setq intCount  (1- intCount)
         lstReturn (cons (ssname ssSelections intCount) lstReturn)
   )
  )
)
(reverse lstReturn)
)

;list��ѡ��
(defun ListToSelectionSet (lstOfEntities / ssReturn)
(if lstOfEntities      
  (foreach entItem lstOfEntities
   (if (= (type entItem) 'ENAME)
    (if ssReturn
     (setq ssReturn (ssadd entItem ssReturn))
     (setq ssReturn (ssadd entItem))
    )
   )
  )
)
ssReturn
)

;�޸�ѡ��
(defun SortSelectionSetByXYZ (ssSelections /  lstOfSelections lstOfSublists lstSelections)
(if
  (and
   (setq lstSelections (SelectionSetToList ssSelections))
   (setq lstOfSublists (mapcar '(lambda (X)(cons X (cdr (assoc 10 (entget X))))) lstSelections))
   (setq lstOfSublists (sortlistofsublistsbyitemX lstOfSublists))
   
   (setq ssSelections  (listtoselectionset (mapcar 'car lstOfSublists)))
  )
  ssSelections
)
)


(defun c:LLE (/ s i e l fn)
  (if (and(setq s (ssget '((0 . "LWPOLYLINE"))))
	  (setq fn (getfiled "Create Output File" "" "csv" 1)))
    (progn
      ;(setq s (_SortSSByXValue s))
      ;��y������
      ;(setq s (SortSelectionSetByXYZ s))

      ;��֯�������
      (setq i (sslength s))
      (while (setq e(ssname s (setq i (1- i))))
	;ȡ�ö����
	(setq myPline (vlax-ename->vla-object e))
	;��ȡ����
	(setq myPlineLength (vla-get-length myPline))
	;��ȡ��ʼ�˵�
	(setq myPlineStart (vlax-curve-getStartPoint myPline))
	;��ȡ�����˵�
	(setq myPlineEnd (vlax-curve-getEndPoint myPline));
	(setq value (nth 0 (cdr myPlineStart)))
	(if(> (nth 0 (cdr myPlineEnd)) value) (setq value (nth 0 (cdr myPlineEnd))) )
	(setq l (cons (list myPlineLength value) l))
	(ssdel e s)
      )

      ; ����
      (setq l (sortListofSublistsbyY l))

      (setq dbdb nil)
      ;������֯�������
      (foreach entItem l
	(progn
	  (setq resel (rtos (nth 0 entItem)))
	  (setq dbdb (cons (list resel) dbdb))
	)
      )
    )
  )
 
  ;�������
  (if (LM:WriteCSV dbdb fn)
                (startapp "explorer" fn)
            )
  (princ)
)


;; Write CSV  -  Lee Mac
;; Writes a matrix list of cell values to a CSV file.
;; lst - [lst] list of lists, sublist is row of cell values
;; csv - [str] filename of CSV file to write
;; Returns T if successful, else nil
 
(defun LM:writecsv ( lst csv / des sep )
    (if (setq des (open csv "w"))
        (progn
            (setq sep (cond ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) (",")))
            (foreach row lst (write-line (LM:lst->csv row sep) des))
            (close des)
            t
        )
    )
)
 
;; List -> CSV  -  Lee Mac
;; Concatenates a row of cell values to be written to a CSV file.
;; lst - [lst] list containing row of CSV cell values
;; sep - [str] CSV separator token
 
(defun LM:lst->csv ( lst sep )
    (if (cdr lst)
        (strcat
	  (LM:csv-addquotes
	    (car lst)
	    sep
	  )
	   sep
	   (LM:lst->csv (cdr lst) sep)
	)
        (LM:csv-addquotes (car lst) sep)
    )
)
 
(defun LM:csv-addquotes ( str sep / pos )
    (cond
        (   (wcmatch str (strcat "*[`" sep "\"]*"))
            (setq pos 0)    
            (while (setq pos (vl-string-position 34 str pos))
                (setq str (vl-string-subst "\"\"" "\"" str pos)
                      pos (+ pos 2)
                )
            )
            (strcat "\"" str "\"")
        )
        (   str   )
    )
)