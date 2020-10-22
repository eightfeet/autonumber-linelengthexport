;Copyright (c) 2020 eightfeet <bajiao000000@gmail.com>
     ;д�����ƣ�ף��������

     ;�鼶����(X0-Xn Yn-Y0)�Զ���Ų��

     ; @parame startnumber �����ʼ����Ĭ��Ϊ1
     ; @parame blocktimes �����Ŵ���Ĭ��Ϊ1
     ; @parame periodic ��Ž������ڣ������ڱ��ʱʹ��
     ; @parame startperiodic ��Ž�����ʼ����
     ; @parame startperiodicnumber ��Ž�����ʼ���ڵ���ʼ���
     ; @parame prefix ���ǰ׺���������ֱ������


     ;�����ң����ϵ�������
(defun sortListofSublistsbyItemX (lstOfSublists) 
  (vl-sort lstOfSublists 
           '(lambda (el1 el2) 
              (< (atof (rtos (cadr el1) 2 2)) 
                 (atof (rtos (cadr el2) 2 2))
              )
            )
  )
  (vl-sort lstOfSublists 
           '(lambda (el1 el2) 
              (> (atof (rtos (caddr el1) 2 2)) 
                 (atof (rtos (caddr el2) 2 2))
              )
            )
  )
)

     ;ѡ����list
(defun SelectionSetToList (ssSelections / intCount lstReturn) 
  (if 
    (and ssSelections 
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
(defun SortSelectionSetByXYZ (ssSelections / lstOfSelections lstOfSublists 
                              lstSelections
                             ) 
  (if 
    (and 
      (setq lstSelections (SelectionSetToList ssSelections))
      (setq lstOfSublists (mapcar 
                            '(lambda (X) 
                               (cons X (cdr (assoc 10 (entget X))))
                             )
                            lstSelections
                          )
      )
      (setq lstOfSublists (sortlistofsublistsbyitemX lstOfSublists))
      (setq ssSelections (listtoselectionset (mapcar 'car lstOfSublists)))
    )
    ssSelections
  )
)


     ; Program to change the color of text to RED whose content has a special character
(defun c:an (/ startnumber blocktimes periodic startperiodic 
             startperiodicnumber prefix
            ) 

  (setq fontsize 2.5)
  ;��ʼ����
  (setq startnumber (getint "\n����������ʼ����(1)��"))
  (if (= startnumber nil) 
    (setq startnumber 1)
  )
  ;�����Ŵ���
  (setq blocktimes (getint "\n�����뵥���鴮���Ŵ���(1)��"))
  (if (= blocktimes nil) 
    (setq blocktimes 1)
  )
  ;�������
  (setq periodic (getint "\n�����ڱ��ʱ,���������ڰ����鴮�����(�鴮����>1)��"))
  (if (= periodic 1) (progn
     (alert "�����������ñ������һ���鴮�飬��������߰�Enter����")
     (setq periodic nil)
     (setq periodic (getint "\n�����ڱ��ʱ,���������ڰ����鴮�����(�鴮����>1)��"))
  ))
  (if (= periodic 0) (progn
     (alert "�����������ñ������һ���鴮�飬��������߰�Enter����")
     (setq periodic nil)
     (setq periodic (getint "\n�����ڱ��ʱ,���������ڰ����鴮�����(�鴮����>1)��"))
  ))
  ;�б������ʱ
  (if (/= periodic nil) 
    (progn 
      ;������ʼ����
      (setq startperiodic (getint "\n���������ڵ���ʼ���(1)��"))
      ;δ������ʼ����Ĭ��Ϊ1
      (if (= startperiodic nil) 
        (setq startperiodic 1)
      )
      ;������ʼ������ʼ���Ϊ��ʼ����
      (setq startperiodicnumber startnumber)
    )
  ) ;end if

  ;���ǰ׺
  (setq prefix (getstring "\n���������ǰ׺��"))
  (if (= prefix nil) 
    (setq prefix "")
  )
  ;ѡ������
  (setq ssdata (ssget))
  ;��ȡ��ѡ���ݳ���
  (setq sslen (sslength ssdata))

  ;��ע��
  (setq itemNum 0)
  ;����������
  (setq counter 0)
  ;������
  (setq ssdata (SortSelectionSetByXYZ ssdata))
  ;����ֵ

  (if (/= periodic nil) 
    (setq hex (* periodic blocktimes))
  ) ;end if
  (if (= periodic nil) 
    (setq hex blocktimes)
  ) ;end if

  

  ;������ѡ����while-1
  (while (<= counter sslen)
    ;group-1
    (progn
      ;��ʼ��һ�����ַ�������
      (setq result "")
      ;���õ������
      (setq times 1)

      ;���������������while-2
      (while (<= times blocktimes)
        ;group-2
        (progn

	   ;�ָ��ַ�
	   (setq divide "")
	   (if (> blocktimes 1) (setq divide "/"))
	   (if (= times blocktimes) (setq divide ""))
	  
	   ;�жϷ����ڲ���
	   (if (= periodic nil)
	     ;group-3
	     (progn
	       
	       (setq itemNum (1+ itemNum))

	       ;����С��10�ַ�����0
	       (setq strstartnumber (rtos startnumber 2 0))
	       (if (< startnumber 10) (setq strstartnumber (strcat "0" strstartnumber)))

	       (setq result (strcat
			      result
			      prefix
			      strstartnumber
			      divide
	           ))
             );end group-3
	   ) ;end if �жϷ����ڲ���


	   ;�ж����ڲ���
	   (if (/= periodic nil)
	     ;group-4
	     (progn
	       (setq hex (* periodic blocktimes))
	       ; ĩ������
	       (setq itemNum (rem startnumber hex))
	       (if (= itemNum 0)(setq itemNum hex))

	       ; ��������
	       (if(> startnumber 1)
		  (if(= itemNum 1)
		    (progn
	   	      (setq startperiodic (+ startperiodic 1))
		    )
	 	  )
	       )

	       ;����С��10�ַ�����0
	       (setq strItemNum (rtos itemNum 2 0))
	       (if (< itemNum 10) (setq strItemNum (strcat "0" strItemNum)))

	       ;����С��10�ַ�����0
	       (setq atrstartperiodic (rtos startperiodic 2 0))
	       (if (< startperiodic 10) (setq atrstartperiodic (strcat "0" atrstartperiodic)))
	       ;times blocktimes
	       (setq result (strcat
			     result
			     prefix
			     atrstartperiodic
			     strItemNum
			     divide
	           ))
             );end group-4
	   ) ;end if �ж����ڲ���



	   (setq startnumber (1+ startnumber))
	   (setq times (1+ times))

	  
	); end group-2
      ); end while-2

      
      ;======================================================================
      ;��ȡ��������
      (setq obj  (vlax-ename->vla-object (ssname ssdata counter))
            ins  (vlax-get obj 'insertionpoint)
      )
      
      ;�������
      (setq position (strcat
                       (rtos (car ins))
                       ","
                       (rtos (cadr ins))
                     )
      )
      
      ;���ò�����������Ϊ
      (command "text" position fontsize "" result)
      ;=====================================================================
      
      (setq counter (1+ counter))
      (princ)
    );end group-1
  ) ;end while-1
  (princ "...done")
  (princ)
)     ; end