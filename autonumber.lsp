;Copyright (c) 2020 eightfeet <bajiao000000@gmail.com>
     ;写给星云，祝工作开心

     ;块级矩阵(X0-Xn Yn-Y0)自动编号插件

     ; @parame startnumber 编号起始数字默认为1
     ; @parame blocktimes 单块编号次数默认为1
     ; @parame periodic 编号进制周期，按周期编号时使用
     ; @parame startperiodic 编号进制起始周期
     ; @parame startperiodicnumber 编号进制起始周期的起始编号
     ; @parame prefix 编号前缀，用于区分编号区间


     ;从左到右，从上到下排序
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

     ;选集到list
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

     ;list到选集
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

     ;修改选集
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
  ;起始数字
  (setq startnumber (getint "\n请输入编号起始数字(1)："))
  (if (= startnumber nil) 
    (setq startnumber 1)
  )
  ;单块编号次数
  (setq blocktimes (getint "\n请输入单个组串块编号次数(1)："))
  (if (= blocktimes nil) 
    (setq blocktimes 1)
  )
  ;编号周期
  (setq periodic (getint "\n按周期编号时,请输入周期包含组串块个数(组串个数>1)："))
  (if (= periodic 1) (progn
     (alert "您的周期设置必须大于一个组串块，请重设或者按Enter跳过")
     (setq periodic nil)
     (setq periodic (getint "\n按周期编号时,请输入周期包含组串块个数(组串个数>1)："))
  ))
  (if (= periodic 0) (progn
     (alert "您的周期设置必须大于一个组串块，请重设或者按Enter跳过")
     (setq periodic nil)
     (setq periodic (getint "\n按周期编号时,请输入周期包含组串块个数(组串个数>1)："))
  ))
  ;有编号周期时
  (if (/= periodic nil) 
    (progn 
      ;设置起始周期
      (setq startperiodic (getint "\n请输入周期的起始编号(1)："))
      ;未设置起始周期默认为1
      (if (= startperiodic nil) 
        (setq startperiodic 1)
      )
      ;设置起始周期起始编号为起始数字
      (setq startperiodicnumber startnumber)
    )
  ) ;end if

  ;编号前缀
  (setq prefix (getstring "\n请输入分区前缀："))
  (if (= prefix nil) 
    (setq prefix "")
  )
  ;选择内容
  (setq ssdata (ssget))
  ;获取所选内容长度
  (setq sslen (sslength ssdata))

  ;标注数
  (setq itemNum 0)
  ;创建计数器
  (setq counter 0)
  ;板数据
  (setq ssdata (SortSelectionSetByXYZ ssdata))
  ;进制值

  (if (/= periodic nil) 
    (setq hex (* periodic blocktimes))
  ) ;end if
  (if (= periodic nil) 
    (setq hex blocktimes)
  ) ;end if

  

  ;遍历所选数据while-1
  (while (<= counter sslen)
    ;group-1
    (progn
      ;初始化一个空字符来存结果
      (setq result "")
      ;设置单块次数
      (setq times 1)

      ;遍历单块次数数据while-2
      (while (<= times blocktimes)
        ;group-2
        (progn

	   ;分割字符
	   (setq divide "")
	   (if (> blocktimes 1) (setq divide "/"))
	   (if (= times blocktimes) (setq divide ""))
	  
	   ;判断非周期布板
	   (if (= periodic nil)
	     ;group-3
	     (progn
	       
	       (setq itemNum (1+ itemNum))

	       ;数字小于10字符串补0
	       (setq strstartnumber (rtos startnumber 2 0))
	       (if (< startnumber 10) (setq strstartnumber (strcat "0" strstartnumber)))

	       (setq result (strcat
			      result
			      prefix
			      strstartnumber
			      divide
	           ))
             );end group-3
	   ) ;end if 判断非周期布板


	   ;判断周期布板
	   (if (/= periodic nil)
	     ;group-4
	     (progn
	       (setq hex (* periodic blocktimes))
	       ; 末节数据
	       (setq itemNum (rem startnumber hex))
	       (if (= itemNum 0)(setq itemNum hex))

	       ; 增加周期
	       (if(> startnumber 1)
		  (if(= itemNum 1)
		    (progn
	   	      (setq startperiodic (+ startperiodic 1))
		    )
	 	  )
	       )

	       ;数字小于10字符串补0
	       (setq strItemNum (rtos itemNum 2 0))
	       (if (< itemNum 10) (setq strItemNum (strcat "0" strItemNum)))

	       ;数字小于10字符串补0
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
	   ) ;end if 判断周期布板



	   (setq startnumber (1+ startnumber))
	   (setq times (1+ times))

	  
	); end group-2
      ); end while-2

      
      ;======================================================================
      ;获取对象属性
      (setq obj  (vlax-ename->vla-object (ssname ssdata counter))
            ins  (vlax-get obj 'insertionpoint)
      )
      
      ;坐标参数
      (setq position (strcat
                       (rtos (car ins))
                       ","
                       (rtos (cadr ins))
                     )
      )
      
      ;设置插入文字内容为
      (command "text" position fontsize "" result)
      ;=====================================================================
      
      (setq counter (1+ counter))
      (princ)
    );end group-1
  ) ;end while-1
  (princ "...done")
  (princ)
)     ; end